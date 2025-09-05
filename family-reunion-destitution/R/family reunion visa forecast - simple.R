library(tidyverse)
# remotes::install_github("humaniverse/asylum")
library(asylum)
# install.packages(c("forecast", "tseries"))
library(forecast)
library(tseries)

decisions <- fetch_decisions()
fr <- fetch_reunion()

refugee_status <- 
  decisions |> 
  filter(
    `Applicant type` == "Main applicant" &
      str_detect(`Case outcome group`, "Grant") &
      UASC == "Non-UASC"
  ) |> 
  group_by(Date, Nationality) |>
  summarise(`Status granted` = sum(Decisions)) |>
  ungroup()

visas <- 
  fr |> 
  group_by(Date, Nationality) |>
  summarise(Visas = sum(`Visas granted`)) |>
  ungroup()

visas |> 
  left_join(refugee_status) |> 
  filter(Nationality %in% nationalities) |>
  
  pivot_longer(cols = c(`Status granted`, Visas), 
               names_to = "Type", values_to = "Count") |>
  
  ggplot(aes(x = Date, y = Count, color = Type, fill = Type)) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~Nationality)

# ---- Overall forecast (not nationality specific) ----
visas_total <- 
  visas |> 
  group_by(Date) |>
  summarise(Visas = sum(Visas)) |>
  ungroup()

grants_total <- 
  refugee_status |> 
  filter(Date >= min(visas$Date)) |>
  group_by(Date) |>
  summarise(Grants = sum(`Status granted`)) |>
  ungroup()

visas_total |> 
  left_join(grants_total) |> 
  pivot_longer(cols = c(Grants, Visas), 
               names_to = "Type", values_to = "Count") |>
  
  ggplot(aes(x = Date, y = Count, color = Type, fill = Type)) +
  geom_line(show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma)

ts_refugee <- ts(grants_total$Grants, start = c(2010, 1), frequency = 4)
ts_family  <- ts(visas_total$Visas,   start = c(2010, 1), frequency = 4)

# 2. Plot the time series
autoplot(ts_refugee, series = "Refugee Grants") +
  autolayer(ts_family, series = "Family Visas") +
  ggtitle("Quarterly Series: Grants vs. Family Visas") +
  xlab("Year") + ylab("Count") +
  scale_colour_manual(values = c("Refugee Grants" = "steelblue", "Family Visas" = "firebrick")) +
  theme_minimal()

# 3. Compute cross-correlation to identify lag
# Using stats::ccf (with plot = FALSE) to find the lag that maximizes correlation
ccf_res <- ccf(ts_refugee, ts_family, lag.max = 8, plot = TRUE, na.action = na.omit)
# ccf_res$lag is in units of quarters; ccf_res$acf gives correlation at each lag.

# Find the lag (in quarters) with maximum correlation (positive side only)
# Note: ccf() returns correlations at negative and positive lags; 
# we want Family lagging Refugee, so we look at positive lags in the ccf output.
lags <- ccf_res$lag[,1,1]
cor_vals <- ccf_res$acf[,1,1]
# Filter for lags >= 0
pos_idx <- which(lags >= 0)
lags_pos <- lags[pos_idx]
corrs_pos <- cor_vals[pos_idx]
best_lag <- lags_pos[which.max(corrs_pos)]
cat("Optimal lag (highest corr):", best_lag, "quarter(s)\n\n")

best_lag <- 1

# 4. Prepare lagged exogenous series
# Create a lagged version of refugee_grants by 'best_lag' quarters
refugee_lagged <- stats::lag(ts_refugee, -best_lag) 
# Convert to a vector aligned with family_visas
refugee_lagged_vec <- as.numeric(refugee_lagged)
# Build a combined ts starting at 2019 Q1
# Drop the first 'best_lag' quarters where refugee_lagged is NA
start_year <- 2010 + (best_lag / 4)
ts_start <- c(2010 + (best_lag) %/% 4, (best_lag %% 4) + 1)

fam_trim     <- window(ts_family, start = ts_start)
refugee_trim <- window(ts_refugee, start = ts_start)
refugee_xreg <- window(refugee_lagged, start = ts_start)

# 5. Split into train and test (last 4 quarters as test)
n_total <- length(fam_trim)
n_test  <- 4
n_train <- n_total - n_test

y_train    <- window(fam_trim, end = c(2010 + (n_train - 1) %/% 4, ((n_train - 1) %% 4) + 1))
x_train    <- window(refugee_xreg, end = c(2010 + (n_train - 1) %/% 4, ((n_train - 1) %% 4) + 1))

y_test     <- window(fam_trim, start = c(2010 + n_train %/% 4, (n_train %% 4) + 1))
x_test     <- window(refugee_xreg, start = c(2010 + n_train %/% 4, (n_train %% 4) + 1))

# 6. Fit ARIMAX on the training data
# Here we choose ARIMA(1,0,1) for illustration; you can also use auto.arima(...)
model_arimax <- Arima(
  y_train,
  order = c(1, 0, 1),
  xreg  = as.numeric(x_train),
  include.constant = TRUE
)
summary(model_arimax)

# 7. Forecast for the test period
fc <- forecast(
  model_arimax,
  h    = n_test,
  xreg = as.numeric(x_test)
)

# Plot training, test, and forecast
autoplot(y_train) +
  autolayer(y_test, series = "Actual Test") +
  autolayer(fc$mean, series = "Forecasted") +
  autolayer(fc$lower[,2], series = "95% CI Lower", linetype = "dashed") +
  autolayer(fc$upper[,2], series = "95% CI Upper", linetype = "dashed") +
  ggtitle("ARIMAX Forecast vs Actual (Test Period)") +
  xlab("Year") + ylab("Family Visas") +
  scale_colour_manual(values = c("Data" = "black", "Actual Test" = "firebrick", "Forecasted" = "darkgreen",
                                 "95% CI Lower" = "gray50", "95% CI Upper" = "gray50")) +
  theme_minimal()

# Display numerical comparison
cat("Actual vs Forecast for Test Period:\n")
print(data.frame(
  Quarter        = time(y_test),
  Actual_Visas   = as.numeric(y_test),
  Forecast_Visas = as.numeric(fc$mean),
  `Lower_95%`    = as.numeric(fc$lower[,2]),
  `Upper_95%`    = as.numeric(fc$upper[,2])
))

# 8. Extend forecast into future (next 4 quarters)
# We need future values of the exogenous variable (lagged refugee grants).
# For demonstration, assume future refugee_grants remain equal to the last observed value.
last_refugee <- tail(ts_refugee, 1)
future_ref_lagged <- ts(
  rep(last_refugee, 4),
  start = end(ts_family) + c(0, 1/4),
  frequency = 4
)

fc_future <- forecast(
  model_arimax,
  h    = 4,
  xreg = as.numeric(future_ref_lagged)
)

autoplot(fc_future) +
  ggtitle("Forecasted Family Visas (Next 4 Quarters)") +
  xlab("Year") + ylab("Family Visas") +
  theme_minimal()

cat("Future Forecasted Family Visas (Next 4 Quarters):\n")
print(data.frame(
  Quarter        = time(fc_future$mean),
  Forecast_Visas = as.numeric(fc_future$mean),
  `Lower_95%`    = as.numeric(fc_future$lower[,2]),
  `Upper_95%`    = as.numeric(fc_future$upper[,2])
))
