# WIP
#--- init ----------------------------------------------------------------------

library(tidyverse)
# remotes::install_github("humaniverse/asylum")
library(asylum)
library(tsbox)
# install.packages(c("forecast", "tseries"))
library(forecast)
library(tseries)
library(ggplot2)
library(brcplot)


#--- get data ------------------------------------------------------------------

decisions <- fetch_decisions()
reunions  <- fetch_reunion()

ts_grants <- decisions |>
  filter(
    Date >= min(reunions$Date),
    UASC == "Non-UASC",
    `Applicant type` == "Main applicant",
    str_detect(`Case outcome group`, "Grant")
  ) |>
  group_by(Date) |>
  summarise(Grants = sum(Decisions)) |>
  ungroup() |>
  select(time = Date, value = Grants) |>
  ts_df()

ts_visas <-
  reunions |> 
  group_by(Date) |>
  summarise(Visas = sum(`Visas granted`)) |>
  ungroup() |>
  select(time = Date, value = Visas) |>
  ts_df()


#--- inspect -------------------------------------------------------------------

ts_plot(ts_grants, ts_visas)


#--- helper functions ----------------------------------------------------------

metrics <- function(y, yhat) {
  c(MEA   = mean(abs(yhat - y), na.rm = TRUE),
    MedEA = median(abs(yhat - y), na.rm = TRUE),
    RMSE  = sqrt(mean((yhat - y)^2, na.rm = TRUE))
  )
}


#--- Window average with concept drift detection ----------------------------

# assumption: time series fluctuate around the mean, but the mean can change

pred_robust_mean <- function(x) {
  vals <- tail(x$value, 3)
  out  <- which.max(rowMeans(abs(outer(vals, vals, `-`))))
  mean(vals[-out])
}

ts_pred <- data.frame(time = ts_visas$time, value = NA)
for(i in 31:61) {
  ts_pred$value[i] <- pred_robust_mean(ts_visas[1:(i-1),])
}

# visual
ts_plot(ts_visas, ts_pred)

# predicted - real
ts_plot(data.frame(time = ts_visas$time, value = ts_pred$value - ts_visas$value))

# metrics
metrics(ts_visas$value, ts_pred$value)

#--- Extend predictions for 4 quarters------------------------------------------
future_dates <- seq(from = ceiling_date(as.Date("2025-06-30"), unit = "quarter"), 
                    by = "3 months", length.out = 4) - days(1)

ts_pred_extended <- bind_rows(
  ts_pred,
  data.frame(time = future_dates, value = NA)
)

ts_pred_extended <- ts_pred_extended |> 
  filter(time > max(ts_visas$time))

full_ts <- bind_rows(ts_visas, ts_pred_extended)

for (i in (nrow(ts_pred) + 1):(nrow(full_ts))) {
  full_ts$value[i] <- pred_robust_mean(full_ts[1:(i - 1), ])
}

ts_pred_final <- bind_rows(ts_pred, full_ts[62:65,])

ts_plot(ts_visas, ts_pred_final)


#--- Add confidence intervals --------------------------------------------------
# Calculate sd of residuals on historical prediction window
residuals <- ts_visas$value[31:61] - ts_pred$value[31:61]
residual_sd <- sd(residuals, na.rm = TRUE)

alpha <- 0.05
z <- qnorm(1 - alpha/2) # 1.96 for 95% CI

ts_pred_final <- ts_pred_final |>
  mutate(
    lower = value - z * residual_sd,
    upper = value + z * residual_sd
  )

#--- Increase future forecasts by 30% ----------------------------------------
df_obs <- ts_visas|> mutate(Series = "Actual")
df_pred <- ts_pred_final |> mutate(Series = "Predicted")
df_all <- bind_rows(df_obs, df_pred)


df_pred_increase <- tail(df_pred, 5) 
df_pred_increase[2:5, c("value", "lower", "upper")] <- 
  df_pred_increase[2:5, c("value", "lower", "upper")] * 1.3
df_pred_increase$Series <- "Upper Scenario"
df_pred_increase$value[1] <- 5052


#--- Plot ----------------------------------------------------------------------
# Can see peak/troughs in historic data e.g. 2020, 2024 <- hard to predict
# Large CI < -- uncertainty, even then unable to predict peak/trough
# No forecasting horizon beyond 2 quarters really, because the window is 3 quarters -1
# Forecasts flatline beyond that. HO data is 1Q behind, so time horizon doesn't work
# Unpredictable peaks/troughs 

ggplot() +
  geom_line(data = df_all, aes(x = time, y = value, color = Series), size = 1) +
  geom_ribbon(data = df_pred, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = brc_colours$red_dunant) +
  geom_line(data = df_pred_increase, aes(x = time, y = value, color = Series), size = 1) +
  geom_ribbon(data = df_pred_increase, aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = brc_colours$earth) +
  labs(
    title = "Family Reunion Visas Granted, Actual and Forecasted",
    x = "Year",
    y = "Family Reunion Visas Granted",
    color = "",
    fill = ""
  ) +
  scale_x_date(
    date_breaks = "2 year",
    date_labels = "%Y",
    #limits = as.Date(c("2010-01-01", "2026-12-31"))
  ) +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = brc_colours$red_dunant, "Upper Scenario" = brc_colours$earth)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  theme_brc()

ggsave("analysis/asylum-destitution/fr-forecast-robust-mean.png")

# ---- Is there a relationship between the residuals and decisions made? ----
# Moderately strong correlation - 0.62
cor(residuals, ts_grants$value[31:61]) # align dates by index

model <- lm(residuals ~ ts_grants$value[31:61])

# R squared of 0.38 <- 38% of residual variance can be explained by decisions made
# p-value < 0.001 <- highly significant
# would be worth incorporating this into model
summary(model)

# ---- Incorporate decisions made into moving average model ----
# Store predictions
ts_pred_hybrid <- data.frame(
  time = ts_visas$time,
  value = NA
)

# Step 1: Estimate β using 1-quarter lag of grants
df_train <- data.frame(
  visas = ts_visas$value,
  grants_lag1 = dplyr::lag(ts_grants$value, 1)  # use 1-quarter lag
)

# Compute trimmed 3-quarter avg
df_train$avg_3q <- purrr::map_dbl(seq_len(nrow(df_train)), function(i) {
  if (i < 4) return(NA)
  vals <- ts_visas$value[(i - 3):(i - 1)]
  mean(vals[order(vals)][2:3])  # drop 1 outlier
})

# Residuals from moving average
df_train$residuals <- df_train$visas - df_train$avg_3q

# Fit regression to learn β using lagged grants
model <- lm(residuals ~ grants_lag1, data = df_train)
summary(model)

beta <- coef(model)["grants_lag1"]

# Step 2: Generate hybrid forecasts using grants lagged by 1 quarter
for (i in 34:60) {  # start at 34 to ensure we have enough history
  visa_vals <- ts_visas$value[(i - 3):(i - 1)]
  grants_val <- ts_grants$value[i - 1]  # <- use 1-quarter lag
  
  # Compute trimmed average
  visa_avg <- mean(visa_vals[order(visa_vals)][2:3])
  
  # Hybrid forecast
  forecast <- visa_avg + beta * grants_val
  ts_pred_hybrid$value[i] <- forecast
}

# visual
ts_plot(ts_visas, ts_pred_hybrid)

# metrics
metrics(ts_visas$value, ts_pred_hybrid$value)




# WIP---------------------------------------------------------------------------


# ---- add seasonality ----
boxplot(ts_visas$value ~ month(ts_grants$time), outline = FALSE)

# Step 1: Assign quarters and calculate seasonal means
ts_visas <- ts_visas |>
  mutate(quarter = lubridate::quarter(time))  # quarter as 1,2,3,4

seasonal_means <- ts_visas |>
  group_by(quarter) |>
  summarise(seasonal_mean = mean(value, na.rm = TRUE))

overall_mean <- mean(ts_visas$value, na.rm = TRUE)

seasonal_means <- seasonal_means |>
  mutate(adj = seasonal_mean - overall_mean)  # seasonal adjustment

# Step 2: Modified prediction function
pred_robust_mean_seasonal <- function(x, q) {
  vals <- tail(x$value, 3)
  out  <- which.max(rowMeans(abs(outer(vals, vals, `-`))))
  base_pred <- mean(vals[-out])
  
  adj <- seasonal_means$adj[seasonal_means$quarter == q]
  return(base_pred - adj)  # remove seasonal effect to get baseline prediction
}

# Step 3: Forecast loop with seasonal adjustment
ts_pred_seasonal <- data.frame(time = ts_visas$time, value = NA) %>%
  mutate(quarter = lubridate::quarter(time))

for(i in 31:61) {
  q <- ts_pred_seasonal$quarter[i]
  ts_pred_seasonal$value[i] <- pred_robust_mean_seasonal(ts_visas[1:(i-1),], q)
}

# visual
ts_plot(ts_visas, ts_pred_seasonal$value)

# predicted - real
ts_plot(data.frame(time = ts_visas$time, value = ts_pred_seasonal$value - ts_visas$value))

#--- compare -------------------------------------------------------------------

metrics(ts_visas$value, ts_pred_seasonal$value)
metrics(ts_visas$value, ts_pred$value)



