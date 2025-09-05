#———————————————#
# 1. LOAD LIBRARIES  #
#———————————————#
# If not already installed, do:
# install.packages(c("dplyr", "zoo", "forecast", "ggplot2", "tseries"))
library(dplyr)
library(zoo)        # for as.yearqtr()
library(forecast)   # for Arima() and forecast()
library(ggplot2)
library(tseries)    # for na.action in ccf()
library(asylum)

#———————————————#
# 2. AGGREGATE DATA  #
#———————————————#

decisions <- fetch_decisions()
fr <- fetch_reunion()

# 2.1. Sum up family-reunion visas by Date & Nationality
agg_visas <- fr |> 
  group_by(Date, Nationality) |>
  summarise(Family_Visas = sum(`Visas granted`), .groups = "drop") |> 
  
  # Nationalities with few visas granted are not useful for modeling
  filter(Family_Visas >= 10)

# 2.2. Keep only refugee-status GRANTS from decisions
agg_decisions <- decisions |>
  filter(
    `Applicant type` == "Main applicant" &
      str_detect(`Case outcome group`, "Grant") &
      UASC == "Non-UASC"
  ) |> 
  group_by(Date, Nationality) |>
  summarise(Refugee_Grants = sum(Decisions), .groups = "drop") |> 

  # Keep only dates and nationalities that are in both datasets  
  filter(Date >= min(agg_visas$Date)) |> 
  filter(Nationality %in% agg_visas$Nationality)

# 2.3. Join them so each row has (Date, Nationality, Refugee_Grants, Family_Visas)
df_nat <- full_join(agg_decisions, agg_visas,
                    by = c("Date", "Nationality")) |>
  arrange(Nationality, Date)

# 2.4. Replace any missing counts with 0 (if you want zeros when there were no visas or no grants)
#      Alternatively, you could leave them as NA if you prefer to omit those quarters entirely.
df_nat <- df_nat |>
  mutate(
    Refugee_Grants = ifelse(is.na(Refugee_Grants), 0, Refugee_Grants),
    Family_Visas   = ifelse(is.na(Family_Visas),   0, Family_Visas)
  )

# 2.5. Convert Date to zoo::yearqtr so we know it’s quarterly
#      (We assume Date is e.g. “2019-03-31”, “2019-06-30”, etc.)
df_nat <- df_nat |>
  mutate(Quarter = as.yearqtr(Date, format = "%Y-%m-%d"))

#———————————————————————————————#
# 3. NATIONALITY‐SPECIFIC MODELING  #
#———————————————————————————————#
results_list <- list()

for (nat in unique(df_nat$Nationality)) {
  sub <- df_nat %>% filter(Nationality == nat) %>% arrange(Quarter)
  if (nrow(sub) < 8) next
  
  # Build ts-series for this nationality
  year0    <- as.numeric(format(as.Date(as.yearqtr(sub$Quarter[1])), "%Y"))
  month0   <- as.numeric(format(as.Date(as.yearqtr(sub$Quarter[1])), "%m"))
  quarter0 <- (month0 - 1) %/% 3 + 1
  
  ts_refugee <- ts(sub$Refugee_Grants,
                   start = c(year0, quarter0), frequency = 4)
  ts_family  <- ts(sub$Family_Visas,
                   start = c(year0, quarter0), frequency = 4)
  
  # 3.1. Cross‐correlation to find optimal lag (0 to 8 quarters)
  ccf_res <- ccf(ts_refugee, ts_family, lag.max = 8, plot = FALSE, na.action = na.omit)
  all_lags  <- as.numeric(ccf_res$lag[, 1, 1])
  all_corrs <- as.numeric(ccf_res$acf[, 1, 1])
  pos_idx   <- which(all_lags >= 0)
  lags_pos  <- all_lags[pos_idx]
  corr_pos  <- all_corrs[pos_idx]
  best_lag  <- lags_pos[which.max(corr_pos)]
  
  # 3.2. Create lagged exogenous (refugee shifted by best_lag)
  refugee_lagged <- stats::lag(ts_refugee, -best_lag)
  
  # 3.3. Trim both series by dropping first 'best_lag' quarters
  add_years    <- best_lag %/% 4
  add_quarters <- best_lag %% 4
  new_year     <- year0 + add_years
  new_qtr      <- quarter0 + add_quarters
  if (new_qtr > 4) {
    new_year <- new_year + 1
    new_qtr  <- new_qtr - 4
  }
  
  ts_ref_trim     <- window(ts_refugee,   start = c(new_year, new_qtr))
  ts_fam_trim     <- window(ts_family,    start = c(new_year, new_qtr))
  ts_ref_lag_trim <- window(refugee_lagged, start = c(new_year, new_qtr))
  
  if (length(ts_fam_trim) < 12) next
  
  # 3.4. Stationarity check (ADF test). If p‐value >= 0.05, difference once.
  adf_pval <- adf.test(ts_fam_trim, k = 0)$p.value
  if (adf_pval >= 0.05) {
    d_order <- 1
  } else {
    d_order <- 0
  }
  
  # Alternatively, use ndiffs() to recommend how many differences:
  # d_order <- ndiffs(ts_fam_trim, alpha = 0.05, test = "adf")
  # (But we force at least 1 if adf_pval >= 0.05.)
  
  # 3.5. Train/Test split (last 4 obs as test)
  n_total <- length(ts_fam_trim)
  n_test  <- 4
  n_train <- n_total - n_test
  
  y_train <- window(ts_fam_trim, end = time(ts_fam_trim)[n_train])
  x_train <- window(ts_ref_lag_trim, end = time(ts_ref_lag_trim)[n_train])
  
  y_test  <- window(ts_fam_trim, start = time(ts_fam_trim)[n_train + 1])
  x_test  <- window(ts_ref_lag_trim, start = time(ts_ref_lag_trim)[n_train + 1])
  
  # 3.6. Fit ARIMAX via auto.arima, forcing d = d_order
  #     (This avoids the “non‐stationary AR” error if we needed to difference.)
  fit_arimax <- auto.arima(
    y_train,
    d      = d_order,
    xreg   = as.numeric(x_train),
    seasonal = FALSE,        # No seasonal terms unless you want them
    stepwise  = FALSE,       # allow fuller search
    approximation = FALSE
  )
  
  # 3.7. Forecast test period
  fc_test <- forecast(
    fit_arimax,
    h    = n_test,
    xreg = as.numeric(x_test)
  )
  
  # 3.8. Future 4‐quarter forecasts for each nationality
  last_refugee_val <- tail(ts_refugee, 1)
  future_xreg      <- ts(
    rep(last_refugee_val, 4),
    start     = time(ts_fam_trim)[n_total] + c(0, 1/4),
    frequency = 4
  )
  fc_future <- forecast(
    fit_arimax,
    h    = 4,
    xreg = as.numeric(future_xreg)
  )
  
  # 3.9. Store everything
  results_list[[nat]] <- list(
    best_lag       = best_lag,
    d_order        = d_order,
    model          = fit_arimax,
    ccf            = ccf_res,
    forecast_test  = fc_test,
    forecast_future = fc_future
  )
}

#—————————————————————————————#
# 4. GATHER “FUTURE” FORECASTS #
#—————————————————————————————#

# We want a data frame that shows, for each Nationality, the next 4 Quarterly forecasts:
# columns: Nationality | Quarter | Forecast | Lo95 | Hi95

library(purrr)  # for map_df()

future_df <- map_df(names(results_list), function(nat) {
  fc_fut <- results_list[[nat]]$forecast_future
  data.frame(
    Nationality = nat,
    Quarter     = time(fc_fut$mean),             # e.g. 2024.75 = Q3 2024 (depending on your timeline)
    Forecast    = as.numeric(fc_fut$mean),
    Lo95        = as.numeric(fc_fut$lower[, 2]),  # 95% lower
    Hi95        = as.numeric(fc_fut$upper[, 2])   # 95% upper
  )
})

# Re‐format Quarter into yearqtr (optional)
future_df <- future_df |>
  mutate(
    # Convert numeric “time” back to year‐quarter
    Year     = floor(Quarter),
    QuarterN = round((Quarter - Year) * 4 + 1),
    YearQtr  = paste0(Year, " Q", QuarterN),
    Date = ceiling_date(yq(YearQtr), unit = "quarter") - days(1)
    #Date = yq(paste0(Year, "-Q", QuarterN))
  ) |>
  select(Nationality, Date, Year, QuarterN, YearQtr, Forecast, Lo95, Hi95) |> 
  as_tibble()

# Print the nationality‐specific 4‐quarter‐ahead forecasts
print(future_df)

#————————————————————————————————————————————#
# (Optional) 5. VISUALIZE A FEW NATIONALITY FITS & FORECASTS #
#————————————————————————————————————————————#

# Example: Plot for the first nationality in results_list
example_nat <- names(results_list)[10]
res       <- results_list[[example_nat]]

# Time series to plot: 
sub        <- df_nat |> filter(Nationality == example_nat) 
year0      <- as.numeric(format(as.Date(first(sub$Quarter)), "%Y"))
month0     <- as.numeric(format(as.Date(first(sub$Quarter)), "%m"))
quart0     <- (month0 - 1) %/% 3 + 1
ts_ref     <- ts(sub$Refugee_Grants, start = c(year0, quart0), frequency = 4)
ts_fam     <- ts(sub$Family_Visas,   start = c(year0, quart0), frequency = 4)

# Plot actual vs fitted + 95% CI for test period + future
autoplot(ts_fam, series = "Actual FamilyVisas") +
  autolayer(res$forecast_test$mean, series = "Forecast (Test)") +
  autolayer(res$forecast_test$lower[,2], series = "95% CI Lower (Test)", lty = "dashed") +
  autolayer(res$forecast_test$upper[,2], series = "95% CI Upper (Test)", lty = "dashed") +
  autolayer(res$forecast_future$mean, series = "Forecast (Next 4 quarters)",
            lty = "solid") +
  autolayer(res$forecast_future$lower[,2], series = "95% CI Lower (Future)", lty = "dotted") +
  autolayer(res$forecast_future$upper[,2], series = "95% CI Upper (Future)", lty = "dotted") +
  ggtitle(paste0("Nationality = ", example_nat,
                 " – Family Reunions vs Forecasts")) +
  xlab("Year") + ylab("Count of Family Visas") +
  theme_minimal() +
  scale_colour_manual(values = c(
    "Actual FamilyVisas"               = "black",
    "Forecast (Test)"                  = "blue",
    "95% CI Lower (Test)"              = "lightblue",
    "95% CI Upper (Test)"              = "lightblue",
    "Forecast (Next 4 quarters)"       = "darkgreen",
    "95% CI Lower (Future)"            = "gray50",
    "95% CI Upper (Future)"            = "gray50"
  ))

#————————————————————————————————————————————#
# COMPARE TOTAL FORECASTS TO ACTUALS         #
#————————————————————————————————————————————#

total_future <- future_df %>%
  group_by(Date) %>%
  summarize(
    Total_Forecast = sum(Forecast),
    Total_Lo95      = sum(Lo95),
    Total_Hi95      = sum(Hi95)
  ) %>%
  ungroup() |> 
  filter(Date >= ymd("2024-06-30"))

actual_totals <- df_nat %>%
  group_by(Date) %>%
  summarize(
    Actual_Total = sum(Family_Visas, na.rm = TRUE),
    .groups = "drop"
  )

comparison_df <- actual_totals %>%
  left_join(total_future, by = "Date")

print(comparison_df)

# Plot total forecasts vs actuals
comparison_df |> 
  ggplot(aes(x = Date, y = Actual_Total)) +
  geom_line() +
  geom_line(aes(y = Total_Forecast), color = "blue") +
  geom_ribbon(aes(ymin = Total_Lo95, ymax = Total_Hi95), fill = "lightblue", alpha = 0.5) +
  labs(title = "Total Family Visas: Actual vs Forecast",
       x = "Date", y = "Count of Family Visas")
