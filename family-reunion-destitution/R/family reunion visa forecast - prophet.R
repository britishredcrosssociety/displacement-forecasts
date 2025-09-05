## Exploration of Prophet, building off Cynkra's initial work 
# WIP

# ---- Init -----
library(tidyverse)
library(asylum)
library(tsbox)
library(prophet)
library(forecast)
library(tseries)
library(ggplot2)


# ---- Get data ----
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


# ---- Explore data ----
ts_plot(ts_grants, ts_visas)

# Are grants and visas correlated?
# Moderate + correlation
cor(ts_visas$value, ts_grants$value)

# Strong correlation with a three quarter lag - 0.74 
cor(ts_visas$value, lag(ts_grants$value, 3), use = "complete.obs")

# Visualise regression
visas_lagged <- data.frame(
  visas = ts_visas$value,
  grants_lag3 = lag(ts_grants$value, 3)
) |>
  filter(!is.na(grants_lag3))  

ggplot(df_lagged, aes(x = grants_lag3, y = visas)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) 


#--- helper functions ----------------------------------------------------------

metrics <- function(y, yhat) {
  c(MEA   = mean(abs(yhat - y), na.rm = TRUE),
    MedEA = median(abs(yhat - y), na.rm = TRUE),
    RMSE  = sqrt(mean((yhat - y)^2, na.rm = TRUE))
  )
}

# ---- Rolling forecasts, linear regression with 3 Q lag 0 ----
ts_pred_lm <- data.frame(time = ts_visas$time, value = NA)

for (i in 34:60) {  # start at 34 because we need 3 lags
  # Training window up to (i-1)
  train_visas <- ts_visas[1:(i-1), ]
  train_grants <- ts_grants[1:(i-1), ]
  
  # Add lagged grants
  df_train <- data.frame(
    ds = train_visas$time,
    y = train_visas$value,
    grants_lag3 = lag(train_grants$value, 3)
  ) %>%
    filter(!is.na(grants_lag3))  # remove early rows with NA lag
  
  # Fit linear model: y ~ grants_lag3
  lm_model <- lm(y ~ grants_lag3, data = df_train)
  
  # Index of prediction date
  current_index <- i - 1
  forecast_index <- current_index + 1
  
  # Index for lagged grant: 3 quarters earlier than forecast date
  lag_index <- forecast_index - 3
  
  # Only predict if lag_index is within range
  if (lag_index >= 1 && forecast_index <= nrow(ts_visas)) {
    grant_value_lag3 <- ts_grants$value[lag_index]
    
    # Predict
    newdata <- data.frame(grants_lag3 = grant_value_lag3)
    yhat <- predict(lm_model, newdata)
    
    # Assign prediction to position i
    ts_pred_lm$value[i] <- yhat
  }
}

# visual
ts_plot(ts_visas, ts_pred_lm)

metrics(ts_visas$value, ts_pred_lm$value)
metrics(ts_visas$value, ts_pred_2$value)


#--- 2. prophet ----------------------------------------------------------------

# assumption: data has complex tendencies which can be parameterized
# NOTE: for now, under-explored
# Params explored: UK holidays - no effect, 

pred_prophet <- function(x) {
  x   <- setNames(x, c("ds", "y"))
  
  # Configure model
  m <- prophet(x, 
               weekly.seasonality = FALSE, 
               daily.seasonality = FALSE, 
               growth = "linear",
               fit = FALSE
               )
  
  # Add Uk holidays
  # m <- add_country_holidays(m, country_name = "UK")
  
  # Fit model
  m <- fit.prophet(m, x)
  
  # Create future df to forecast one quarter at a time 
  ndf <- data.frame(ds = tail(x$ds, 1) %m+% months(3))
  
  # Predict and pull out predictions
  predict(m, ndf)$yhat
}

ts_pred_2 <- data.frame(time = ts_visas$time, value = NA)
for(i in 31:60) {
  ts_pred_2$value[i] <- pred_prophet(ts_visas[1:(i-1),])
}

# visual
ts_plot(ts_visas, ts_pred_2)

# predicted - real
ts_plot(data.frame(time = ts_visas$time, value = ts_pred_2$value - ts_visas$value))

# look at which holidays impacted the model
# m$train.holiday.names
#--- compare -------------------------------------------------------------------

metrics(ts_visas$value, ts_pred_1$value)
metrics(ts_visas$value, ts_pred_2$value)
metrics(ts_visas$value, ts_pred_3$value)
metrics(ts_visas$value, ts_pred_4$value)


#--- explore -------------------------------------------------------------------

# startify by season
boxplot(ts_visas$value ~ month(ts_grants$time), outline = FALSE)