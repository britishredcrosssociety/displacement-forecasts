#--- init ----------------------------------------------------------------------

library(tidyverse)
# remotes::install_github("humaniverse/asylum")
library(asylum)
library(tsbox)
library(prophet)
# install.packages(c("forecast", "tseries"))
library(forecast)
library(tseries)


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


#--- 1. baseline ---------------------------------------------------------------

# assumption: tomorrow will be exactly the same as today

pred_base <- function(x) {
  tail(x$value, 1)
}

ts_pred_1 <- data.frame(time = ts_visas$time, value = NA)
for(i in 31:60) {
  ts_pred_1$value[i] <- pred_base(ts_visas[1:(i-1),])
}

metrics(ts_visas$value, ts_pred_1$value)

# visual
ts_plot(ts_visas, ts_pred_1)

# predicted - real
ts_plot(data.frame(time = ts_visas$time, value = ts_pred_1$value - ts_visas$value))


#--- 2. prophet ----------------------------------------------------------------

# assumption: data has complex tendencies which can be parameterized
# NOTE: for now, under-explored

pred_prophet <- function(x) {
  x   <- setNames(x, c("ds", "y"))
  fit <- prophet(x, weekly.seasonality = FALSE, daily.seasonality = FALSE, growth = "flat")
  ndf <- data.frame(ds = tail(x$ds, 1) %m+% months(3))
  predict(fit, ndf)$yhat
}

ts_pred_2 <- data.frame(time = ts_visas$time, value = NA)
for(i in 31:60) {
  ts_pred_2$value[i] <- pred_prophet(ts_visas[1:(i-1),])
}

# visual
ts_plot(ts_visas, ts_pred_2)

# predicted - real
ts_plot(data.frame(time = ts_visas$time, value = ts_pred_2$value - ts_visas$value))


#--- 3. window average ---------------------------------------------------------

# assumption: dat has different averages that are better estimated in windows

pred_mean <- function(x) {
  mean(tail(x$value, 4))
}

ts_pred_3 <- data.frame(time = ts_visas$time, value = NA)
for(i in 31:60) {
  ts_pred_3$value[i] <- pred_mean(ts_visas[1:(i-1),])
}

# visual
ts_plot(ts_visas, ts_pred_3)

# predicted - real
ts_plot(data.frame(time = ts_visas$time, value = ts_pred_3$value - ts_visas$value))


#--- 4. window average with concept drift detection ----------------------------

# assumption: time series fluctuate around the mean, but the mean can change

pred_robust_mean <- function(x) {
  vals <- tail(x$value, 3)
  out  <- which.max(rowMeans(abs(outer(vals, vals, `-`))))
  mean(vals[-out])
}

ts_pred_4 <- data.frame(time = ts_visas$time, value = NA)
for(i in 31:60) {
  ts_pred_4$value[i] <- pred_robust_mean(ts_visas[1:(i-1),])
}

# visual
ts_plot(ts_visas, ts_pred_4)

# predicted - real
ts_plot(data.frame(time = ts_visas$time, value = ts_pred_4$value - ts_visas$value))


#--- compare -------------------------------------------------------------------

ts_plot(ts_c(ts_visas, ts_pred_4))

metrics(ts_visas$value, ts_pred_1$value)
metrics(ts_visas$value, ts_pred_2$value)
metrics(ts_visas$value, ts_pred_3$value)
metrics(ts_visas$value, ts_pred_4$value)


#--- explore -------------------------------------------------------------------

# startify by season
boxplot(ts_visas$value ~ month(ts_grants$time), outline = FALSE)


