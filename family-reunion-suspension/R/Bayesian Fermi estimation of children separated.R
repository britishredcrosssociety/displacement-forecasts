library(tidyverse)
library(asylum)

#' Empirical-Bayes Gamma prior from historical counts
#'
#' We assume a Poisson–Gamma mixture across periods:
#'   y_t | λ  ~ Poisson(λ)
#'   λ   ~ Gamma(a, b)   (shape a, rate b)
#'
#' With constant-length periods, method-of-moments yields:
#'   μ  = mean(y)
#'   s2 = var(y)
#'   if s2 > μ:  b = μ / (s2 - μ),  a = μ * b
#'   else fallback to near-Poisson (very large a,b)
#'
#' @param counts numeric vector of historical FR grants per equal-length period.
#' @param overdisp_tolerance numeric, treat near-Poisson variance as Poisson if s2 <= μ*(1+tol).
#' @return list(a, b, mean = μ, var = s2)
fit_gamma_from_counts <- function(counts, overdisp_tolerance = 0.05) {
  stopifnot(is.numeric(counts), all(is.finite(counts)), all(counts >= 0))
  mu  <- mean(counts)
  s2  <- stats::var(counts)
  if (length(counts) == 1 || is.na(s2)) s2 <- mu  # degenerate: single point
  
  if (s2 <= mu * (1 + overdisp_tolerance)) {
    # Essentially Poisson across periods -> use a tight Gamma around mu
    # Large a,b keeps draws close to mu but still proper.
    a <- mu * 1e3 + 1e-6
    b <- 1e3
  } else {
    b <- mu / (s2 - mu)
    a <- mu * b
  }
  list(a = a, b = b, mean = mu, var = s2)
}

#' Empirical-Bayes Beta prior from historical child proportions
#'
#' Two modes:
#' 1) If totals are provided, reconstruct successes/failures and set:
#'      alpha = prior_strength_alpha + sum(children)
#'      beta  = prior_strength_beta  + sum(adults)
#'    (Uniform prior: 1,1. Jeffreys: 0.5,0.5.)
#' 2) If only proportions are provided, fit Beta by moments:
#'      m = mean(p), v = var(p), kappa = m*(1-m)/v - 1
#'      alpha = m*kappa, beta = (1-m)*kappa
#'    With tiny v, fall back to tight but proper prior.
#'
#' @param props  numeric vector in [0,1]
#' @param totals optional integer vector: FR grants per period (same length as props)
#' @param prior_strength_alpha numeric, default 1
#' @param prior_strength_beta  numeric, default 1
#' @return list(alpha, beta, mean = m, var = v, mode = "binomial" or "moments")
fit_beta_from_props <- function(
    props,
    totals = NULL,
    prior_strength_alpha = 1,
    prior_strength_beta  = 1
) {
  stopifnot(is.numeric(props), all(is.finite(props)), all(props >= 0 & props <= 1))
  if (!is.null(totals)) {
    stopifnot(length(totals) == length(props), all(totals >= 0))
    # Reconstruct successes/failures conservatively via rounding
    children <- round(props * totals)
    adults   <- totals - children
    alpha <- prior_strength_alpha + sum(children, na.rm = TRUE)
    beta  <- prior_strength_beta  + sum(adults,   na.rm = TRUE)
    m <- alpha / (alpha + beta)
    v <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
    mode <- "binomial"
  } else {
    # Moments fit to proportions themselves
    m <- mean(props, na.rm = TRUE)
    v <- stats::var(props, na.rm = TRUE)
    if (is.na(v) || v <= 1e-6) {
      # almost constant proportions; set a tight prior around m
      kappa <- 1e3
    } else {
      kappa <- m * (1 - m) / v - 1
      # Guard against negative/degenerate kappa (can happen with small samples)
      if (!is.finite(kappa) || kappa <= 2) kappa <- 50
    }
    alpha <- m * kappa
    beta  <- (1 - m) * kappa
    mode <- "moments"
  }
  list(alpha = alpha, beta = beta, mean = m, var = v, mode = mode)
}

#' Simulate future children granted Family Reunion visas
#'
#' Pipeline:
#'   λ ~ Gamma(a,b)   from fit_gamma_from_counts()
#'   N | λ ~ Poisson(λ * rate_adjust[t])
#'   θ ~ Beta(α,β)    from fit_beta_from_props()
#'   C | N,θ ~ Binomial(N, θ)
#'
#' @param counts_hist numeric vector of past FR grants per (equal) period.
#' @param props_hist  numeric vector of past child proportions in [0,1].
#' @param totals_hist optional integer vector of past totals aligned to props_hist (to reconstruct successes).
#' @param horizon integer, number of future periods to simulate.
#' @param n_sim integer, number of Monte Carlo draws.
#' @param resample_lambda logical; if TRUE, draw a fresh λ each future period (adds process volatility).
#' @param resample_theta  logical; if TRUE, draw a fresh θ each future period (lets composition vary).
#' @param rate_adjust optional numeric vector length horizon; multiplicative factors on expected counts (e.g., trend/scenario).
#' @param seed optional integer for reproducibility.
#' @return list(draws = list(children = matrix, grants = matrix, p_child = matrix),
#'              priors = list(gamma = ..., beta = ...),
#'              summary = tibble with per-period stats,
#'              total_summary = tibble with total-across-horizon stats)
simulate_fr <- function(
    counts_hist,
    props_hist,
    totals_hist = NULL,
    horizon = 4,
    n_sim = 10000,
    resample_lambda = TRUE,
    resample_theta  = TRUE,
    rate_adjust = NULL,
    seed = NULL
) {
  stopifnot(horizon >= 1, n_sim >= 1)
  if (!is.null(seed)) set.seed(seed)
  
  gfit <- fit_gamma_from_counts(counts_hist)
  bfit <- fit_beta_from_props(props_hist, totals_hist)
  
  if (is.null(rate_adjust)) rate_adjust <- rep(1, horizon)
  stopifnot(length(rate_adjust) == horizon, all(rate_adjust >= 0))
  
  # Draw λ and θ
  if (resample_lambda) {
    lambda_mat <- matrix(stats::rgamma(n_sim * horizon, shape = gfit$a, rate = gfit$b),
                         nrow = n_sim, ncol = horizon)
  } else {
    lambda_vec <- stats::rgamma(n_sim, shape = gfit$a, rate = gfit$b)
    lambda_mat <- matrix(rep(lambda_vec, horizon), nrow = n_sim, ncol = horizon)
  }
  
  if (resample_theta) {
    theta_mat <- matrix(stats::rbeta(n_sim * horizon, shape1 = bfit$alpha, shape2 = bfit$beta),
                        nrow = n_sim, ncol = horizon)
  } else {
    theta_vec <- stats::rbeta(n_sim, shape1 = bfit$alpha, shape2 = bfit$beta)
    theta_mat <- matrix(rep(theta_vec, horizon), nrow = n_sim, ncol = horizon)
  }
  
  # Adjust expected counts by scenario multipliers
  lambda_mat <- sweep(lambda_mat, 2, rate_adjust, `*`)
  
  # Simulate grants then children
  grants_mat   <- matrix(0L, n_sim, horizon)
  children_mat <- matrix(0L, n_sim, horizon)
  for (t in seq_len(horizon)) {
    grants_mat[, t]   <- stats::rpois(n_sim, lambda = lambda_mat[, t])
    theta_t           <- pmin(pmax(theta_mat[, t], 1e-9), 1 - 1e-9) # guardrails
    children_mat[, t] <- stats::rbinom(n_sim, size = grants_mat[, t], prob = theta_t)
  }
  
  # Summaries
  qfun <- function(x) stats::quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), names = FALSE, type = 8)
  period_stats <- purrr::map_dfr(seq_len(horizon), function(t) {
    ct <- children_mat[, t]
    gt <- grants_mat[, t]
    tibble::tibble(
      period = t,
      grants_mean   = mean(gt),
      grants_median = stats::median(gt),
      grants_q05    = qfun(gt)[1],
      grants_q95    = qfun(gt)[5],
      children_mean   = mean(ct),
      children_median = stats::median(ct),
      children_q05    = qfun(ct)[1],
      children_q25    = qfun(ct)[2],
      children_q50    = qfun(ct)[3],
      children_q75    = qfun(ct)[4],
      children_q95    = qfun(ct)[5]
    )
  })
  
  total_children <- rowSums(children_mat)
  total_grants   <- rowSums(grants_mat)
  total_summary <- tibble::tibble(
    horizon = horizon,
    grants_mean   = mean(total_grants),
    grants_median = stats::median(total_grants),
    grants_q05    = qfun(total_grants)[1],
    grants_q95    = qfun(total_grants)[5],
    children_mean   = mean(total_children),
    children_median = stats::median(total_children),
    children_q05    = qfun(total_children)[1],
    children_q25    = qfun(total_children)[2],
    children_q50    = qfun(total_children)[3],
    children_q75    = qfun(total_children)[4],
    children_q95    = qfun(total_children)[5]
  )
  
  list(
    draws = list(children = children_mat, grants = grants_mat, p_child = theta_mat, lambda = lambda_mat),
    priors = list(gamma = gfit, beta = bfit),
    summary = period_stats,
    total_summary = total_summary
  )
}

#' Convenience: tidy summary for a vector or matrix of draws
#' @param x numeric vector or matrix (rows = sims)
#' @param name label
#' @return tibble with mean/median/quantiles
summarise_sim <- function(x, name = "value") {
  q <- c(0.05, 0.25, 0.5, 0.75, 0.95)
  qfun <- function(v) stats::quantile(v, probs = q, names = FALSE, type = 8)
  if (is.vector(x)) {
    tibble::tibble(
      name = name,
      mean = mean(x),
      median = stats::median(x),
      q05 = qfun(x)[1],
      q25 = qfun(x)[2],
      q50 = qfun(x)[3],
      q75 = qfun(x)[4],
      q95 = qfun(x)[5]
    )
  } else {
    tibble::tibble(
      period = seq_len(ncol(x)),
      mean   = apply(x, 2, mean),
      median = apply(x, 2, stats::median),
      q05    = apply(x, 2, function(v) qfun(v)[1]),
      q25    = apply(x, 2, function(v) qfun(v)[2]),
      q50    = apply(x, 2, function(v) qfun(v)[3]),
      q75    = apply(x, 2, function(v) qfun(v)[4]),
      q95    = apply(x, 2, function(v) qfun(v)[5])
    )
  }
}

# clean up data column names
clean_cols <- function(df) {
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- trimws(colnames(df))
  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("_/_", "_", colnames(df))
  df
}

# ---- Prep Family Reunion visa data ----
fr <- clean_cols(fetch_reunion())

# Check trends in FR visas over time - looking for the date from which visas surged
# We'll use data up to this point (minus the three quarters )
fr_summary_all <- fr |> 
  group_by(date) |> 
  summarise(visas_granted = sum(visas_granted)) |> 
  ungroup()

# What proportion of FR visas are adults vs children?
fr_adults_kids_all <- fr |> 
  mutate(age_group = if_else(age == "Under 18", "Child", "Adult")) |> 
  group_by(date, age_group) |> 
  summarise(visas_granted = sum(visas_granted)) |>
  ungroup() |> 
  group_by(date) |> 
  mutate(prop = visas_granted / sum(visas_granted)) |> 
  ungroup()

# ---- Test model against historical data ----
# Look for when the step change in visa grants began
fr_summary_all |> 
  ggplot(aes(x = date, y = visas_granted)) +
  geom_line()

# Surge in visas started from 2023-09-30, so we'll use the previous quarter 
# (2023-06-30) as the cut-off. We want to test model performance over a three-quarter
# time horizon, so will use data up to 2022-09-30 (inclusive) for the priors.
to_date <- as.Date("2022-09-30")

# Fetch data only up to when the step change in grants began, minus three quarters
fr_summary <- fr |> 
  filter(date <= to_date) |>
  group_by(date) |> 
  summarise(visas_granted = sum(visas_granted)) |> 
  ungroup()

fr_adults_kids <- fr_adults_kids_all |> 
  filter(date <= to_date)

# Fit priors from history
gamma_prior <- fit_gamma_from_counts(fr_summary$visas_granted)
beta_prior  <- fit_beta_from_props(fr_adults_kids[fr_adults_kids$age_group == "Child", ]$prop, totals = fr_summary$visas_granted)  # uses reconstructed successes/failures

# Visualize priors
p_grants <- stats::rgamma(1000, shape = gamma_prior$a, rate = gamma_prior$b)
hist(p_grants)
# Overlay actual data onto histogram
hist(fr_summary$visas_granted, add = TRUE, col = rgb(1,0,0,0.5))

p_children <- stats::rbeta(1000, shape1 = beta_prior$alpha, shape2 = beta_prior$beta)
hist(p_children)
# Overlay actual data onto histogram
hist(fr_adults_kids[fr_adults_kids$age_group == "Child", ]$prop, add = TRUE, col = rgb(1,0,0,0.5))

# Forecast next 3 periods (i.e. to 2023-06-30)
sim <- simulate_fr(
  counts_hist = fr_summary$visas_granted,
  props_hist  = fr_adults_kids[fr_adults_kids$age_group == "Child", ]$prop,
  totals_hist = fr_summary$visas_granted,
  horizon = 3,
  n_sim = 10000,
  resample_lambda = TRUE,  # allow process variability period-to-period
  resample_theta  = TRUE,  # allow composition to drift
  rate_adjust = c(1.00, 1.02, 1.02),  # optional trend/scenario (e.g., +2% per period)
  seed = 2025
)

# Compare forecast to actual number of children granted visas
test_accuracy <- 
  fr_adults_kids_all |> 
  filter(date > to_date & date <= as.Date("2023-06-30")) |> 
  filter(age_group == "Child") |> 
  mutate(period = row_number()) |> 
  
  left_join(
    sim$summary |> 
      select(period, starts_with("children_")),
    by = "period"
  )

test_accuracy |> 
  ggplot(aes(x = date, y = visas_granted)) +
  geom_ribbon(aes(ymin = children_q05, ymax = children_q95), alpha = 0.3) +
  geom_line(aes(y = children_median)) +
  geom_point(colour = "red", size = 1.1) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

# Two of the three periods fall within 95% prediction intervals
# The first period (2022-12-31) is slightly out: actual number of visas granted
# to children is 37 visas lower than the bottom of our predicted range.

# ---- Forecast future visas granted ----
# Date from which Family Reunion visa grants became high each quarter
from_date <- as.Date("2024-06-30")

fr_summary <- fr_summary_all |> 
  filter(date >= from_date)

# What proportion of FR visas are adults vs children?
fr_adults_kids <- fr_adults_kids_all |> 
  filter(date >= from_date)

# Fit priors from history
gamma_prior <- fit_gamma_from_counts(fr_summary$visas_granted)
beta_prior  <- fit_beta_from_props(fr_adults_kids[fr_adults_kids$age_group == "Child", ]$prop, totals = fr_summary$visas_granted)  # uses reconstructed successes/failures

# Visualize priors
p_grants <- stats::rgamma(1000, shape = gamma_prior$a, rate = gamma_prior$b)
hist(p_grants)
# Overlay actual data onto histogram
hist(fr_summary$visas_granted, add = TRUE, col = rgb(1,0,0,0.5))

p_children <- stats::rbeta(1000, shape1 = beta_prior$alpha, shape2 = beta_prior$beta)
hist(p_children)
# Overlay actual data onto histogram
hist(fr_adults_kids[fr_adults_kids$age_group == "Child", ]$prop, add = TRUE, col = rgb(1,0,0,0.5))

# Forecast next 3 periods (i.e. to the end of March 2026)
sim <- simulate_fr(
  counts_hist = fr_summary$visas_granted,
  props_hist  = fr_adults_kids[fr_adults_kids$age_group == "Child", ]$prop,
  totals_hist = fr_summary$visas_granted,
  horizon = 3,
  n_sim = 10000,
  resample_lambda = TRUE,  # allow process variability period-to-period
  resample_theta  = TRUE,  # allow composition to drift
  rate_adjust = c(1.00, 1.02, 1.02),  # optional trend/scenario (e.g., +2% per period)
  seed = 2025
)

# Per-period forecast summary for children
sim$summary |> 
  select(period, starts_with("children_")) |> 
  # For the first quarter, we only want September (the final month) so 
  # divide every column starting with "children" by 3
  mutate(across(starts_with("children_"), ~ if_else(period == 1, .x / 3, .x))) |> 
  
  # Totals
  summarise(across(starts_with("children_"), sum))

# ---- Forecast adults, as a proxy for family units ----
beta_prior_adults  <- fit_beta_from_props(fr_adults_kids[fr_adults_kids$age_group == "Adult", ]$prop, totals = fr_summary$visas_granted)  # uses reconstructed successes/failures
p_adults <- stats::rbeta(1000, shape1 = beta_prior_adults$alpha, shape2 = beta_prior_adults$beta)
hist(p_adults)
# Overlay actual data onto histogram
hist(fr_adults_kids[fr_adults_kids$age_group == "Adult", ]$prop, add = TRUE, col = rgb(1,0,0,0.5))

# Forecast next 3 periods (i.e. to the end of March 2026)
sim_adults <- simulate_fr(
  counts_hist = fr_summary$visas_granted,
  props_hist  = fr_adults_kids[fr_adults_kids$age_group == "Adult", ]$prop,
  totals_hist = fr_summary$visas_granted,
  horizon = 3,
  n_sim = 10000,
  resample_lambda = TRUE,  # allow process variability period-to-period
  resample_theta  = TRUE,  # allow composition to drift
  rate_adjust = c(1.00, 1.02, 1.02),  # optional trend/scenario (e.g., +2% per period)
  seed = 2025
)

# Per-period forecast summary for children
sim_adults$summary |> 
  select(period, starts_with("children_")) |> 
  # For the first quarter, we only want September (the final month) so 
  # divide every column starting with "children" by 3
  mutate(across(starts_with("children_"), ~ if_else(period == 1, .x / 3, .x))) |> 
  
  # Totals
  summarise(across(starts_with("children_"), sum))
