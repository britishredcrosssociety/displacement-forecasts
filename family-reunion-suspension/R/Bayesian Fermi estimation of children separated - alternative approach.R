# This version is based on feedback from Professor Chris Sherlock at Lancaster University
# suggesting we could simulate visas from a negative binomial distribution,
# which is mathematically equivalent to our original approach.
library(tidyverse)
library(asylum)
library(rethinking)

# clean up data column names
clean_cols <- function(df) {
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- trimws(colnames(df))
  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("_/_", "_", colnames(df))
  df
}

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
  mu <- mean(counts)
  s2 <- stats::var(counts)
  if (length(counts) == 1 || is.na(s2)) {
    s2 <- mu
  } # degenerate: single point

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

# ---- Prep Family Reunion visa data ----
fr <- clean_cols(fetch_reunion())

# What proportion of FR visas are adults vs children?
fr_adults_kids_all <- fr |>
  mutate(age_group = if_else(age == "Under 18", "Child", "Adult")) |>
  group_by(date, age_group) |>
  summarise(visas_granted = sum(visas_granted)) |>
  ungroup() |>
  group_by(date) |>
  mutate(prop = visas_granted / sum(visas_granted)) |>
  ungroup()

# Date from which Family Reunion visa grants became high each quarter
from_date <- as.Date("2024-06-30")

# Surge in visas started from 2023-09-30, so we'll use the previous quarter
# (2023-06-30) as the cut-off. We want to test model performance over a three-quarter
# time horizon, so will use data up to 2022-09-30 (inclusive) for the priors.
to_date <- as.Date("2022-09-30")

fr_adults_kids_testing <- fr_adults_kids_all |>
  filter(date <= to_date)

# What proportion of FR visas are adults vs children?
fr_adults_kids <- fr_adults_kids_all |>
  filter(date >= from_date)

# ---- Children ----
# Gamma parameters for children
# Fit priors from history
gamma_child <- fit_gamma_from_counts(
  fr_adults_kids[
    fr_adults_kids$age_group == "Child",
  ]$visas_granted
)

a_child = gamma_child$a
b_child = gamma_child$b

# Visualize priors
p_grants_child <- rgamma(1000, shape = a_child, rate = b_child)
hist(p_grants_child)
# Overlay actual data onto histogram
hist(
  fr_adults_kids[
    fr_adults_kids$age_group == "Child",
  ]$visas_granted,
  add = TRUE,
  col = rgb(1, 0, 0, 0.5)
)

n = 10000 ## create 10 quarters of data
ts = (1:n) / 4 ## quarterly times of the data
beta = 1

## Sim data
ps = b_child / (b_child + beta^ts) ## R doesn't like "b", it wants a p or a mu (See later)

xs = rnbinom(n, a_child, ps)
xs2 = rnbinom(n, a_child, ps)
xs3 = rnbinom(n, a_child, ps)

median(xs)

# Histogram of simulated data
hist(xs)

# Overlay actual data onto histogram
hist(
  fr_adults_kids_testing[
    fr_adults_kids_testing$age_group == "Child",
  ]$visas_granted,
  add = TRUE,
  col = rgb(1, 0, 0, 0.5)
)

xs = xs / 3 # only want September for first quarter
draws_children_total <- xs + xs2 + xs3

rethinking::HPDI(draws_children_total, prob = 0.95)

# ---- Adults ----
# Gamma parameters for adults
# Fit priors from history
gamma_adult <- fit_gamma_from_counts(
  fr_adults_kids[
    fr_adults_kids$age_group == "Adult",
  ]$visas_granted
)

a_adult = gamma_adult$a
b_adult = gamma_adult$b

# Visualize priors
p_grants_adult <- rgamma(1000, shape = a_adult, rate = b_adult)
hist(p_grants_adult)
# Overlay actual data onto histogram
hist(
  fr_adults_kids[
    fr_adults_kids$age_group == "Adult",
  ]$visas_granted,
  add = TRUE,
  col = rgb(1, 0, 0, 0.5)
)

n = 10000 ## create 10 quarters of data
ts = (1:n) / 4 ## quarterly times of the data
beta = 1

## Sim data
ps = b_adult / (b_adult + beta^ts) ## R doesn't like "b", it wants a p or a mu (See later)

xs = rnbinom(n, a_adult, ps)
xs2 = rnbinom(n, a_adult, b_adult / (b_adult + 1.02))
xs3 = rnbinom(n, a_adult, b_adult / (b_adult + 1.04))

median(xs)

# Histogram of simulated data
hist(xs)

# Overlay actual data onto histogram
hist(
  fr_adults_kids_testing[
    fr_adults_kids_testing$age_group == "Adult",
  ]$visas_granted,
  add = TRUE,
  col = rgb(1, 0, 0, 0.5)
)

xs = xs / 3 # only want September for first quarter
draws_adult_total <- xs + xs2 + xs3

rethinking::HPDI(draws_adult_total, prob = 0.95)
