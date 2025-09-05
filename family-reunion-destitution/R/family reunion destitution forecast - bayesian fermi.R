set.seed(2025)

library(tidyverse)
library(asylum)

# ---- Bayesian Fermi estimation ----
# Simulation size
n_sim <- 10000

# Sample from each distribution
# (Old, test values that included more drivers of destitution)
# p_housing_fail <- rbeta(n_sim, 75, 25)
# p_no_income    <- rbeta(n_sim, 90, 10)
# p_benefit_delay<- rbeta(n_sim, 80, 20)
# p_no_crisis    <- rbeta(n_sim, 10, 90)

# Combine into destitution probability
# p_destitute <- p_housing_fail * p_no_income * p_benefit_delay * p_no_crisis

p_housing_insecure <- rbeta(n_sim, 77, 25)
p_financial_insecure <- rbeta(n_sim, 96, 4)

# Simulate joint probability of destitution
p_destitute <- p_housing_insecure * p_financial_insecure

# Summarise results
summary_stats <- tibble(
  mean = mean(p_destitute),
  median = median(p_destitute),
  ci_95_low = quantile(p_destitute, 0.025),
  ci_95_high = quantile(p_destitute, 0.975)
)

print(summary_stats)

# Visualise probability distribution
ggplot(data.frame(p_destitute), aes(x = p_destitute)) +
  geom_histogram(bins = 50, fill = "grey", color = "black") +
  labs(title = "Estimated Probability of Destitution",
       x = "P(destitute)", y = "Frequency") +
  theme_minimal()

# ---- What proportion of arrivals might be children? ----
# Based on family reunion visa data
fr <- fetch_reunion()

# Trends in %s of children granted family reunion visas
fr_children <- 
  fr |> 
  group_by(Date) |> 
  summarise(
    total = sum(`Visas granted`),
    children = sum(`Visas granted`[Age == "Under 18"])
  ) |> 
  ungroup() |> 
  mutate(p_children = children / total)

# Plot trends
fr_children |> 
  ggplot(aes(x = Date, y = p_children)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(title = "Proportion of Family Reunion Visas Granted to Children",
       x = "Date", y = "Proportion") +
  theme_minimal()

# What's the distribution/IQR of %s of children since 2022 (i.e. post-pandemic)?
p_children <- fr_children |> filter(Date >= as.Date("2022-01-01")) |> pull(p_children)
summary(p_children)

fr_children |> 
  filter(Date >= as.Date("2022-01-01")) |> 
  ggplot(aes(x = p_children)) +
  geom_histogram(binwidth = 0.01, fill = "lightgreen", color = "black") +
  scale_x_continuous(limits = c(0, 1))

quantile(p_children, 0.25)
quantile(p_children, 0.75)

# ---- Functions for simulating number of destitute arrivals ----
forecast_destitution <- function(n_arrivals, probabilities, n_sim = 10000) {
  rbinom(n_sim, size = n_arrivals, prob = probabilities)
}

summarise_forecast <- function(sim) {
  # Summarise results
  tibble(
    mean = mean(sim),
    median = median(sim),
    ci_95_low = quantile(sim, 0.025),
    ci_95_high = quantile(sim, 0.975)
  )
}

# Plot simulation
plot_forecast <- function(sim) {
  sim_summary <- summarise_forecast(sim)
  
  ggplot(data.frame(sim), aes(x = sim)) +
    geom_histogram(bins = 50, fill = "skyblue", color = "black") +
    labs(title = "Expected Destitute Arrivals (next year)",
         x = "Number of destitute arrivals", y = "Frequency") +
    theme_minimal()
}

# ---- Scenario 1: Home Office family reunion decision makers continue to all focus on remaining cases ----
# There were ~10,000 applications for visas on the backlog as of early September 2025.
# We expected there to be a few hundred more applications before the system was suspended.
# The grant rate is ~95%, so we'll say 10,000 visas will be granted in the coming months.
n_arrivals <- 10000

sim_scenario_1 <- forecast_destitution(n_arrivals, p_destitute, n_sim)

plot_forecast(sim_scenario_1)

# Use the figures from here in our write-up
(destitute_summary_1 <- summarise_forecast(sim_scenario_1))

# How many of these might be children?
summarise_forecast(sim_scenario_1 * quantile(p_children, 0.25))  # use the lower 95%ile for a conservative estimate
summarise_forecast(sim_scenario_1 * quantile(p_children, 0.75))  # use the upper 95%ile for the higher estimate

# How many people might be housing insecure?
sim_scenario_1_housing <- forecast_destitution(n_arrivals, p_housing_insecure, n_sim)
summarise_forecast(sim_scenario_1_housing)

# ---- Scenario 2: Decision making continues at current rate  ----
# Assuming 5,000 visa grants and arrivals per quarter between now and March 2026
n_arrivals <- 5000

sim_scenario_2 <- forecast_destitution(n_arrivals, p_destitute, n_sim)

plot_forecast(sim_scenario_2)

# Use the figures from here in our write-up
(destitute_summary_2 <- summarise_forecast(sim_scenario_2))

# How many of these might be children?
summarise_forecast(sim_scenario_2 * quantile(p_children, 0.25))
summarise_forecast(sim_scenario_2 * quantile(p_children, 0.75))
