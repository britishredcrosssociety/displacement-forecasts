#--- note ----------------------------------------------------------------------

# This script is for forecasts post Q3 2024
# Updates for later dates might require code adjustments

#--- intro ---------------------------------------------------------------------

# The script creates the two types of CSVs needed for the Shiny app
# https://britishredcross.shinyapps.io/rs-destitution/
#
# 1) Historic and forecasted number of positive outcomes (granted status)
# 2) UK regional split (geographic distribution of asylum seekers in receipt of state support)

#--- libraries -----------------------------------------------------------------

library(tidyverse)
library(asylum)

#--- functions -----------------------------------------------------------------

# clean up data column names
clean_cols <- function(df) {
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- trimws(colnames(df))
  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("_/_", "_", colnames(df))
  df
}

# get the total number of applications for the most recent date
# returns 0 if most recent date is older than 2 years
# arguments:
#   1. dat - either "asylum application" or "awaiting decisions" dataset
#   2. nationalities - a vector of specifying what nationalities to include
get_applications <- function(dat, nationalities) {
  date_cutoff <- max(dat$date) %m-% months(24)
  dat <- dat[dat$nationality %in% nationalities, ]
  dat <- dat[dat$date == max(dat$date), ]
  ifelse(max(dat$date) < date_cutoff, 0, sum(dat$claims))
}

# Return the most recent number of applications and the maximum number of applications
# over the last 2 years
get_applications_range <- function(dat, nationalities) {
  date_cutoff <- max(dat$date) %m-% months(24)
  dat <- dat[dat$nationality %in% nationalities, ]
  dat <- dat[dat$date >= date_cutoff, ]
  
  # Calculate the most recent number of applications
  applications_min <- ifelse(max(dat$date) < date_cutoff, 0, sum(dat[dat$date == max(dat$date), ]$claims))
  
  # Calculate the maximum number of applications over the last two years
  applications_max <- ifelse(nrow(dat) == 0, 0, max(tapply(dat$claims, dat$date, sum)))
  
  return(data.frame(min = applications_min, max = applications_max))
}

# project upper and lower number of made decisions
# arguments:
#   1. dat - "decisions" dataset
#   2. nationalities - a vector of specifying what nationalities to include
#   3. backlog - total backlog (as returned by get_applications function)
#   3. backlog - total applications (as returned by get_applications function)
# NOTE: projections post Q3 2024 based on total backlog and takes into account new applications
project_decisions <- function(dat, nationalities, backlog, applications_range) {
  dat <- dat[dat$nationality %in% nationalities, ]
  dat <- dat[order(dat$date), ]
  
  # exclude resettlements and relocations
  # dat <- dat[!grepl("resettlement|relocation", tolower(dat$case_outcome)), ]
  # dat <- dat[!grepl("resettlement|relocation", tolower(dat$case_outcome_group)), ]
  
  # get total, positive, negative and withdrawal decision numbers for each quarter
  # TODO: move to asylum package
  pos_outcomes <- c(
    "Refugee Permission",
    "Humanitarian Protection"
    # "Temporary Refugee Permission",
    # "UASC Leave",
    # "Other Grants",
    # "Discretionary Leave"
  )
  neg_outcomes <- c(
    "3rd Country Refusal",
    "Certified Refusal",
    "Other Refusals"
  )
  wtd_outcomes <- c(
    "Explicit Withdrawal",
    "Implicit Withdrawal",
    "Other Withdrawal"
  )
  
  dat$outcome <- factor(NA, c("positive", "negative", "withdraw"))
  dat$outcome[dat$case_outcome %in% pos_outcomes] <- "positive"
  dat$outcome[dat$case_outcome %in% neg_outcomes] <- "negative"
  dat$outcome[dat$case_outcome %in% wtd_outcomes] <- "withdraw"
  
  dec <- tapply(dat$decisions, list(dat$date, dat$outcome), sum, default = 0)
  dec <- data.frame(date = as.Date(rownames(dec)), dec, total = rowSums(dec))
  
  # get decision type percentages based on the most recent date
  perc_pos <- tail(dec$positive / dec$total, 1)
  perc_neg <- tail(dec$negative / dec$total, 1)
  perc_wtd <- tail(dec$withdraw / dec$total, 1)
  
  if (is.nan(perc_pos)) {
    perc_pos <- 0
  }
  if (is.nan(perc_neg)) {
    perc_neg <- 0
  }
  if (is.nan(perc_wtd)) {
    perc_wtd <- 0
  }
  
  # initiate projections for next 3 quarters starting with numbers for current quarter
  dec$upper[nrow(dec)] <- dec$total[nrow(dec)]
  dec$lower[nrow(dec)] <- dec$total[nrow(dec)]
  dec$backlog_max[nrow(dec)] <- backlog
  dec$backlog_min[nrow(dec)] <- backlog
  
  dec[(nrow(dec) + 1):(nrow(dec) + 3), ] <- NA
  
  # get the number of maximum and minimum decisions observed recently
  # TODO: not sure how to justify the date
  recent_min <- min(dec$total[dec$date > "2023-08-01"], na.rm = TRUE)
  recent_max <- max(dec$total[dec$date > "2023-08-01"], na.rm = TRUE)
  
  # iterate over the future quarters and compute the results
  for (i in which(is.na(dec$date))) {
    dec$date[i] <- dec$date[i - 1] %m+% months(3)
    
    dec$upper[i] <- min(recent_max, max(dec$backlog_max[i - 1], 0))
    dec$lower[i] <- min(recent_min, max(dec$backlog_min[i - 1], 0))
    
    dec$backlog_min[i] <- dec$backlog_min[i - 1] - dec$lower[i] + applications_range$min
    dec$backlog_max[i] <- dec$backlog_max[i - 1] - dec$upper[i] + applications_range$max
  }
  
  # form the result: adjust the values by projected percent of positive, negative and withdraw decisions
  res <- data.frame(date = dec$date, nationality = nationalities)
  
  res$total_decisions <- dec$total
  res$total_decisions_upper <- dec$upper
  res$total_decisions_lower <- dec$lower
  
  res$positive_decisions <- dec$positive
  res$positive_decisions_upper <- dec$upper * perc_pos
  res$positive_decisions_lower <- dec$lower * perc_pos
  
  res$negative_decisions <- dec$negative
  res$negative_decisions_upper <- dec$upper * perc_neg
  res$negative_decisions_lower <- dec$lower * perc_neg
  
  res$withdraw_decisions <- dec$withdraw
  res$withdraw_decisions_upper <- dec$upper * perc_wtd
  res$withdraw_decisions_lower <- dec$lower * perc_wtd
  
  res
}

#--- process -------------------------------------------------------------------

# prepare datasets
dat_dec <- clean_cols(fetch_decisions()) # decisions
dat_app <- clean_cols(fetch_applications()) # asylum applications
dat_bck <- clean_cols(fetch_awaiting_decision()) # awaiting decisions (backlog)

# create a list of nationalities (using last 5 years)
# in order for nationality to be included it has to have at least 10 decisions
get_nationalities <- function(dat_dec) {
  nationalities_df <- dat_dec[
    dat_dec$nationality != "NA" & dat_dec$year > max(dat_dec$year - 5),
  ]
  nationalities_agg <- aggregate(
    decisions ~ nationality,
    FUN = sum,
    data = nationalities_df
  )
  nationalities_list <- nationalities_agg$nationality[nationalities_agg$decisions > 10]
  nationalities <- setNames(as.list(nationalities_list), tolower(nationalities_list))
  
  return(nationalities)
}

nationalities <- get_nationalities(dat_dec)

# TODO: not sure how to explain
dat_dec <- dat_dec[dat_dec$date >= ymd("2021.01.01"), ]

# iterate over all nationalities and project decisions
decisions = list()

for (n in names(nationalities)) {
  total_backlog <- get_applications(dat_bck, nationalities[[n]])
  total_applications <- get_applications_range(dat_app, nationalities[[n]])
  decisions[[n]] <- project_decisions(
    dat_dec,
    nationalities[[n]],
    total_backlog,
    total_applications
  )
}

# Aggregate all nationalities into a single dataframe
all_decisions <- do.call(rbind, decisions)

all_decisions <- all_decisions |> 
  # Fix date weirdness
  mutate(date = case_when(
    date == as.Date("2024-12-30") ~ as.Date("2024-12-31"),
    date == as.Date("2025-03-30") ~ as.Date("2025-03-31"),
    .default = date)) |>
  mutate(date = as.Date(date)) |> 

  group_by(date) |> 
  summarise(
    total_decisions = sum(total_decisions, na.rm = TRUE),
    total_decisions_lower = sum(total_decisions_lower, na.rm = TRUE),
    total_decisions_upper = sum(total_decisions_upper, na.rm = TRUE),
    
    # positive_decisions = sum(positive_decisions, na.rm = TRUE),
    positive_decisions_lower = sum(positive_decisions_lower, na.rm = TRUE),
    positive_decisions_upper = sum(positive_decisions_upper, na.rm = TRUE),
    
    # negative_decisions = sum(negative_decisions, na.rm = TRUE),
    negative_decisions_lower = sum(negative_decisions_lower, na.rm = TRUE),
    negative_decisions_upper = sum(negative_decisions_upper, na.rm = TRUE),
    
    # withdraw_decisions = sum(withdraw_decisions, na.rm = TRUE),
    withdraw_decisions_lower = sum(withdraw_decisions_lower, na.rm = TRUE),
    withdraw_decisions_upper = sum(withdraw_decisions_upper, na.rm = TRUE)
  )

#--- Family Reunion visas -----------------------------------------------------------------
fr <- clean_cols(fetch_reunion())

# What proportion of FR visas are adults vs children?
fr_adults_kids <- fr |> 
  mutate(age_group = if_else(age == "Under 18", "Child", "Adult")) |> 
  group_by(date, age_group) |> 
  summarise(visas_granted = sum(visas_granted)) |>
  ungroup() |> 
  group_by(date) |> 
  mutate(prop = visas_granted / sum(visas_granted)) |> 
  ungroup()

# Plot the proportion of FR visas granted to adults vs children since 2022
fr_adults_kids |> 
  filter(year(date) >= 2022) |> 
  ggplot(aes(x = date, y = prop, fill = age_group)) +
  geom_area() +
  labs(
    title = "Proportion of FR visas granted to adults vs children",
    x = "Date",
    y = "Proportion"
  ) +
  theme_minimal()

# Average proportion of adults and children over the last two years
fr_adults_kids_summary <- fr_adults_kids |> 
  filter(date >= max(date) - dmonths(24)) |> 
  group_by(age_group) |> 
  summarise(
    avg_prop = mean(prop),
    sd_prop = sd(prop)
  ) |> 
  ungroup()

prop_families_granted_visas <- fr_adults_kids_summary$avg_prop[fr_adults_kids_summary$age_group == "Adult"]
prop_children_granted_visas <- fr_adults_kids_summary$avg_prop[fr_adults_kids_summary$age_group == "Child"]
children_per_family <- prop_children_granted_visas / prop_families_granted_visas

# What proportion of people granted status get FR visas the following quarter?
# On average, people apply for Family Reunion 4 months after being granted asylum
visas <- fr |> 
  group_by(date) |> 
  summarise(visas_granted = sum(visas_granted)) |>
  ungroup()

grants_and_visas <- 
  dat_dec |> 
  
  # Calculate number of grants to main applicants who become eligible for FR
  filter(applicant_type == "Main applicant") |> 
  filter(case_outcome %in% c("Refugee Permission", "Humanitarian Protection")) |> 
  group_by(date) |> 
  summarise(potential_sponsors = sum(decisions)) |> 
  ungroup() |> 
  
  # Merge visa data
  left_join(visas, by = "date") |> 
  drop_na() |> 

  # Calculate number of families reunited and proportion of potential sponsors who successfully apply for visas
  # To translate individual visas issued into families (i.e. sponsor-households), 
  # assume that arriving adults are essentially the spouses/partners; 
  # their share (~44%) is therefore a good proxy for the number of family units reunited
  mutate(
    families_reunited = visas_granted * prop_families_granted_visas,
    prop_sponsors_reunited = families_reunited / lag(potential_sponsors),
    children_per_potential_sponsor = (visas_granted * prop_children_granted_visas) / lag(potential_sponsors)
  )

# Plot trends in proportion of people granted asylum who reunite with family
grants_and_visas |> 
  ggplot(aes(x = date, y = prop_sponsors_reunited)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of people granted asylum who reunite with family",
    subtitle = "Proportion of main applicants granted asylum who are later granted FR visas",
    x = "Date",
    y = "Proportion"
  ) +
  theme_minimal()

# Use the latest proportion for forecasts
prop_sponsors_reunited <- 
  grants_and_visas |> 
  tail(1) |> 
  pull(prop_sponsors_reunited)

prop_children_per_sponsor <- 
  grants_and_visas |> 
  tail(1) |> 
  pull(children_per_potential_sponsor)

#--- Family Reunion visas -----------------------------------------------------------------
# Forecast number of families who would've been reunited over the next 9 months, 
# based on projected positive decisions
fr_forecast <- all_decisions |> 
  # filter(date >= as.Date("2024-10-01")) |> 
  tail(3) |> 
  mutate(
    families_reunited_lower = positive_decisions_lower * prop_sponsors_reunited,
    families_reunited_upper = positive_decisions_upper * prop_sponsors_reunited,
    
    children_reunited_lower = positive_decisions_lower * prop_children_per_sponsor,
    children_reunited_upper = positive_decisions_upper * prop_children_per_sponsor,
  ) |> 
  select(
    date,
    families_reunited_lower,
    families_reunited_upper,
    children_reunited_lower,
    children_reunited_upper
  )

# Refugee Family Reunion was suspended at the beginning of September, so only take one month for that quarter
fr_forecast |> 
  mutate(
    families_reunited_lower = if_else(date == as.Date("2025-09-30"), families_reunited_lower / 3, families_reunited_lower),
    families_reunited_upper = if_else(date == as.Date("2025-09-30"), families_reunited_upper / 3, families_reunited_upper),
    children_reunited_lower = if_else(date == as.Date("2025-09-30"), children_reunited_lower / 3, children_reunited_lower),
    children_reunited_upper = if_else(date == as.Date("2025-09-30"), children_reunited_upper / 3, children_reunited_upper)
  ) |> 
  
  summarise(
    families_reunited_lower = sum(families_reunited_lower),
    families_reunited_upper = sum(families_reunited_upper),
    children_reunited_lower = sum(children_reunited_lower),
    children_reunited_upper = sum(children_reunited_upper)
  )
