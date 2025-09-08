# How many single adults are at risk of destitution?

library(tidyverse)

# ---- 1) How many projected positive decisions will be made for single adults? ----
# Run script that creates nationality-specific csvs of positive decisions filtered
# for single adults
source(file.path("move-on-destitution", "granted-status-single-adult.R"))

# ---- 2) Aggregate all nationalities by summing projections from each nationality ----
csv_files <- list.files(
  path = "move-on-destitution/granted-status-data",
  pattern = "^positive_projected_single_adult_.*\\.csv$",
  full.names = TRUE
)
# Exclude "all_nationalities" and SAP countries
csv_files <- csv_files[!grepl("all", csv_files)]

# Read and sum all CSVs
all_nationalities <- csv_files |>
  lapply(read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  group_by(date) |>
  summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop")

# ---- 3) Calculate % destitute ----
# Independent Chief Inspector of Borders and Immigration estimates 47% of legacy
# backlog claims were in receipt of asylum support

all_destitute <- all_nationalities |>
  mutate(destitute_total = total_decisions * 0.47,
    destitute_positive_upper = positive_decisions_upper * 0.47,
         destitute_positive_lower = positive_decisions_lower * 0.47) |>
  select(1, 15, 16)

# ---- Sense check ----
# Perc of single adults from all decisions made in last year?

single_adult_share <- fetch_decisions() |>
  filter(Year == 2025) |>
  summarise(
    single_adults = sum(Decisions[`Applicant type` == "Main applicant" & UASC == "Non-UASC"]),
    total = sum(Decisions)
  ) |>
  mutate(share = single_adults / total) |>
  pull(share)

single_adult_share

all_people <- read_csv("https://raw.githubusercontent.com/britishredcrosssociety/rs-destitution/refs/heads/main/data/positive_projected_all%20nationalities.csv?token=GHSAT0AAAAAADGEAMOJSGK3NA2MWLPYX3IU2F6764A")

all_destitute$destitute_positive_upper / (all_people$positive_decisions_upper * 0.47)
