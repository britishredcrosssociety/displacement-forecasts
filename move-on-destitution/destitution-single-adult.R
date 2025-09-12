# How many single adults are at risk of destitution?

library(tidyverse)
library(asylum)
library(brcplot)

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
csv_files <- csv_files[!grepl("all nationalities|all sap", csv_files)]

# Read and sum all CSVs
all_nationalities <- csv_files |>
  lapply(read_csv, show_col_types = FALSE) |>
  bind_rows() |>
  group_by(date) |>
  summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop")

# ---- 3) Calculate % destitute ----
# Independent Chief Inspector of Borders and Immigration estimates 47% of legacy
# backlog claims were in receipt of asylum support

# Use figures from table below for write up
single_adult_destitute <- all_nationalities |>
  mutate(
    destitute_total = total_decisions * 0.47,
    destitute_total_upper = total_decisions_upper * 0.47,
    destitute_total_lower = total_decisions_lower * 0.47
  ) |>
  select(1, 14, 15, 16) |>
  mutate(
    destitute_total = if_else(destitute_total == 0, NA_real_, destitute_total),
    destitute_total_upper = if_else(
      destitute_total_upper == 0,
      NA_real_,
      destitute_total_upper
    ),
    destitute_total_lower = if_else(
      destitute_total_lower == 0,
      NA_real_,
      destitute_total_lower
    )
  ) |>
  slice(-22:-23) # Only look 3 Qs ahead as accuracy reducing into the future

# Plot
single_adult_destitute <- single_adult_destitute |>
  mutate(quarter = paste0(year(date), " Q", quarter(date)))

ggplot(single_adult_destitute, aes(x = quarter, group = 1)) +
  # Main line and points for historic numbers
  geom_line(
    aes(y = destitute_total, color = "Estimated historic risk"),
    size = 1
  ) +
  geom_point(
    aes(y = destitute_total, color = "Estimated historic risk"),
    size = 2
  ) +

  # Ribbon with projection range
  geom_ribbon(
    aes(
      ymin = destitute_total_lower,
      ymax = destitute_total_upper,
      fill = "Projection range"
    ),
    alpha = 0.3
  ) +

  # Dashed upper/lower bounds
  geom_line(
    aes(y = destitute_total_upper),
    linetype = "dashed",
    color = "black",
    show.legend = FALSE
  ) +
  geom_line(
    aes(y = destitute_total_lower),
    linetype = "dashed",
    color = "black",
    show.legend = FALSE
  ) +

  # Colors and fills
  scale_color_manual(values = c("Estimated historic risk" = "black")) +
  scale_fill_manual(values = c("Projection range" = "grey")) +
  scale_y_continuous(labels = scales::comma) +

  # Labels
  labs(
    title = "Newly granted single adult refugees at risk of destitution",
    subtitle = "Number of single adults at risk each quarter",
    x = "",
    y = "Number of people",
    color = NULL, # removes legend title for lines
    fill = NULL # removes legend title for ribbon
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title.position = "plot"
  )

ggsave(
  "move-on-destitution/projection.png",
  width = 150,
  height = 100,
  units = "mm"
)

# ---- Sense check ----
# Aggregated nationalities and filtered all nationalities roughly the same?
single_adult <- read_csv(
  "move-on-destitution/granted-status-data/positive_projected_single_adult_all nationalities.csv"
) |>
  select(1, 2, 3, 4) |>
  slice(-22:-23)

single_adult_agg <- all_nationalities |> slice(-22:-23)

compare <- tibble(
  date = single_adult_agg$date,
  single_adult_difference_perc = (single_adult$total_decisions -
    single_adult_agg$total_decisions) /
    single_adult_agg$total_decisions *
    100
)

# Num of single adult < all people?
# need to regenerate this link each time as its a private repo
all_people <- read_csv(
  "https://raw.githubusercontent.com/britishredcrosssociety/rs-destitution/refs/heads/main/data/positive_projected_all%20nationalities.csv?token=GHSAT0AAAAAADGEAMOIQ7UQCSVT7SNWQLM42GCWNUA"
) |>
  select(1, 2, 3, 4) |>
  slice(-22:-23)

compare <- compare |>
  mutate(
    all_people_single_adult_diff = all_people$total_decisions -
      single_adult_agg$total_decisions
  )

# Perc of single adults from all decisions made in last year?
single_adult_share <- fetch_decisions() |>
  filter(Year == 2025) |>
  summarise(
    single_adults = sum(Decisions[
      `Applicant type` == "Main applicant" & UASC == "Non-UASC"
    ]),
    total = sum(Decisions)
  ) |>
  mutate(share = single_adults / total) |>
  pull(share)

single_adult_share

compare <- compare |>
  mutate(
    all_destitute_total = all_people$total_decisions * 0.47,
    single_adult_destitute = single_adult_destitute$destitute_total
  )

# 59% vs 73% - could be that we are using the pre-filtered all nationalities rather than adding them up?
mean(compare$single_adult_destitute / compare$all_destitute_total, na.rm = TRUE)
