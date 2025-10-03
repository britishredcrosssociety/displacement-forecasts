library(tidyverse)
library(plotly)
library(asylum)

# clean up data column names
clean_cols <- function(df) {
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- trimws(colnames(df))
  colnames(df) <- gsub(" ", "_", colnames(df))
  colnames(df) <- gsub("_/_", "_", colnames(df))
  df
}

fr <- clean_cols(fetch_reunion())

fr_summary_all <- fr |>
  group_by(date) |>
  summarise(visas_granted = sum(visas_granted)) |>
  ungroup()

# Look for when the step change in visa grants began
fr_summary_all |>
  ggplot(aes(x = date, y = visas_granted)) +
  geom_line()

# Calculate quarter-on-quarter % change
fr_summary_all |>
  mutate(
    delta = (visas_granted - lag(visas_granted)) / lag(visas_granted),
    delta_label = scales::percent(delta)
  ) |>
  filter(delta < 1) |>
  ggplot(aes(x = date, y = delta, text = delta_label, group = 1)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent)

ggplotly()

fr_summary_all |>
  rolling_year_totals(date, visas_granted) |>
  mutate(
    delta = (year_total - lag(year_total)) / lag(year_total),
    delta_label = scales::percent(delta)
  ) |>
  filter(delta < 1) |>
  ggplot(aes(x = date, y = delta, text = delta_label, group = 1)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent)

ggplotly()

# How correlated are visas for adults and children?
fr_adults_kids_all <- fr |>
  mutate(age_group = if_else(age == "Under 18", "Child", "Adult")) |>
  group_by(date, age_group) |>
  summarise(visas_granted = sum(visas_granted)) |>
  ungroup() |>
  group_by(date) |>
  mutate(prop = visas_granted / sum(visas_granted)) |>
  ungroup()

fr_adults_kids_all |>
  ggplot(aes(
    x = date,
    y = visas_granted,
    colour = age_group,
    group = age_group
  )) +
  geom_line() +
  theme_minimal()
