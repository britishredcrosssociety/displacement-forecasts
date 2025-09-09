library(tidyverse)
library(asylum)

decisions <- fetch_decisions()
fr <- fetch_reunion()

grants <- decisions |> 
  filter(`Applicant type` == "Main applicant") |> 
  filter(`Case outcome` %in% c("Refugee Permission", "Humanitarian Protection")) |> 
  # mutate(Refugee = case_when(
  #   `Case outcome` %in% c("Refugee Permission", "Humanitarian Protection")
  # ))
  group_by(Date) |> 
  summarise(Sponsors = sum(Decisions)) |> 
  ungroup()

grants |> 
  ggplot(aes(x = Date, y = Sponsors)) +
  geom_line()

grants_annual <- 
  grants |> 
  rolling_year_totals(Date, Sponsors)

grants_annual |>
  ggplot(aes(x = Date, y = year_total)) +
  geom_line()

# How many grants per month in recent years?
grants_annual <- grants_annual |> 
  mutate(
    grants_per_month_from_quarterly = Sponsors / 3,
    grants_per_month_from_annual = year_total / 12,
  )

# Over the last five years, how many people were granted status in each quarter?
grants |> 
  filter(year(Date) >= 2020) |> 
  mutate(
    Quarter = quarter(Date),
    Year = year(Date)
  ) |> 
  
  ggplot(aes(x = Quarter, y = Sponsors, group = Year)) +
  geom_line(aes(colour = factor(Year)))

grants |> 
  filter(Date >= as.Date("2015-01-01")) |> 
  ggplot(aes(x = Date, y = grants_per_months)) +
  geom_line() +
  labs(
    title = "Grants of asylum to main applicants in the UK",
    subtitle = "Monthly average over the previous three months",
    x = "Date",
    y = "Grants per month"
  ) +
  theme_minimal()

# What proportion of FR visas are adults vs children?
fr_adults_kids <- fr |> 
  mutate(Age_Group = if_else(Age == "Under 18", "Child", "Adult")) |> 
  group_by(Date, Age_Group) |> 
  summarise(Visas = sum(`Visas granted`)) |>
  ungroup() |> 
  group_by(Date) |> 
  mutate(prop = Visas / sum(Visas)) |> 
  ungroup()

fr_adults_kids |> 
  filter(year(Date) >= 2022) |> 
  ggplot(aes(x = Date, y = prop, fill = Age_Group)) +
  geom_area() +
  labs(
    title = "Proportion of FR visas granted to adults vs children",
    x = "Date",
    y = "Proportion"
  ) +
  theme_minimal()

# Average proportion of adults and kids over the last two years


# What proportion of people granted status get FR visas the following quarter?
visas <- fr |> 
  group_by(Date) |> 
  summarise(Visas = sum(`Visas granted`)) |>
  ungroup()

grants_and_visas <- 
  grants |> 
  left_join(visas, by = "Date") |> 
  drop_na()

grants_and_visas <- grants_and_visas |> 
  mutate(
    families_reunited = Visas * 0.44,
    prop_visas = families_reunited / lead(Sponsors)
  )
