# Code adapted from rs-destitution/data-raw/create_csvs_granted_statusQ32024.r
# Filtered to only include main applicant, non UASC - L 168-175

#--- libraries -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(asylum) # contains prepared data tables
  library(lubridate) # helper functions for working with dates
})


#--- parameters ----------------------------------------------------------------

output_path <- "move-on-destitution/granted-status-data"


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

# dat <- dat_app
# nationalities <- nationalities[["afghanistan"]]

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
    "Humanitarian Protection",
    "Temporary Refugee Permission",
    "UASC Leave",
    "Other Grants",
    "Discretionary Leave"
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
  
  # initiate projections for next 5 quarters starting with numbers for current quarter
  dec$upper[nrow(dec)] <- dec$total[nrow(dec)]
  dec$lower[nrow(dec)] <- dec$total[nrow(dec)]
  dec$backlog_max[nrow(dec)] <- backlog
  dec$backlog_min[nrow(dec)] <- backlog
  
  dec[(nrow(dec) + 1):(nrow(dec) + 5), ] <- NA
  
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
  res <- data.frame(date = dec$date)
  
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
# filter for single adults and UASC for each dataset
dat_dec <- clean_cols(fetch_decisions()) |>
  filter(applicant_type %in% c("Main applicant") & age != "Under 18" & uasc %in% c("Non-UASC")) # decisions

dat_app <- clean_cols(fetch_applications()) |>
  filter(applicant_type %in% c("Main applicant") & age != "Under 18" & uasc %in% c("Non-UASC")) # asylum applications

dat_bck <- clean_cols(fetch_awaiting_decision()) |>
  filter(applicant_type %in% c("Main applicant"))# awaiting decisions (backlog)

# Apply UASC proportion from YTD to data_bck (backlog) as not possible to filter in data
# Apply proportion when f'n is called below
uasc_proportion <- clean_cols(fetch_applications()) |>
  filter(applicant_type %in% c("Main applicant") & !uasc %in% c("Total (pre-2006)") & year == 2025) |>
  group_by(uasc) |>
  summarise(claims = sum(claims, na.rm = TRUE), .groups = "drop") |>
  mutate(proportion = claims / sum(claims)) |>
  filter(uasc == "Non-UASC") |>
  pull(proportion)

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
  
  # add necessary nationality groups
  nationalities[["all nationalities"]] <- unique(dat_dec$nationality)
  nationalities[["all sap countries"]] <- c(
    "Afghanistan",
    "Eritrea",
    "Libya",
    "Syria",
    "Yemen",
    "Iran",
    "Iraq",
    "Sudan"
  )
  
  return(nationalities)
}

nationalities <- get_nationalities(dat_dec)

# TODO: not sure how to explain
dat_dec <- dat_dec[dat_dec$date >= ymd("2021.01.01"), ]

# delete any previous .csv output files
# TODO: maybe add a message displaying the files that were removed
invisible(file.remove(list.files(
  output_path,
  pattern = ".csv$",
  full.names = TRUE
)))

# iterate over all nationalities and save results .csv output files
for (n in names(nationalities)) {
  total_backlog <- get_applications(dat_bck, nationalities[[n]]) * uasc_proportion
  total_applications <- get_applications_range(dat_app, nationalities[[n]])
  decisions <- project_decisions(
    dat_dec,
    nationalities[[n]],
    total_backlog,
    total_applications
  )
  
  # only produce the output if the number of positive decisions for the current quarter is above 10
  if (tail(na.omit(decisions$positive_decisions), 1) > 10) {
    write.csv(
      decisions,
      file.path(output_path, paste0("positive_projected_single_adult_", n, ".csv")),
      row.names = FALSE
    )
    
  }
}
