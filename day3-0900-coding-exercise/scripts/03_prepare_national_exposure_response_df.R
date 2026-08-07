# 03_prepare_national_exposure_response_df.R
# Build Mexico national monthly exposure–response panel:
# National incidence + national Google Trends + national climate (INLA lags).
#
# Usage (from repo root):
#   Rscript day3-0900-coding-exercise/scripts/03_prepare_national_exposure_response_df.R

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(lubridate)
})

source("day3-0900-coding-exercise/scripts/functions/lag_helpers.R")

#--------------- Load data

#----- Dengue incidence data (national)
global_national_incid <- read_csv("day3-0900-coding-exercise/data/National_incidence.csv")
mex_national_incid <- global_national_incid %>%
  dplyr::select(!adm_2_name) %>%
  filter(iso3 == "MEX")

#----- Climate data (Mexico national daily extracts)
daily_temp_mex <- read_csv("day3-0900-coding-exercise/data/daily_temp_mex_national.csv") %>%
  mutate(date = as.Date(date))
daily_ppt_mex <- read_csv("day3-0900-coding-exercise/data/daily_ppt_mex_national.csv") %>%
  mutate(date = as.Date(date))

#----- Google trends (national geo=MX; same filename strings as download script)
gt_dir <- "day3-0900-coding-exercise/downloads/2026-07-22"

load_gtrends_csvs <- function(pattern, gt_dir) {
  files <- list.files(gt_dir, pattern = pattern, full.names = TRUE)
  bind_rows(lapply(files, read_csv, show_col_types = FALSE))
}

# Filenames match sprintf("gtrends_%s_%s.csv", canon, "MX") from
# 01_download_gtrends_topics_national.R
gtrends_Aedes_aegypti <- load_gtrends_csvs("^gtrends_Aedes_aegypti_MX\\.csv$", gt_dir)
gtrends_dengue <- load_gtrends_csvs("^gtrends_dengue_MX\\.csv$", gt_dir)
gtrends_epidemic <- load_gtrends_csvs("^gtrends_epidemic_MX\\.csv$", gt_dir)
gtrends_fumigation <- load_gtrends_csvs("^gtrends_fumigation_MX\\.csv$", gt_dir)
gtrends_insecticide <- load_gtrends_csvs("^gtrends_insecticide_MX\\.csv$", gt_dir)
gtrends_mosquito <- load_gtrends_csvs("^gtrends_mosquito_MX\\.csv$", gt_dir)
gtrends_public_health <- load_gtrends_csvs("^gtrends_public_health_MX\\.csv$", gt_dir)
gtrends_source_management <- load_gtrends_csvs("^gtrends_source_management_MX\\.csv$", gt_dir)
gtrends_vector <- load_gtrends_csvs("^gtrends_vector_MX\\.csv$", gt_dir)

gtrends_raw <- bind_rows(
  gtrends_Aedes_aegypti,
  gtrends_dengue,
  gtrends_epidemic,
  gtrends_fumigation,
  gtrends_insecticide,
  gtrends_mosquito,
  gtrends_public_health,
  gtrends_source_management,
  gtrends_vector
) %>%
  mutate(date = as.Date(date))

#--------------- Process climate data

# Monthly mean temperature (matches INLA mean_temp_celsius)
monthly_temp <- daily_temp_mex %>%
  mutate(
    year = as.integer(year(date)),
    month = as.integer(month(date))
  ) %>%
  group_by(year, month) %>%
  summarise(
    mean_temp_celsius = mean(mean_temp_celsius, na.rm = TRUE),
    .groups = "drop"
  )

# Monthly total precipitation (matches INLA total_precip_mm)
monthly_ppt <- daily_ppt_mex %>%
  mutate(
    year = as.integer(year(date)),
    month = as.integer(month(date))
  ) %>%
  group_by(year, month) %>%
  summarise(
    total_precip_mm = sum(mean_precipitation_mm, na.rm = TRUE),
    .groups = "drop"
  )

climate_monthly <- monthly_temp %>%
  inner_join(monthly_ppt, by = c("year", "month"))

#--------------- Process gtrends data

gtrends_long <- gtrends_raw %>%
  mutate(
    year = as.integer(year(date)),
    month = as.integer(month(date)),
    term = as.character(topic_label),
    value = as.numeric(hits)
  ) %>%
  select(term, month, year, value) %>%
  arrange(term, year, month)

gtrends_wide <- gtrends_long %>%
  mutate(term = paste0("gt_", term)) %>%
  pivot_wider(
    id_cols = c(year, month),
    names_from = term,
    values_from = value
  )

gt_cols <- grep("^gt_", names(gtrends_wide), value = TRUE)

#--------------- Generate final df

counts_monthly <- mex_national_incid %>%
  transmute(
    iso3 = "MEX",
    year = as.integer(Year),
    month = as.integer(Month),
    monthly_cases = as.numeric(monthly_cases),
    monthly_cases_per_100000_pop = as.numeric(monthly_cases_per_100000_pop),
    population = as.numeric(population_interpolated),
    month_start = as.Date(sprintf("%04d-%02d-01", Year, Month))
  )

combined_df <- counts_monthly %>%
  left_join(climate_monthly, by = c("year", "month")) %>%
  left_join(gtrends_wide, by = c("year", "month")) %>%
  arrange(month_start)

# Design lags: climate 1:6; ARGO-style AR(1,12,24) for counts;
# Google Trends lags 1:3 months for each gt_* term.
# Single national series → group by iso3
combined_df <- add_group_lags(
  combined_df, "mean_temp_celsius", CLIMATE_LAGS,
  id_col = "iso3"
)
combined_df <- add_group_lags(
  combined_df, "total_precip_mm", CLIMATE_LAGS,
  id_col = "iso3"
)
combined_df <- add_group_lags(
  combined_df, "monthly_cases", AR_LAGS,
  id_col = "iso3"
)
for (gc in gt_cols) {
  combined_df <- add_group_lags(combined_df, gc, GT_LAGS, id_col = "iso3")
}

message(
  "Rows in combined_df ", nrow(combined_df),
  "  non-NA counts rows=", sum(!is.na(combined_df$monthly_cases)),
  "  non-NA climate rows=", sum(!is.na(combined_df$mean_temp_celsius) & !is.na(combined_df$total_precip_mm)),
  " | non-NA any gt row=", if (length(gt_cols)) {
    sum(rowSums(!is.na(combined_df[, gt_cols, drop = FALSE])) > 0)
  } else {
    0
  }
)

final_df <- combined_df %>%
  filter(if_all(all_of(gt_cols), ~ !is.na(.x)))

message(
  "Rows in final_df ", nrow(final_df),
  "  non-NA counts rows=", sum(!is.na(final_df$monthly_cases)),
  "  non-NA climate rows=", sum(!is.na(final_df$mean_temp_celsius) & !is.na(final_df$total_precip_mm)),
  " | non-NA any gt row=", if (length(gt_cols)) {
    sum(rowSums(!is.na(final_df[, gt_cols, drop = FALSE])) > 0)
  } else {
    0
  }
)

#--------------- Saving

write_csv(final_df, "day3-0900-coding-exercise/data/mx_national_exposure_response_monthly.csv")
