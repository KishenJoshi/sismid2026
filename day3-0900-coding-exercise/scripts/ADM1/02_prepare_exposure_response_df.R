# 02_prepare_exposure_response_df.R
# Build Mexico Admin1 monthly exposure–response panel:
# ADM1 incidence + Google Trends topics + climate (INLA lags).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(data.table)
  library(tibble)
  library(lubridate)
})

source("day3-0900-coding-exercise/scripts/functions/lag_helpers.R")

#--------------- Load data

#----- Dengue incidence data
global_adm1_incid <- read_csv("day3-0900-coding-exercise/data/ADM1_incidence.csv")
mex_adm1_incid <- global_adm1_incid %>%
  dplyr::select(!adm_2_name) %>%
  filter(iso3 == "MEX")

#----- Climate data
daily_temp <- read_csv("day3-0900-coding-exercise/data/daily_temp.csv")
daily_ppt <- read_csv("day3-0900-coding-exercise/data/daily_precipitation.csv")

#----- Google trends (topics with data for all 32 states)
gt_dir <- "day3-0900-coding-exercise/downloads/2026-07-22"

load_gtrends_csvs <- function(pattern, gt_dir) {
  files <- list.files(gt_dir, pattern = pattern, full.names = TRUE)
  bind_rows(lapply(files, read_csv, show_col_types = FALSE))
}

gtrends_Aedes_aegypti <- load_gtrends_csvs("^gtrends_Aedes_aegypti_MX_.*\\.csv$", gt_dir)
gtrends_dengue <- load_gtrends_csvs("^gtrends_dengue_MX_.*\\.csv$", gt_dir)
gtrends_epidemic <- load_gtrends_csvs("^gtrends_epidemic_MX_.*\\.csv$", gt_dir)
gtrends_fumigation <- load_gtrends_csvs("^gtrends_fumigation_MX_.*\\.csv$", gt_dir)
gtrends_insecticide <- load_gtrends_csvs("^gtrends_insecticide_MX_.*\\.csv$", gt_dir)
gtrends_mosquito <- load_gtrends_csvs("^gtrends_mosquito_MX_.*\\.csv$", gt_dir)
gtrends_public_health <- load_gtrends_csvs("^gtrends_public_health_MX_.*\\.csv$", gt_dir)
gtrends_source_management <- load_gtrends_csvs("^gtrends_source_management_MX_.*\\.csv$", gt_dir)
gtrends_vector <- load_gtrends_csvs("^gtrends_vector_MX_.*\\.csv$", gt_dir)

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

daily_temp_mex <- daily_temp %>%
  filter(iso3 == "MEX", !is.na(rne_iso_code), rne_iso_code != "") %>%
  mutate(date = as.Date(date))

daily_ppt_mex <- daily_ppt %>%
  filter(iso3 == "MEX", !is.na(rne_iso_code), rne_iso_code != "") %>%
  mutate(date = as.Date(date))

# Monthly mean temperature (matches INLA mean_temp_celsius)
monthly_temp <- daily_temp_mex %>%
  mutate(
    year = as.integer(year(date)),
    month = as.integer(month(date))
  ) %>%
  group_by(rne_iso_code, year, month) %>%
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
  group_by(rne_iso_code, year, month) %>%
  summarise(
    total_precip_mm = sum(mean_precipitation_mm, na.rm = TRUE),
    .groups = "drop"
  )

climate_monthly <- monthly_temp %>%
  inner_join(monthly_ppt, by = c("rne_iso_code", "year", "month"))

#--------------- Process gtrends data

gtrends_long <- gtrends_raw %>%
  mutate(
    year = as.integer(year(date)),
    month = as.integer(month(date)),
    term = as.character(topic_label),
    state = as.character(geo),
    value = as.numeric(hits)
  ) %>%
  select(term, state, month, year, value) %>%
  arrange(term, state, year, month)

gtrends_wide <- gtrends_long %>%
  mutate(term = paste0("gt_", term)) %>%
  pivot_wider(
    id_cols = c(state, year, month),
    names_from = term,
    values_from = value
  ) %>%
  rename(rne_iso_code = state)

gt_cols <- grep("^gt_", names(gtrends_wide), value = TRUE)

#--------------- Generate final df

counts_monthly <- mex_adm1_incid %>%
  transmute(
    iso3 = "MEX",
    rne_iso_code = as.character(rne_iso_code),
    year = as.integer(Year),
    month = as.integer(Month),
    monthly_cases = as.numeric(monthly_cases),
    monthly_cases_per_100000_pop = as.numeric(monthly_cases_per_100000_pop),
    population = as.numeric(population_interpolated),
    month_start = as.Date(sprintf("%04d-%02d-01", Year, Month))
  )

combined_df <- counts_monthly %>%
  left_join(climate_monthly, by = c("rne_iso_code", "year", "month")) %>%
  left_join(gtrends_wide, by = c("rne_iso_code", "year", "month")) %>%
  arrange(rne_iso_code, month_start)

# INLA design lags: climate 1:6; ARGO-style AR(1,12,24) for counts and each gt_* term
combined_df <- add_group_lags(combined_df, "mean_temp_celsius", CLIMATE_LAGS)
combined_df <- add_group_lags(combined_df, "total_precip_mm", CLIMATE_LAGS)
combined_df <- add_group_lags(combined_df, "monthly_cases", AR_LAGS)
for (gc in gt_cols) {
  combined_df <- add_group_lags(combined_df, gc, AR_LAGS)
}

message(
  "Rows in combined_df ", nrow(combined_df),
  "  non-NA counts rows=", sum(!is.na(combined_df$monthly_cases)),
  "  non-NA climate rows=", sum(!is.na(combined_df$mean_temp_celsius) & !is.na(combined_df$total_precip_mm)),
  " | non-NA any gt row=", sum(rowSums(!is.na(combined_df[, gt_cols, drop = FALSE])) > 0)
)

final_df <- combined_df %>%
  filter(if_all(all_of(gt_cols), ~ !is.na(.x)))

message(
  "Rows in final_df ", nrow(final_df),
  "  non-NA counts rows=", sum(!is.na(final_df$monthly_cases)),
  "  non-NA climate rows=", sum(!is.na(final_df$mean_temp_celsius) & !is.na(final_df$total_precip_mm)),
  " | non-NA any gt row=", sum(rowSums(!is.na(final_df[, gt_cols, drop = FALSE])) > 0)
)

#--------------- Saving

write_csv(final_df, "day3-0900-coding-exercise/data/mx_adm1_exposure_response_monthly.csv")
