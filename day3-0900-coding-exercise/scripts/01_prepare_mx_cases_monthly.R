# 01_prepare_mx_cases_monthly.R
# Day-split OpenDengue MX Admin1 weekly cases to monthly totals; join GHS population.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(data.table)
  library(tibble)
})

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
source(file.path(root, "scripts/functions/paths.R"))
source(file.path(root, "scripts/functions/week_to_month.R"))

od_path <- file.path(
  DSD_ROOT,
  "data/dengue_counts/opendengue/V1.3/Spatial_extract_V1_3.csv"
)
pop_path <- file.path(
  DSD_ROOT,
  "results/02_05_generating_incidence_data/2026-04-15/ADM1_incidence.csv"
)

message("Reading OpenDengue: ", od_path)
od <- data.table::fread(
  od_path,
  select = c(
    "ISO_A0", "S_res", "T_res", "RNE_iso_code",
    "calendar_start_date", "calendar_end_date", "dengue_total"
  )
)
mx_week <- od[
  ISO_A0 == "MEX" & S_res == "Admin1" & T_res == "Week" &
    RNE_iso_code %in% MX_STATES_32
]
mx_week[, `:=`(
  calendar_start_date = as.Date(calendar_start_date),
  calendar_end_date = as.Date(calendar_end_date),
  rne_iso_code = as.character(RNE_iso_code),
  dengue_total = as.numeric(dengue_total)
)]

message("Day-splitting ", nrow(mx_week), " weekly rows to monthly...")
cases_monthly <- week_intervals_to_monthly(
  as.data.frame(mx_week),
  start_col = "calendar_start_date",
  end_col = "calendar_end_date",
  id_cols = "rne_iso_code",
  value_col = "dengue_total"
) %>%
  dplyr::rename(cases = dengue_total) %>%
  dplyr::mutate(cases = round(cases))

message("Reading population from seasonal-drivers ADM1 incidence...")
pop <- data.table::fread(
  pop_path,
  select = c("rne_iso_code", "Year", "Month", "population_interpolated")
)
pop <- pop[rne_iso_code %in% MX_STATES_32]
pop_df <- as.data.frame(pop) %>%
  dplyr::rename(year = Year, month = Month, population = population_interpolated)

panel <- cases_monthly %>%
  dplyr::left_join(pop_df, by = c("rne_iso_code", "year", "month"))

n_miss_pop <- sum(is.na(panel$population))
if (n_miss_pop > 0) {
  warning(n_miss_pop, " rows missing population; forward/back filling within state")
  panel <- panel %>%
    dplyr::group_by(rne_iso_code) %>%
    dplyr::arrange(month_start, .by_group = TRUE) %>%
    tidyr::fill(population, .direction = "downup") %>%
    dplyr::ungroup()
}

out_dir <- ensure_dir(DAY3_ROOT, "data")
out_path <- file.path(out_dir, "mx_adm1_cases_monthly.csv")
readr::write_csv(panel, out_path)

# Coverage / gap report (OpenDengue MX Admin1 weekly is not continuous)
coverage <- panel %>%
  dplyr::distinct(year, month, month_start) %>%
  dplyr::arrange(month_start)
full_grid <- tibble::tibble(
  month_start = seq.Date(min(coverage$month_start), max(coverage$month_start), by = "month")
)
gaps <- full_grid %>%
  dplyr::anti_join(coverage, by = "month_start")
readr::write_csv(gaps, file.path(out_dir, "mx_adm1_cases_month_gaps.csv"))
message(
  "Wrote ", out_path, " | states=", length(unique(panel$rne_iso_code)),
  " | rows=", nrow(panel),
  " | months=", paste(range(panel$month_start), collapse = " .. "),
  " | gap months=", nrow(gaps)
)
