# 02_prepare_mx_climate_monthly.R
# Aggregate daily ERA5 MX adm1 extracts to monthly mean temperature and total precip.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(data.table)
})

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
source(file.path(root, "scripts/functions/paths.R"))

clim_dir <- file.path(
  DSD_ROOT,
  "results/02_09_extract_climate_data/2026-04-18"
)
temp_path <- file.path(clim_dir, "daily_temp.csv")
ppt_path <- file.path(clim_dir, "daily_precipitation.csv")

message("Reading daily temperature...")
temp <- data.table::fread(
  temp_path,
  select = c("date", "rne_iso_code", "mean_temp_celsius")
)
temp <- temp[grepl("^MX-", rne_iso_code) & rne_iso_code %in% MX_STATES_32]
temp[, `:=`(
  date = as.Date(date),
  year = as.integer(format(date, "%Y")),
  month = as.integer(format(date, "%m"))
)]
temp_m <- temp[, .(
  mean_temp_celsius = mean(mean_temp_celsius, na.rm = TRUE)
), by = .(rne_iso_code, year, month)]

message("Reading daily precipitation...")
ppt <- data.table::fread(
  ppt_path,
  select = c("date", "rne_iso_code", "mean_precipitation_mm")
)
ppt <- ppt[grepl("^MX-", rne_iso_code) & rne_iso_code %in% MX_STATES_32]
ppt[, `:=`(
  date = as.Date(date),
  year = as.integer(format(date, "%Y")),
  month = as.integer(format(date, "%m"))
)]
ppt_m <- ppt[, .(
  total_precip_mm = sum(mean_precipitation_mm, na.rm = TRUE)
), by = .(rne_iso_code, year, month)]

climate <- merge(temp_m, ppt_m, by = c("rne_iso_code", "year", "month"), all = TRUE)
climate[, month_start := as.Date(sprintf("%04d-%02d-01", year, month))]
climate <- climate[order(rne_iso_code, month_start)]

out_dir <- ensure_dir(DAY3_ROOT, "data")
out_path <- file.path(out_dir, "mx_adm1_climate_monthly.csv")
readr::write_csv(as.data.frame(climate), out_path)
message(
  "Wrote ", out_path,
  " | states=", uniqueN(climate$rne_iso_code),
  " | rows=", nrow(climate),
  " | months=", paste(range(climate$month_start), collapse = " .. ")
)
