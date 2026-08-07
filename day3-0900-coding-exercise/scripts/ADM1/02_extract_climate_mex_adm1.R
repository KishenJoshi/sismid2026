# 02_extract_climate_mex_adm1.R
# Extract daily ERA5 climate for Mexico ADM1 units (pop-weighted).
# Uses the same Mexico-cropped yearly GeoTIFF cache as the national script
# (bbox + 2 ERA5 pop-cell buffer). Years = National_incidence MEX calendar years.
#
# Usage (from repo root):
#   Rscript day3-0900-coding-exercise/scripts/ADM1/02_extract_climate_mex_adm1.R

suppressPackageStartupMessages({
  library(rnaturalearth)
  library(dplyr)
  library(sf)
  library(terra)
  library(exactextractr)
  library(furrr)
  library(future)
  library(tibble)
  library(readr)
  library(ncdf4)
  library(purrr)
})

source("day3-0900-coding-exercise/scripts/functions/extract_climate_data_functions.R")

plan(multisession, workers = 3)

#--------------- Paths

era5_temp_dir <- "C:/Users/Har-KishenJoshi/Documents/LSHTM/Sync_Independent/Project/dengue_seasonal_drivers/data/era5_daily/daily_temperature_mean"
era5_ppt_dir <- "C:/Users/Har-KishenJoshi/Documents/LSHTM/Sync_Independent/Project/dengue_seasonal_drivers/data/era5_daily/daily_precipitation"
pop_raster_path <- "day3-0900-coding-exercise/data/at_risk_denv_pop_era5.tif"
national_incidence_path <- "day3-0900-coding-exercise/data/National_incidence.csv"
adm1_incidence_path <- "day3-0900-coding-exercise/data/ADM1_incidence.csv"
crop_root <- "day3-0900-coding-exercise/data/era5_mex_cropped"

#--------------- Mexico national bbox shapefile + ADM1 polygons

mexico_sf <- rnaturalearth::ne_countries(scale = 50, type = "map_units", returnclass = "sf") %>%
  dplyr::select(iso_a3_eh, geometry) %>%
  dplyr::rename(iso3 = iso_a3_eh) %>%
  dplyr::filter(iso3 == "MEX")

stopifnot(nrow(mexico_sf) == 1L)

adm1_codes <- readr::read_csv(adm1_incidence_path, show_col_types = FALSE) %>%
  dplyr::filter(.data$iso3 == "MEX", !is.na(.data$rne_iso_code), .data$rne_iso_code != "") %>%
  dplyr::distinct(.data$rne_iso_code) %>%
  dplyr::pull(.data$rne_iso_code)

adm1_sf <- rnaturalearth::ne_states(iso_a2 = "MX", returnclass = "sf") %>%
  dplyr::select(iso_3166_2, geometry) %>%
  dplyr::rename(rne_iso_code = iso_3166_2) %>%
  dplyr::filter(.data$rne_iso_code %in% adm1_codes) %>%
  dplyr::group_by(.data$rne_iso_code) %>%
  dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") %>%
  dplyr::arrange(.data$rne_iso_code) %>%
  dplyr::mutate(location_id = dplyr::row_number())

missing <- setdiff(adm1_codes, adm1_sf$rne_iso_code)
if (length(missing)) {
  warning("Missing NE polygons for: ", paste(missing, collapse = ", "))
}
stopifnot(nrow(adm1_sf) > 0L)

location_lookup <- adm1_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(location_id, rne_iso_code)

#--------------- Years + Mexico-cropped cache (shared with national)

target_years <- mex_national_incidence_years(national_incidence_path)

mex_crops <- ensure_mex_era5_crops(
  mexico_sf = mexico_sf,
  pop_raster_path = pop_raster_path,
  era5_temp_dir = era5_temp_dir,
  era5_ppt_dir = era5_ppt_dir,
  target_years = target_years,
  crop_root = crop_root,
  buffer_cells = 2L
)

message(
  "ADM1 extract: ", nrow(adm1_sf), " states; years ",
  min(mex_crops$years), "-", max(mex_crops$years),
  " (", length(mex_crops$temp_filepaths), " temp files, ",
  length(mex_crops$ppt_filepaths), " ppt files)"
)

#--------------- Extract (parallel over cropped years)

daily_mean_temp_proc <- run_extraction_parallel_by_year(
  locations_sf = adm1_sf,
  climate_filepaths = mex_crops$temp_filepaths,
  pop_raster_path = mex_crops$pop_raster_path,
  climate_variable_name = "mean_temp"
)

daily_total_precipitation_proc <- run_extraction_parallel_by_year(
  locations_sf = adm1_sf,
  climate_filepaths = mex_crops$ppt_filepaths,
  pop_raster_path = mex_crops$pop_raster_path,
  climate_variable_name = "mean_precipitation"
)

#--------------- Post-process + save (daily only)

daily_temp_mex_adm1 <- daily_mean_temp_proc %>%
  dplyr::left_join(location_lookup, by = "location_id") %>%
  dplyr::mutate(
    iso3 = "MEX",
    mean_temp_celsius = mean_temp - 273.15
  ) %>%
  dplyr::select(date, iso3, rne_iso_code, mean_temp_celsius) %>%
  dplyr::arrange(rne_iso_code, date)

daily_ppt_mex_adm1 <- daily_total_precipitation_proc %>%
  dplyr::left_join(location_lookup, by = "location_id") %>%
  dplyr::mutate(
    iso3 = "MEX",
    mean_precipitation_mm = mean_precipitation * 1000
  ) %>%
  dplyr::select(date, iso3, rne_iso_code, mean_precipitation_mm) %>%
  dplyr::arrange(rne_iso_code, date)

out_temp <- "day3-0900-coding-exercise/data/daily_temp_mex_adm1.csv"
out_ppt <- "day3-0900-coding-exercise/data/daily_ppt_mex_adm1.csv"

readr::write_csv(daily_temp_mex_adm1, out_temp)
readr::write_csv(daily_ppt_mex_adm1, out_ppt)

message("Wrote ", out_temp, " and ", out_ppt)
