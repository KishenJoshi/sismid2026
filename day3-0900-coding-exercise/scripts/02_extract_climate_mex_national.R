# 02_extract_climate_mex_national.R
# Extract daily ERA5 climate for Mexico national outline (pop-weighted).
# 1) Ensure Mexico-cropped yearly GeoTIFF cache (+ 2 ERA5 pop-cell buffer)
# 2) Parallel pop-weighted extract over cropped years (day loop)
# Years = calendar years in National_incidence for MEX.
#
# Usage (from repo root):
#   Rscript day3-0900-coding-exercise/scripts/02_extract_climate_mex_national.R

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
crop_root <- "day3-0900-coding-exercise/data/era5_mex_cropped"

#--------------- Mexico national shapefile

mexico_sf <- rnaturalearth::ne_countries(scale = 50, type = "map_units", returnclass = "sf") %>%
  dplyr::select(iso_a3_eh, geometry) %>%
  dplyr::rename(iso3 = iso_a3_eh) %>%
  dplyr::filter(iso3 == "MEX") %>%
  dplyr::mutate(location_id = 1L)

stopifnot(nrow(mexico_sf) == 1L)

#--------------- Years + Mexico-cropped cache (build only if incomplete)

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

#--------------- Extract (parallel over cropped years)

daily_mean_temp_proc <- run_extraction_parallel_by_year(
  locations_sf = mexico_sf,
  climate_filepaths = mex_crops$temp_filepaths,
  pop_raster_path = mex_crops$pop_raster_path,
  climate_variable_name = "mean_temp"
)

daily_total_precipitation_proc <- run_extraction_parallel_by_year(
  locations_sf = mexico_sf,
  climate_filepaths = mex_crops$ppt_filepaths,
  pop_raster_path = mex_crops$pop_raster_path,
  climate_variable_name = "mean_precipitation"
)

#--------------- Post-process + save (daily only)

daily_temp_mex_national <- daily_mean_temp_proc %>%
  dplyr::mutate(
    iso3 = "MEX",
    mean_temp_celsius = mean_temp - 273.15
  ) %>%
  dplyr::select(date, iso3, mean_temp_celsius) %>%
  dplyr::arrange(date)

daily_ppt_mex_national <- daily_total_precipitation_proc %>%
  dplyr::mutate(
    iso3 = "MEX",
    mean_precipitation_mm = mean_precipitation * 1000
  ) %>%
  dplyr::select(date, iso3, mean_precipitation_mm) %>%
  dplyr::arrange(date)

out_temp <- "day3-0900-coding-exercise/data/daily_temp_mex_national.csv"
out_ppt <- "day3-0900-coding-exercise/data/daily_ppt_mex_national.csv"

readr::write_csv(daily_temp_mex_national, out_temp)
readr::write_csv(daily_ppt_mex_national, out_ppt)

plan(sequential)
