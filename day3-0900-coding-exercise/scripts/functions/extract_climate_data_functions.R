# extract_climate_data_functions.R
# Mexico ERA5 daily climate extraction (national or ADM1).
# Pre-crop global yearly NetCDFs to Mexico bbox (+ 2 pop-cell buffer), cache as
# GeoTIFFs, then pop-weighted extract with a per-day exact_extract loop.

pop_weighted_mean <- function(rasters, coverage_fraction) {
  clim_raster <- rasters[, setdiff(names(rasters), "pop_weight"), drop = FALSE]
  pop_weights <- rasters$pop_weight * coverage_fraction

  area_weights <- coverage_fraction
  area_weights[is.na(area_weights)] <- 0
  pop_weights[is.na(pop_weights)] <- 0

  pop_num <- colSums(clim_raster * pop_weights, na.rm = TRUE)
  pop_denom <- colSums((!is.na(clim_raster)) * pop_weights)
  out <- pop_num / pop_denom

  need_fallback <- pop_denom == 0
  if (any(need_fallback)) {
    area_num <- colSums(clim_raster * area_weights, na.rm = TRUE)
    area_denom <- colSums((!is.na(clim_raster)) * area_weights)
    fallback_out <- area_num / area_denom
    fallback_out[area_denom == 0] <- NA_real_
    out[need_fallback] <- fallback_out[need_fallback]
  }

  as.numeric(out)
}

read_era5_times_from_files <- function(files) {
  parse_origin <- function(units_str) {
    origin_str <- sub("^(days|hours|seconds) since ", "", units_str, ignore.case = TRUE)
    as.Date(trimws(origin_str))
  }
  do.call(
    c,
    lapply(files, function(filepath) {
      nc <- ncdf4::nc_open(filepath)
      units_str <- nc$dim$valid_time$units
      vals <- nc$dim$valid_time$vals
      ncdf4::nc_close(nc)
      origin <- parse_origin(units_str)
      if (grepl("days since", units_str, ignore.case = TRUE)) {
        origin + vals
      } else if (grepl("hours since", units_str, ignore.case = TRUE)) {
        origin + (vals / 24)
      } else if (grepl("seconds since", units_str, ignore.case = TRUE)) {
        origin + (vals / 86400)
      } else {
        stop("read_era5_times_from_files: unsupported valid_time units: ", units_str)
      }
    })
  )
}

#' Calendar years of Mexico rows in National_incidence.csv
mex_national_incidence_years <- function(national_incidence_path) {
  incid <- readr::read_csv(national_incidence_path, show_col_types = FALSE)
  years <- incid %>%
    dplyr::filter(.data$iso3 == "MEX") %>%
    dplyr::pull(.data$Year) %>%
    as.integer() %>%
    unique() %>%
    sort()
  stopifnot(length(years) > 0L)
  years
}

year_from_climate_path <- function(filepaths) {
  as.integer(sub(".*_(\\d{4})\\.(nc|tif)$", "\\1", basename(filepaths), ignore.case = TRUE))
}

#' Keep yearly NetCDF paths whose filename year is in `years`.
filter_climate_files_by_years <- function(filepaths, years) {
  yrs <- year_from_climate_path(filepaths)
  keep <- !is.na(yrs) & yrs %in% years
  if (!any(keep)) {
    stop(
      "filter_climate_files_by_years: no files match years ",
      paste(range(years), collapse = "-")
    )
  }
  filepaths[keep]
}

#' Mexico bbox expanded by `buffer_cells` × pop raster cell size (lon and lat).
buffered_mexico_extent <- function(mexico_sf, pop_raster, buffer_cells = 2L) {
  bb <- sf::st_bbox(mexico_sf)
  buf_x <- as.numeric(buffer_cells) * terra::xres(pop_raster)
  buf_y <- as.numeric(buffer_cells) * terra::yres(pop_raster)
  terra::ext(
    bb[["xmin"]] - buf_x,
    bb[["xmax"]] + buf_x,
    bb[["ymin"]] - buf_y,
    bb[["ymax"]] + buf_y
  )
}

mex_cropped_tif_path <- function(out_dir, variable_tag, year) {
  file.path(out_dir, sprintf("era5_mex_%s_%d.tif", variable_tag, as.integer(year)))
}

#' Crop one yearly NetCDF to `crop_ext` and write a GeoTIFF (with layer times).
crop_year_nc_to_tif <- function(nc_path, out_tif, crop_ext, climate_subds) {
  climate_raster <- terra::rast(nc_path, subds = climate_subds)
  terra::time(climate_raster) <- read_era5_times_from_files(nc_path)
  clim_crop <- terra::crop(climate_raster, crop_ext)
  terra::writeRaster(
    clim_crop,
    out_tif,
    overwrite = TRUE,
    gdal = c("COMPRESS=LZW")
  )
  invisible(out_tif)
}

#' Ensure Mexico-cropped ERA5 + pop cache exists.
#' Crops only missing year files (keeps existing). Parallel over missing jobs
#' via the caller's future plan (e.g. plan(multisession, workers = 3)).
#'
#' Cache layout:
#'   crop_root/temperature/era5_mex_t2m_YYYY.tif
#'   crop_root/precipitation/era5_mex_tp_YYYY.tif
#'   crop_root/at_risk_denv_pop_era5_mex.tif
ensure_mex_era5_crops <- function(mexico_sf,
                                  pop_raster_path,
                                  era5_temp_dir,
                                  era5_ppt_dir,
                                  target_years,
                                  crop_root = "day3-0900-coding-exercise/data/era5_mex_cropped",
                                  buffer_cells = 2L,
                                  climate_temp_subds = "t2m",
                                  climate_ppt_subds = "tp") {
  stopifnot(file.exists(pop_raster_path))
  stopifnot(nrow(mexico_sf) >= 1L)

  temp_out_dir <- file.path(crop_root, "temperature")
  ppt_out_dir <- file.path(crop_root, "precipitation")
  pop_out_path <- file.path(crop_root, "at_risk_denv_pop_era5_mex.tif")
  dir.create(temp_out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(ppt_out_dir, recursive = TRUE, showWarnings = FALSE)

  pop_raster <- terra::rast(pop_raster_path)
  crop_ext <- buffered_mexico_extent(mexico_sf, pop_raster, buffer_cells = buffer_cells)
  crop_ext_vec <- c(
    terra::xmin(crop_ext),
    terra::xmax(crop_ext),
    terra::ymin(crop_ext),
    terra::ymax(crop_ext)
  )

  src_temp <- filter_climate_files_by_years(
    list.files(era5_temp_dir, pattern = "\\.nc$", full.names = TRUE),
    target_years
  )
  src_ppt <- filter_climate_files_by_years(
    list.files(era5_ppt_dir, pattern = "\\.nc$", full.names = TRUE),
    target_years
  )

  years <- sort(intersect(
    year_from_climate_path(src_temp),
    year_from_climate_path(src_ppt)
  ))
  stopifnot(length(years) > 0L)

  src_temp <- src_temp[match(years, year_from_climate_path(src_temp))]
  src_ppt <- src_ppt[match(years, year_from_climate_path(src_ppt))]

  expected_temp <- vapply(
    years,
    function(y) mex_cropped_tif_path(temp_out_dir, "t2m", y),
    character(1)
  )
  expected_ppt <- vapply(
    years,
    function(y) mex_cropped_tif_path(ppt_out_dir, "tp", y),
    character(1)
  )

  if (!file.exists(pop_out_path)) {
    message("Cropping at-risk population raster to Mexico bbox...")
    pop_crop <- terra::crop(pop_raster, crop_ext)
    terra::writeRaster(
      pop_crop,
      pop_out_path,
      overwrite = TRUE,
      gdal = c("COMPRESS=LZW")
    )
  }

  missing_temp <- !file.exists(expected_temp)
  missing_ppt <- !file.exists(expected_ppt)

  crop_jobs <- dplyr::bind_rows(
    tibble::tibble(
      year = years[missing_temp],
      nc_path = src_temp[missing_temp],
      out_tif = expected_temp[missing_temp],
      climate_subds = climate_temp_subds,
      variable = "t2m"
    ),
    tibble::tibble(
      year = years[missing_ppt],
      nc_path = src_ppt[missing_ppt],
      out_tif = expected_ppt[missing_ppt],
      climate_subds = climate_ppt_subds,
      variable = "tp"
    )
  ) %>%
    dplyr::arrange(.data$year, .data$variable)

  if (nrow(crop_jobs) == 0L) {
    message(
      "Using existing Mexico cropped ERA5 cache (",
      length(years), " years, ", min(years), "-", max(years), ")"
    )
  } else {
    message(
      "Cropping ", nrow(crop_jobs), " missing Mexico ERA5 file(s) ",
      "in parallel (years ", paste(sort(unique(crop_jobs$year)), collapse = ", "), ")..."
    )

    furrr::future_pmap(
      list(
        crop_jobs$nc_path,
        crop_jobs$out_tif,
        crop_jobs$climate_subds,
        crop_jobs$year,
        crop_jobs$variable
      ),
      function(nc_path, out_tif, climate_subds, year, variable) {
        message("  cropping ", variable, " ", year)
        crop_year_nc_to_tif(
          nc_path = nc_path,
          out_tif = out_tif,
          crop_ext = terra::ext(crop_ext_vec),
          climate_subds = climate_subds
        )
      },
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c("terra", "ncdf4"),
        globals = c(
          "crop_year_nc_to_tif",
          "read_era5_times_from_files",
          "crop_ext_vec"
        )
      ),
      .progress = TRUE
    )

    still_missing <- c(
      expected_temp[!file.exists(expected_temp)],
      expected_ppt[!file.exists(expected_ppt)]
    )
    if (length(still_missing) > 0L) {
      stop(
        "ensure_mex_era5_crops: failed to create: ",
        paste(basename(still_missing), collapse = ", ")
      )
    }
    message("Mexico cropped ERA5 cache ready at ", crop_root)
  }

  list(
    temp_filepaths = expected_temp,
    ppt_filepaths = expected_ppt,
    pop_raster_path = pop_out_path,
    crop_ext = crop_ext,
    years = years
  )
}

ensure_climate_time <- function(climate_raster, filepath) {
  dates <- terra::time(climate_raster)
  if (!is.null(dates) && length(dates) == terra::nlyr(climate_raster) && !any(is.na(dates))) {
    terra::time(climate_raster) <- as.Date(dates)
    return(climate_raster)
  }
  yr <- year_from_climate_path(filepath)
  if (is.na(yr)) {
    stop("ensure_climate_time: could not parse year from ", filepath)
  }
  terra::time(climate_raster) <- as.Date(sprintf("%d-01-01", yr)) +
    seq_len(terra::nlyr(climate_raster)) - 1L
  climate_raster
}

#' Pop-weighted extract: one exact_extract call per day (custom R summary).
#' Expects climate + pop rasters already cropped to the same Mexico window.
extract_climate <- function(climate_raster, pop_raster, locations_sf) {
  stopifnot("location_id" %in% names(locations_sf))
  dates <- as.Date(terra::time(climate_raster))
  stopifnot(length(dates) == terra::nlyr(climate_raster))
  stopifnot(!any(is.na(dates)))

  if (terra::nlyr(pop_raster) > 1L) {
    pop_raster <- pop_raster[[1L]]
  }

  n_clim <- terra::nlyr(climate_raster)
  r_stack <- c(climate_raster, pop_raster)
  names(r_stack)[n_clim + 1L] <- "pop_weight"
  loc_ids <- locations_sf$location_id

  purrr::map_dfr(seq_len(n_clim), function(i) {
    r_timestep <- r_stack[[c(i, n_clim + 1L)]]
    vals <- exactextractr::exact_extract(
      r_timestep,
      locations_sf,
      pop_weighted_mean
    )
    tibble::tibble(
      location_id = loc_ids,
      date = dates[[i]],
      climate_value = as.numeric(vals)
    )
  })
}

#' Parallel extraction over yearly Mexico-cropped climate GeoTIFFs.
#' Assumes `plan(multisession, ...)` (or similar) is set by the caller.
run_extraction_parallel_by_year <- function(locations_sf,
                                            climate_filepaths,
                                            pop_raster_path,
                                            climate_variable_name) {
  stopifnot(nrow(locations_sf) >= 1L)
  stopifnot(file.exists(pop_raster_path))
  stopifnot(length(climate_filepaths) > 0L)
  stopifnot(all(file.exists(climate_filepaths)))

  locations_sf <- sf::st_as_sf(locations_sf)

  file_panels <- furrr::future_map(
    climate_filepaths,
    function(fp) {
      climate_raster <- ensure_climate_time(terra::rast(fp), fp)
      pop_raster <- terra::rast(pop_raster_path)
      extract_climate(
        climate_raster = climate_raster,
        pop_raster = pop_raster,
        locations_sf = locations_sf
      )
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      packages = c("terra", "sf", "exactextractr", "dplyr", "tibble", "purrr"),
      globals = c(
        "extract_climate",
        "ensure_climate_time",
        "year_from_climate_path",
        "pop_weighted_mean",
        "locations_sf",
        "pop_raster_path"
      )
    ),
    .progress = TRUE
  )

  climate_panel <- dplyr::bind_rows(file_panels) %>%
    dplyr::arrange(.data$location_id, .data$date)

  if (!missing(climate_variable_name)) {
    climate_panel <- climate_panel %>%
      dplyr::rename(!!climate_variable_name := climate_value)
  }
  climate_panel
}
