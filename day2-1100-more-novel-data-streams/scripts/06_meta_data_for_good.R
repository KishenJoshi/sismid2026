# Meta AI for Good via HDX: Relative Wealth Index + Movement Distribution (Mexico).
#
# Mirrors notebooks/06_meta_data_for_good_soln.ipynb.
# Movement LIVE download is ~195 MB; default LIVE = FALSE uses cached Mexico slices.
#
# Usage (from repo root):
#   Rscript day2-1100-more-novel-data-streams/scripts/06_meta_data_for_good.R
#
# Packages: dplyr, tidyr, readr, httr, jsonlite, ggplot2

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(ggplot2)
})

root <- "day2-1100-more-novel-data-streams"
out_dir <- file.path(root, "outputs", "06_meta_data_for_good")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

USER_AGENT <- "SISMID2026-course/1.0 (your-email@example.com)"
HDX <- "https://data.humdata.org/api/3/action/"
COUNTRY <- "Mexico"
LIVE <- FALSE # TRUE downloads the ~195 MB global movement file and filters to MEX

`%||%` <- function(a, b) if (!is.null(a)) a else b

cache_path <- function(fname) {
  candidates <- c(
    file.path(root, "data", fname),
    file.path("data", fname),
    fname
  )
  for (p in candidates) if (file.exists(p)) return(p)
  NULL
}

hdx <- function(action, ...) {
  params <- list(...)
  url <- modify_url(paste0(HDX, action), query = params)
  resp <- GET(url, user_agent(USER_AGENT), timeout(60))
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")$result
}

# ---------------------------------------------------------------------------
# Catalogue: Meta organization on HDX
# ---------------------------------------------------------------------------

cat_df <- tryCatch(
  {
    res <- hdx("package_search", fq = "organization:meta", rows = 50)
    message(res$count, " datasets published by 'AI for Good at Meta' on HDX")
    bind_rows(lapply(res$results, function(p) {
      formats <- unique(vapply(p$resources, function(r) r$format %||% "?", character(1)))
      tibble(
        title = p$title,
        formats = paste(sort(formats), collapse = ","),
        n_resources = length(p$resources)
      )
    }))
  },
  error = function(e) {
    message("HDX catalogue query failed: ", conditionMessage(e))
    tibble()
  }
)

if (nrow(cat_df) > 0L) {
  print(head(cat_df, 12))
  write_csv(cat_df, file.path(out_dir, "hdx_meta_catalogue_sample.csv"))
}

# ---------------------------------------------------------------------------
# Relative Wealth Index (Mexico)
# ---------------------------------------------------------------------------

rwi <- tryCatch(
  {
    pkg <- hdx("package_show", id = "relative-wealth-index")
    url <- NULL
    for (r in pkg$resources) {
      if (identical(r$name, paste0(COUNTRY, "_relative_wealth_index.csv"))) {
        url <- r$url
        break
      }
    }
    if (is.null(url)) stop("RWI resource not found for ", COUNTRY)
    resp <- GET(url, user_agent(USER_AGENT), timeout(180))
    stop_for_status(resp)
    tmp <- tempfile(fileext = ".csv")
    writeBin(content(resp, as = "raw"), tmp)
    on.exit(unlink(tmp), add = TRUE)
    read_csv(tmp, show_col_types = FALSE)
  },
  error = function(e) {
    p <- cache_path("meta_rwi_mexico.csv")
    message("Live HDX pull failed: ", conditionMessage(e), " -> cache ", p)
    read_csv(p, show_col_types = FALSE)
  }
)

message(format(nrow(rwi), big.mark = ","), " grid cells; columns: ",
        paste(names(rwi), collapse = ", "))
message("rwi range ", round(min(rwi$rwi, na.rm = TRUE), 2), " to ",
        round(max(rwi$rwi, na.rm = TRUE), 2))

# Scatter is dense; subsample for a readable PNG if very large.
rwi_plot <- if (nrow(rwi) > 40000L) {
  dplyr::slice_sample(rwi, n = 40000L)
} else {
  rwi
}

p_rwi <- ggplot(rwi_plot, aes(longitude, latitude, colour = rwi)) +
  geom_point(size = 0.15) +
  scale_colour_viridis_c(name = "relative wealth index") +
  labs(
    title = paste0("Meta Relative Wealth Index, ", COUNTRY, " (~2.4 km grid)"),
    x = "longitude", y = "latitude"
  ) +
  coord_fixed() +
  theme_minimal()
ggsave(
  file.path(out_dir, "meta_rwi_mexico.png"),
  p_rwi, width = 7.5, height = 6, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# Movement Distribution (Mexico national daily)
# ---------------------------------------------------------------------------

if (isTRUE(LIVE)) {
  pkg <- hdx("package_show", id = "movement-distribution")
  url <- NULL
  for (r in pkg$resources) {
    if (identical(r$format, "CSV")) {
      url <- r$url
      break
    }
  }
  if (is.null(url)) stop("movement-distribution CSV resource not found")
  message("Downloading global movement file (large)...")
  resp <- GET(url, user_agent(USER_AGENT), timeout(600))
  stop_for_status(resp)
  tmp <- tempfile(fileext = ".csv")
  writeBin(content(resp, as = "raw"), tmp)
  allc <- read_csv(tmp, show_col_types = FALSE)
  unlink(tmp)
  mex <- allc %>% filter(country == "MEX")
  daily <- mex %>%
    group_by(ds, home_to_ping_distance_category) %>%
    summarise(
      mean_fraction = mean(distance_category_ping_fraction, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(distance_category = home_to_ping_distance_category)
} else {
  p <- cache_path("meta_movement_mx_national_daily.csv")
  message("Using cached Mexico slices: ", p)
  daily <- read_csv(p, show_col_types = FALSE)
  # cached may already use distance_category / mean_fraction
  if (!"distance_category" %in% names(daily) &&
      "home_to_ping_distance_category" %in% names(daily)) {
    daily <- daily %>%
      rename(
        distance_category = home_to_ping_distance_category,
        mean_fraction = distance_category_ping_fraction
      )
  }
}

daily <- daily %>% mutate(ds = as.Date(ds))
message(n_distinct(daily$ds), " days: ", min(daily$ds), " -> ", max(daily$ds))

piv <- daily %>%
  pivot_wider(
    id_cols = ds,
    names_from = distance_category,
    values_from = mean_fraction
  )

band_cols <- setdiff(names(piv), "ds")
piv_long <- piv %>%
  pivot_longer(all_of(band_cols), names_to = "distance_category",
               values_to = "mean_fraction")

p_mov <- ggplot(piv_long, aes(ds, mean_fraction, colour = distance_category)) +
  geom_line() +
  geom_point(size = 1.2) +
  labs(
    title = "Mexico: distance from home (Meta Movement Distribution)",
    x = "date", y = "mean fraction of pings", colour = "distance band"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(
  file.path(out_dir, "meta_movement_distance_bands.png"),
  p_mov, width = 10, height = 4, dpi = 120, bg = "white"
)

if ("0" %in% names(piv)) {
  message("mean share who stayed home: ", round(mean(piv[["0"]], na.rm = TRUE), 3))
}

# ---------------------------------------------------------------------------
# Municipal latest snapshot
# ---------------------------------------------------------------------------

muni_path <- cache_path("meta_movement_mx_municipal_latest.csv")
muni <- read_csv(muni_path, show_col_types = FALSE)
home <- muni %>%
  filter(as.character(home_to_ping_distance_category) == "0") %>%
  arrange(desc(distance_category_ping_fraction))

message(format(n_distinct(muni$gadm_name), big.mark = ","),
        " municipalities on ", muni$ds[[1]])
message("most home-bound municipalities:")
print(head(home %>% select(gadm_name, distance_category_ping_fraction), 8))
message("most mobile municipalities:")
print(tail(home %>% select(gadm_name, distance_category_ping_fraction), 8))

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

write_csv(rwi, file.path(out_dir, "meta_rwi.csv"))
write_csv(daily, file.path(out_dir, "meta_movement_daily.csv"))
message("saved meta_rwi.csv and meta_movement_daily.csv")
message("RWI missing values:")
print(colSums(is.na(rwi)))
if ("error" %in% names(rwi)) {
  message(
    "RWI error column: mean ", round(mean(rwi$error, na.rm = TRUE), 3),
    " -> every cell is a MODEL ESTIMATE with uncertainty, not a measurement"
  )
}
