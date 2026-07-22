# Ground (LODES) + air (OpenFlights) mobility into Atlanta / Fulton Co.
#
# Mirrors notebooks/01_mobility_soln.ipynb.
# LODES live download is ~22 MB; default LIVE = FALSE uses the cached aggregate.
#
# Usage (from repo root):
#   Rscript day2-1100-more-novel-data-streams/scripts/01_mobility.R
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
out_dir <- file.path(root, "outputs", "01_mobility")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

USER_AGENT <- "SISMID2026-course/1.0 (your-email@example.com)"
LIVE <- FALSE # TRUE downloads the 22 MB LODES file and re-aggregates
TARGET_FIPS <- "13121" # Fulton County, GA
AIRPORT <- "ATL"

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

fetch_raw <- function(url, timeout_sec = 120) {
  resp <- GET(url, user_agent(USER_AGENT), timeout(timeout_sec))
  stop_for_status(resp)
  content(resp, as = "raw")
}

# ---------------------------------------------------------------------------
# Ground: LODES commuting into Fulton
# ---------------------------------------------------------------------------

if (isTRUE(LIVE)) {
  message("Downloading LODES + county names (LIVE=TRUE)...")
  names_txt <- rawToChar(
    fetch_raw(
      paste0(
        "https://www2.census.gov/geo/docs/reference/codes2020/",
        "national_county2020.txt"
      )
    )
  )
  name_lines <- strsplit(names_txt, "\n", fixed = TRUE)[[1]][-1]
  county_names <- list()
  for (line in name_lines) {
    p <- strsplit(line, "|", fixed = TRUE)[[1]]
    if (length(p) > 4L) {
      county_names[[paste0(p[[2]], p[[3]])]] <- paste0(p[[5]], ", ", p[[1]])
    }
  }

  raw <- fetch_raw(
    paste0(
      "https://lehd.ces.census.gov/data/lodes/LODES8/ga/od/",
      "ga_od_main_JT00_2021.csv.gz"
    ),
    timeout_sec = 300
  )
  tmp <- tempfile(fileext = ".csv.gz")
  writeBin(raw, tmp)
  od <- read_csv(gzfile(tmp), show_col_types = FALSE)
  unlink(tmp)

  od <- od %>%
    mutate(
      work_fips = substr(as.character(w_geocode), 1, 5),
      home_fips = substr(as.character(h_geocode), 1, 5)
    ) %>%
    filter(work_fips == TARGET_FIPS)

  total <- sum(od$S000, na.rm = TRUE)
  ground <- od %>%
    group_by(home_county_fips = home_fips) %>%
    summarise(commuters_to_fulton = sum(S000, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(commuters_to_fulton)) %>%
    slice_head(n = 15) %>%
    mutate(
      home_county = vapply(
        home_county_fips,
        function(f) county_names[[f]] %||% "?",
        character(1)
      ),
      share_pct = round(100 * commuters_to_fulton / total, 2)
    )
} else {
  p <- cache_path("lodes_fulton_inflow_top_counties.csv")
  message("Using cached LODES aggregate: ", p)
  ground <- read_csv(p, show_col_types = FALSE)
}

g <- ground %>%
  filter(as.character(home_county_fips) != TARGET_FIPS) %>%
  slice_head(n = 8)

p_ground <- ggplot(g, aes(
  x = reorder(home_county, commuters_to_fulton),
  y = commuters_to_fulton
)) +
  geom_col(fill = "#2A6F97") +
  coord_flip() +
  labs(
    title = "Ground mobility: who feeds us",
    x = NULL, y = "commuters into Fulton County, GA"
  ) +
  theme_minimal()
ggsave(
  file.path(out_dir, "ground_mobility_fulton.png"),
  p_ground, width = 9, height = 4, dpi = 120, bg = "white"
)
message("These are the neighbours to watch: their outbreak tends to reach us first.")

# ---------------------------------------------------------------------------
# Air: OpenFlights routes into ATL
# ---------------------------------------------------------------------------

air_cache <- cache_path("openflights_atl_inbound.csv")
air <- tryCatch(
  {
    ap_raw <- rawToChar(fetch_raw(
      "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
    ))
    ap_lines <- strsplit(ap_raw, "\n", fixed = TRUE)[[1]]
    airports <- list()
    for (line in ap_lines) {
      q <- scan(text = line, what = character(), sep = ",", quote = "\"",
                quiet = TRUE, fill = TRUE)
      if (length(q) > 4L && !(q[[5]] %in% c("", "\\N"))) {
        airports[[q[[5]]]] <- list(city = q[[3]], country = q[[4]])
      }
    }

    rt_raw <- rawToChar(fetch_raw(
      "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"
    ))
    rt_lines <- strsplit(rt_raw, "\n", fixed = TRUE)[[1]]
    rows <- list()
    for (line in rt_lines) {
      q <- scan(text = line, what = character(), sep = ",", quote = "\"",
                quiet = TRUE, fill = TRUE)
      if (length(q) > 4L && identical(q[[5]], AIRPORT) && !is.null(airports[[q[[3]]]])) {
        key <- paste(airports[[q[[3]]]]$country, q[[3]],
                     airports[[q[[3]]]]$city, sep = "\t")
        rows[[key]] <- (rows[[key]] %||% 0L) + 1L
      }
    }
    bind_rows(lapply(names(rows), function(k) {
      parts <- strsplit(k, "\t", fixed = TRUE)[[1]]
      tibble(
        origin_country = parts[[1]],
        origin_airport = parts[[2]],
        origin_city = parts[[3]],
        n_routes = rows[[k]]
      )
    }))
  },
  error = function(e) {
    message("Live OpenFlights pull failed: ", conditionMessage(e),
            " -> using cache ", air_cache)
    read_csv(air_cache, show_col_types = FALSE)
  }
)

by_country <- air %>%
  group_by(origin_country) %>%
  summarise(n_routes = sum(n_routes), .groups = "drop") %>%
  arrange(desc(n_routes))

message(
  nrow(air), " origin airports, ", nrow(by_country),
  " countries connect into ", AIRPORT
)
print(head(by_country, 10))

intl <- by_country %>%
  filter(origin_country != "United States") %>%
  slice_head(n = 10)

p_air <- ggplot(intl, aes(
  x = reorder(origin_country, n_routes),
  y = n_routes
)) +
  geom_col(fill = "#C1666B") +
  coord_flip() +
  labs(
    title = "Air mobility: international connectivity (importation risk proxy)",
    x = NULL, y = paste("inbound routes to", AIRPORT)
  ) +
  theme_minimal()
ggsave(
  file.path(out_dir, "air_mobility_atl_intl.png"),
  p_air, width = 9, height = 4, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

message("ground rows: ", nrow(ground), " | air rows: ", nrow(air))
message("ground share of total accounted for: ",
        round(sum(ground$share_pct, na.rm = TRUE), 1), " %")

write_csv(ground, file.path(out_dir, "mobility_ground_top_counties.csv"))
write_csv(air, file.path(out_dir, "mobility_air_inbound.csv"))
message("saved mobility_ground_top_counties.csv and mobility_air_inbound.csv")
