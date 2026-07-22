# Open-Meteo weather -> absolute humidity for Atlanta (or edit MY_*).
#
# Mirrors notebooks/02_weather_soln.ipynb.
#
# Usage (from repo root):
#   Rscript day2-1100-more-novel-data-streams/scripts/02_weather.R
#
# Packages: dplyr, tidyr, readr, httr, jsonlite, lubridate, ggplot2

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(lubridate)
  library(ggplot2)
})

root <- "day2-1100-more-novel-data-streams"
out_dir <- file.path(root, "outputs", "02_weather")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

USER_AGENT <- "SISMID2026-course/1.0 (your-email@example.com)"

# ===== EDIT for your own location =====
MY_PLACE <- "Atlanta"
MY_LAT <- 33.749
MY_LON <- -84.388
START <- "2016-01-01"
END <- "2026-07-10"
# ======================================

cache_path <- function(fname) {
  candidates <- c(
    file.path(root, "data", fname),
    file.path("data", fname),
    fname
  )
  for (p in candidates) if (file.exists(p)) return(p)
  NULL
}

absolute_humidity <- function(T, Td) {
  # g/m^3 from temperature and dew point in Celsius
  e <- 6.112 * exp(17.67 * Td / (Td + 243.5))
  out <- 216.7 * e / (T + 273.15)
  out[is.na(T) | is.na(Td)] <- NA_real_
  out
}

# ---------------------------------------------------------------------------
# Fetch daily temp + dewpoint
# ---------------------------------------------------------------------------

url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive",
  "?latitude=", MY_LAT, "&longitude=", MY_LON,
  "&start_date=", START, "&end_date=", END,
  "&daily=temperature_2m_mean,dew_point_2m_mean&timezone=America%2FNew_York"
)

wx <- tryCatch(
  {
    resp <- GET(url, user_agent(USER_AGENT), timeout(120))
    stop_for_status(resp)
    d <- content(resp, as = "parsed", type = "application/json")$daily
    tibble(
      date = as.Date(unlist(d$time)),
      temp_c = as.numeric(unlist(d$temperature_2m_mean)),
      dewpoint_c = as.numeric(unlist(d$dew_point_2m_mean))
    ) %>%
      mutate(abs_humidity_g_m3 = absolute_humidity(temp_c, dewpoint_c))
  },
  error = function(e) {
    p <- cache_path("openmeteo_atlanta_weather.csv")
    message("Live pull failed: ", conditionMessage(e), " -> cache ", p)
    read_csv(p, show_col_types = FALSE) %>% mutate(date = as.Date(date))
  }
)

message(nrow(wx), " days, ", min(wx$date), " to ", max(wx$date))

# ---------------------------------------------------------------------------
# Weekly AH series
# ---------------------------------------------------------------------------

wk <- wx %>%
  mutate(week = floor_date(date, "week", week_start = 7)) %>%
  group_by(week) %>%
  summarise(abs_humidity_g_m3 = mean(abs_humidity_g_m3, na.rm = TRUE),
            .groups = "drop") %>%
  rename(date = week)

p_wk <- ggplot(wk, aes(date, abs_humidity_g_m3)) +
  geom_line(linewidth = 0.6, colour = "#2A6F97") +
  labs(
    title = paste("Weekly absolute humidity,", MY_PLACE),
    x = "date", y = expression("absolute humidity (g/" * m^3 * ")")
  ) +
  theme_minimal()
ggsave(
  file.path(out_dir, "weekly_absolute_humidity.png"),
  p_wk, width = 11, height = 4, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# By-month climatology (flu season shaded dark)
# ---------------------------------------------------------------------------

monthly <- wx %>%
  mutate(month = month(date)) %>%
  group_by(month) %>%
  summarise(abs_humidity_g_m3 = mean(abs_humidity_g_m3, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(flu_season = month %in% c(12, 1, 2))

p_mo <- ggplot(monthly, aes(month, abs_humidity_g_m3, fill = flu_season)) +
  geom_col() +
  scale_fill_manual(
    values = c(`TRUE` = "#2A6F97", `FALSE` = "#9bbfd4"),
    guide = "none"
  ) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Absolute humidity by month (flu season shaded dark)",
    x = "month", y = expression("mean AH (g/" * m^3 * ")")
  ) +
  theme_minimal()
ggsave(
  file.path(out_dir, "monthly_absolute_humidity.png"),
  p_mo, width = 8, height = 3.5, dpi = 120, bg = "white"
)

driest <- monthly %>%
  arrange(abs_humidity_g_m3) %>%
  slice_head(n = 3)
message(
  "driest months: ",
  paste0(driest$month, "=", round(driest$abs_humidity_g_m3, 2), collapse = ", ")
)

# ---------------------------------------------------------------------------
# Diagnostics + save
# ---------------------------------------------------------------------------

message("date range : ", min(wx$date), " to ", max(wx$date))
message("missing    :")
print(colSums(is.na(wx[c("temp_c", "dewpoint_c", "abs_humidity_g_m3")])))
message(
  "AH range   : ", round(min(wx$abs_humidity_g_m3, na.rm = TRUE), 2),
  " to ", round(max(wx$abs_humidity_g_m3, na.rm = TRUE), 2), " g/m^3"
)

out_csv <- file.path(out_dir, "weather_absolute_humidity.csv")
write_csv(wx, out_csv)
message("saved ", out_csv)
