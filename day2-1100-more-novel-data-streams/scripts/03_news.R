# GDELT outbreak-watch news (optional Media Cloud if key set).
#
# Mirrors notebooks/03_news_soln.ipynb.
#
# Usage (from repo root):
#   Rscript day2-1100-more-novel-data-streams/scripts/03_news.R
# Optional:
#   MEDIACLOUD_API_KEY=... Rscript .../03_news.R
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
out_dir <- file.path(root, "outputs", "03_news")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

USER_AGENT <- "SISMID2026-course/1.0 (your-email@example.com)"

# ===== EDIT for your own disease / region =====
MY_QUERY <- "dengue outbreak"
TIMESPAN <- "1m" # 1m = past month; also 1w, 3m ...
# =============================================

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

# ---------------------------------------------------------------------------
# GDELT DOC 2.0
# ---------------------------------------------------------------------------

api <- paste0(
  "https://api.gdeltproject.org/api/v2/doc/doc",
  "?query=", URLencode(MY_QUERY, reserved = TRUE),
  "&mode=artlist&maxrecords=250&format=json&timespan=", TIMESPAN
)

news <- tryCatch(
  {
    resp <- GET(api, user_agent(USER_AGENT), timeout(120))
    stop_for_status(resp)
    arts <- content(resp, as = "parsed", type = "application/json")$articles
    if (is.null(arts) || length(arts) < 1L) {
      stop("no articles returned (rate-limited?)")
    }
    bind_rows(lapply(arts, function(a) {
      tibble(
        seendate = a$seendate %||% "",
        sourcecountry = a$sourcecountry %||% "",
        domain = a$domain %||% "",
        title = trimws(a$title %||% ""),
        url = a$url %||% ""
      )
    }))
  },
  error = function(e) {
    p <- cache_path("gdelt_dengue_articles.csv")
    message("Live GDELT pull failed: ", conditionMessage(e), " -> cache ", p)
    read_csv(p, show_col_types = FALSE)
  }
)

news <- news %>%
  mutate(
    date = as.POSIXct(seendate, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
  )

message(nrow(news), " articles, ", min(news$date, na.rm = TRUE),
        " to ", max(news$date, na.rm = TRUE))
print(head(news %>% select(date, sourcecountry, title), 8))

# ---------------------------------------------------------------------------
# By source country
# ---------------------------------------------------------------------------

top <- news %>%
  mutate(sourcecountry = ifelse(sourcecountry == "", "unknown", sourcecountry)) %>%
  count(sourcecountry, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = 12)

p_top <- ggplot(top, aes(x = reorder(sourcecountry, n), y = n)) +
  geom_col(fill = "#3B8A5B") +
  coord_flip() +
  labs(
    title = paste0('News mentions of "', MY_QUERY, '" by source country'),
    x = NULL, y = "articles"
  ) +
  theme_minimal()
ggsave(
  file.path(out_dir, "news_by_source_country.png"),
  p_top, width = 9, height = 4, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# Daily volume
# ---------------------------------------------------------------------------

daily <- news %>%
  filter(!is.na(date)) %>%
  mutate(day = as.Date(date)) %>%
  count(day, name = "n")

p_daily <- ggplot(daily, aes(day, n)) +
  geom_line(colour = "#2A6F97") +
  geom_point(size = 1.2, colour = "#2A6F97") +
  labs(
    title = paste0('Coverage volume: "', MY_QUERY, '"'),
    x = NULL, y = "articles/day"
  ) +
  theme_minimal()
ggsave(
  file.path(out_dir, "news_daily_volume.png"),
  p_daily, width = 10, height = 3.5, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# Diagnostics + save
# ---------------------------------------------------------------------------

message("articles      : ", nrow(news))
message("unique domains: ", n_distinct(news$domain))
message("duplicate titles: ", sum(duplicated(news$title)), " (syndication)")

out_csv <- file.path(out_dir, "news_outbreak_watch.csv")
write_csv(news, out_csv)
message("saved ", out_csv)

message("most recent headlines:")
recent <- news %>%
  arrange(desc(date)) %>%
  slice_head(n = 5) %>%
  mutate(
    dlab = ifelse(is.na(date), "?", as.character(as.Date(date))),
    title_short = substr(title, 1, 90)
  )
for (i in seq_len(nrow(recent))) {
  message(" ", recent$dlab[[i]], " | ", recent$sourcecountry[[i]],
          " | ", recent$title_short[[i]])
}

# ---------------------------------------------------------------------------
# Optional Media Cloud (skip if MEDIACLOUD_API_KEY unset)
# ---------------------------------------------------------------------------

MC_KEY <- Sys.getenv("MEDIACLOUD_API_KEY", "")
US_NATIONAL_COLLECTION <- 34412234L

if (!nzchar(MC_KEY)) {
  message("MEDIACLOUD_API_KEY not set -> skipping Media Cloud.")
  message("Free key: sign up at https://search.mediacloud.org/")
  message("The GDELT results above are the no-key path and need nothing extra.")
} else {
  message("MEDIACLOUD_API_KEY set; attempting Media Cloud story search...")
  tryCatch(
    {
      end_d <- Sys.Date()
      start_d <- end_d - 30
      # Directory/Search API: collection-scoped story list (best-effort HTTP).
      mc_url <- modify_url(
        "https://api.mediacloud.org/api/v2/stories_public/list",
        query = list(
          key = MC_KEY,
          q = MY_QUERY,
          fq = paste0("tags_id_media:", US_NATIONAL_COLLECTION),
          rows = 25
        )
      )
      resp <- GET(mc_url, user_agent(USER_AGENT), timeout(60))
      if (http_error(resp)) {
        # Newer Search API style
        resp <- GET(
          "https://search.mediacloud.org/api/search/story_list",
          add_headers(Authorization = paste("Bearer", MC_KEY)),
          query = list(
            q = MY_QUERY,
            start = as.character(start_d),
            end = as.character(end_d),
            collection_ids = US_NATIONAL_COLLECTION
          ),
          user_agent(USER_AGENT),
          timeout(60)
        )
      }
      stop_for_status(resp)
      payload <- content(resp, as = "parsed", type = "application/json")
      stories <- if (is.data.frame(payload)) {
        payload
      } else if (!is.null(payload$stories)) {
        bind_rows(lapply(payload$stories, as_tibble))
      } else if (is.list(payload) && !is.null(payload[[1]])) {
        bind_rows(lapply(payload, function(x) as_tibble(x)))
      } else {
        tibble()
      }
      message('Media Cloud: ', nrow(stories), ' stories for "', MY_QUERY, '"')
      if (nrow(stories) > 0L) {
        keep <- intersect(c("publish_date", "media_name", "title"), names(stories))
        print(head(stories[keep], 8))
        write_csv(stories, file.path(out_dir, "mediacloud_stories.csv"))
      }
    },
    error = function(e) {
      message("Media Cloud request failed: ", conditionMessage(e))
      message("GDELT results above remain the no-key path.")
    }
  )
}
