# Wikipedia monthly pageviews for Dengue (en / es / pt).
#
# Mirrors notebooks/01_wikipedia_pageviews_soln.ipynb.
#
# Usage (from repo root):
#   Rscript day2-0900-data-beyond-google-trends/scripts/01_wikipedia_pageviews.R
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

root <- "day2-0900-data-beyond-google-trends"
out_dir <- file.path(root, "outputs", "01_wikipedia_pageviews")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

USER_AGENT <- "SISMID2026-course/1.0 (your-email@example.com)"
CACHE_PATHS <- c(
  file.path(root, "data/wikipedia_dengue_pageviews_cached.csv"),
  "data/wikipedia_dengue_pageviews_cached.csv",
  "wikipedia_dengue_pageviews_cached.csv"
)

wiki_fetch <- function(article, wiki, start = "2016010100", end = "2025120100") {
  url <- paste0(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
    wiki, "/all-access/all-agents/", article, "/monthly/", start, "/", end
  )
  tryCatch(
    {
      resp <- GET(url, user_agent(USER_AGENT), timeout(30))
      stop_for_status(resp)
      items <- content(resp, as = "parsed", type = "application/json")$items
      tibble(
        date = as.Date(substr(vapply(items, `[[`, "", "timestamp"), 1, 8),
                       format = "%Y%m%d"),
        views = vapply(items, `[[`, 0, "views")
      )
    },
    error = function(e) {
      message("Wikipedia live pull failed: ", conditionMessage(e))
      NULL
    }
  )
}

load_cache <- function() {
  for (p in CACHE_PATHS) {
    if (file.exists(p)) {
      message("Using cached snapshot: ", p)
      return(read_csv(p, show_col_types = FALSE) %>% mutate(date = as.Date(date)))
    }
  }
  stop("Wikipedia cache not found; check the data/ folder.")
}

get_dengue_wiki <- function() {
  en <- wiki_fetch("Dengue_fever", "en.wikipedia")
  es <- wiki_fetch("Dengue", "es.wikipedia")
  pt <- wiki_fetch("Dengue", "pt.wikipedia")
  if (is.null(en) || is.null(es) || is.null(pt)) {
    return(load_cache())
  }
  en %>%
    rename(dengue_en = views) %>%
    inner_join(es %>% rename(dengue_es = views), by = "date") %>%
    inner_join(pt %>% rename(dengue_pt = views), by = "date")
}

# ---------------------------------------------------------------------------
# Fetch + plot
# ---------------------------------------------------------------------------

df <- get_dengue_wiki()
cols <- setdiff(names(df), "date")
message("rows: ", nrow(df), " | range: ", min(df$date), " to ", max(df$date))

p <- ggplot(
  df %>% pivot_longer(all_of(cols), names_to = "series", values_to = "views"),
  aes(date, views, colour = series)
) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Wikipedia pageviews: Dengue by language",
    x = "date", y = "views", colour = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(
  file.path(out_dir, "dengue_pageviews_by_language.png"),
  p, width = 10, height = 4, dpi = 120, bg = "white"
)

# ---------------------------------------------------------------------------
# Reproducibility check
# ---------------------------------------------------------------------------

a <- wiki_fetch("Dengue_fever", "en.wikipedia")
b <- wiki_fetch("Dengue_fever", "en.wikipedia")
if (!is.null(a) && !is.null(b)) {
  message("identical pulls? ", identical(a$views, b$views))
} else {
  message("live unavailable; from the cache this series is fixed anyway.")
}

# ---------------------------------------------------------------------------
# Diagnostics + save
# ---------------------------------------------------------------------------

message("missing per column:")
print(colSums(is.na(df)))
message("correlation between languages:")
print(round(cor(df[cols], use = "pairwise.complete.obs"), 2))

out_csv <- file.path(out_dir, "dengue_wikipedia.csv")
write_csv(df, out_csv)
message("saved ", out_csv, " ", paste(dim(df), collapse = " x "))
