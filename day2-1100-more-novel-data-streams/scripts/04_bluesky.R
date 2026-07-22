# Bluesky public-health account discovery + keyword feed scan.
#
# Mirrors notebooks/04_bluesky_soln.ipynb.
# Uses public AppView endpoints (no auth). searchPosts needs auth; we avoid it.
#
# Usage (from repo root):
#   Rscript day2-1100-more-novel-data-streams/scripts/04_bluesky.R
#
# Packages: dplyr, tidyr, readr, httr, jsonlite, stringr, ggplot2

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(httr)
  library(jsonlite)
  library(stringr)
  library(ggplot2)
})

root <- "day2-1100-more-novel-data-streams"
out_dir <- file.path(root, "outputs", "04_bluesky")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

USER_AGENT <- "SISMID2026-course/1.0 (your-email@example.com)"
API <- "https://public.api.bsky.app/xrpc"

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

bsky <- function(method, ...) {
  params <- list(...)
  url <- modify_url(paste0(API, "/", method), query = params)
  resp <- GET(url, user_agent(USER_AGENT), timeout(45))
  stop_for_status(resp)
  content(resp, as = "parsed", type = "application/json")
}

# ---------------------------------------------------------------------------
# Discover accounts from seed queries
# ---------------------------------------------------------------------------

SEEDS <- c(
  "epidemiology", "public health", "outbreak", "infectious disease",
  "virology", "global health"
)

accounts <- tryCatch(
  {
    acc <- list()
    for (t in SEEDS) {
      actors <- bsky("app.bsky.actor.searchActors", q = t, limit = 10)$actors
      for (a in actors) {
        acc[[a$handle]] <- a$displayName %||% ""
      }
      Sys.sleep(0.4)
    }
    acc
  },
  error = function(e) {
    p <- cache_path("bluesky_health_accounts.csv")
    message("Live discovery failed: ", conditionMessage(e), " -> cache ", p)
    cached <- read_csv(p, show_col_types = FALSE)
    dcol <- if ("display_name" %in% names(cached)) "display_name" else names(cached)[2]
    stats::setNames(as.list(as.character(cached[[dcol]])), cached$handle)
  }
)

message(length(accounts), " accounts on the watch list, e.g.:")
for (h in head(names(accounts), 6)) {
  message("   @", sprintf("%-36s", h), " ", substr(accounts[[h]], 1, 40))
}

# ---------------------------------------------------------------------------
# Scan author feeds for disease keywords (word-boundary)
# ---------------------------------------------------------------------------

KEYWORDS <- c(
  "dengue", "influenza", "covid", "measles", "rsv", "outbreak", "h5n1",
  "malaria", "cholera", "mpox", "avian flu", "bird flu", "flu"
)
pat <- paste0("\\b(", paste(str_replace_all(KEYWORDS, "([.+*?^$(){}|\\[\\]\\\\])",
                                            "\\\\\\1"), collapse = "|"), ")\\b")

scan_feeds <- function(accounts, n_accounts = 25, per_account = 50) {
  rows <- list()
  scanned <- 0L
  handles <- head(names(accounts), n_accounts)
  for (h in handles) {
    feed <- tryCatch(
      bsky("app.bsky.feed.getAuthorFeed", actor = h, limit = per_account)$feed,
      error = function(e) NULL
    )
    if (is.null(feed)) next
    for (item in feed) {
      rec <- item$post$record
      txt <- gsub("\n", " ", rec$text %||% "", fixed = TRUE)
      txt <- trimws(txt)
      scanned <- scanned + 1L
      hits <- str_extract_all(txt, regex(pat, ignore_case = TRUE))[[1]]
      if (length(hits) > 0L) {
        rows[[length(rows) + 1L]] <- tibble(
          date = substr(rec$createdAt %||% "", 1, 10),
          handle = h,
          keywords = paste(sort(unique(tolower(hits))), collapse = ";"),
          text = substr(txt, 1, 280)
        )
      }
    }
    Sys.sleep(0.25)
  }
  list(posts = bind_rows(rows), scanned = scanned)
}

posts <- tryCatch(
  {
    res <- scan_feeds(accounts)
    if (nrow(res$posts) < 1L) stop("no matches (rate-limited?)")
    message("scanned ", res$scanned, " posts -> ", nrow(res$posts),
            " keyword matches")
    res$posts
  },
  error = function(e) {
    p <- cache_path("bluesky_health_posts.csv")
    message("Live scan failed: ", conditionMessage(e), " -> cache ", p)
    read_csv(p, show_col_types = FALSE)
  }
)

# ---------------------------------------------------------------------------
# Naive substring vs word-boundary sanity check
# ---------------------------------------------------------------------------

naive <- sum(str_detect(posts$text, regex("flu", ignore_case = TRUE)), na.rm = TRUE)
strict <- sum(str_detect(posts$keywords, "\\bflu\\b"), na.rm = TRUE)
message("substring 'flu' anywhere : ", naive)
message("word-boundary \\bflu\\b     : ", strict)
message("example of what substring matching wrongly catches:")
for (t in head(posts$text, 200)) {
  tl <- tolower(as.character(t))
  if (grepl("influen", tl, fixed = TRUE) && !grepl("influenza", tl, fixed = TRUE)) {
    message("  ", substr(t, 1, 100))
    break
  }
}

# ---------------------------------------------------------------------------
# Keyword bar chart + recent chatter
# ---------------------------------------------------------------------------

kw <- posts %>%
  mutate(keywords = as.character(keywords)) %>%
  filter(!is.na(keywords), keywords != "") %>%
  separate_rows(keywords, sep = ";") %>%
  count(keywords, name = "n") %>%
  arrange(desc(n))

p_kw <- ggplot(kw, aes(x = reorder(keywords, n), y = n)) +
  geom_col(fill = "#2A6F97") +
  coord_flip() +
  labs(
    title = "Disease mentions across public-health accounts (Bluesky)",
    x = NULL, y = "posts"
  ) +
  theme_minimal()
ggsave(
  file.path(out_dir, "bluesky_keyword_counts.png"),
  p_kw, width = 9, height = 4, dpi = 120, bg = "white"
)

message("most recent outbreak chatter:")
recent <- posts %>% arrange(desc(date)) %>% slice_head(n = 5)
for (i in seq_len(nrow(recent))) {
  message(
    "  [", recent$date[[i]], "] @",
    sprintf("%-26s", substr(recent$handle[[i]], 1, 26)),
    " (", recent$keywords[[i]], ") ",
    substr(recent$text[[i]], 1, 80)
  )
}

out_csv <- file.path(out_dir, "bluesky_outbreak_chatter.csv")
write_csv(posts, out_csv)
message("saved ", out_csv, ": ", nrow(posts), " posts")
