# Google Trends helpers (gtrendsR): term / additive / topic pulls with retries.

parse_hits <- function(x) {
  x <- as.character(x)
  x[x == "<1"] <- "0"
  as.numeric(x)
}

norm_col <- function(x) {
  gsub("\\s+", "_", trimws(as.character(x)))
}

# Resolve a phrase to a Google Trends topic mid via the autocomplete API.
# Returns list(mid, title, type); falls back to the literal phrase.
topic_mid <- function(phrase, hl = "en-US") {
  url <- paste0(
    "https://trends.google.com/trends/api/autocomplete/",
    utils::URLencode(phrase, reserved = TRUE),
    "?hl=", hl
  )
  tryCatch(
    {
      raw <- httr::GET(url, httr::user_agent("Mozilla/5.0"))
      httr::stop_for_status(raw)
      txt <- httr::content(raw, as = "text", encoding = "UTF-8")
      txt <- sub("^\\)\\]\\}'\\,?\\s*", "", txt)
      payload <- jsonlite::fromJSON(txt, simplifyDataFrame = TRUE)
      topics <- payload$default$topics
      if (is.null(topics) || NROW(topics) < 1L) {
        return(list(mid = phrase, title = phrase, type = "raw term"))
      }
      list(
        mid = as.character(topics$mid[[1]]),
        title = as.character(topics$title[[1]]),
        type = as.character(topics$type[[1]])
      )
    },
    error = function(e) {
      message("suggestions lookup failed: ", conditionMessage(e))
      list(mid = phrase, title = phrase, type = "raw term")
    }
  )
}

# Map timeframe aliases used in the Python notebooks onto gtrendsR.
normalize_timeframe <- function(timeframe) {
  tf <- trimws(as.character(timeframe))
  # gtrendsR expects "today+5-y" (plus), not "today 5-y"
  tf <- gsub("^today\\s+", "today+", tf)
  tf <- gsub("^now\\s+", "now+", tf)
  tf
}

# Fetch interest-over-time for up to 5 keywords.
# kw_list entries may be:
#   - raw terms ("flu")
#   - additive / OR queries ("flu + gripe")
#   - topic mids ("/m/0cycc") when query_type = "topic" (or already resolved)
# query_type:
#   - "term"  : use kw_list as literal strings (additive queries ok)
#   - "topic" : resolve each entry via topic_mid() and pull by mid
# Returns a tidy data.frame (date + one column per keyword), or NULL on failure.
gt_fetch <- function(kw_list,
                     timeframe = "today 5-y",
                     geo = "US",
                     query_type = c("term", "topic"),
                     tries = 4L,
                     wait_sec = 12) {
  query_type <- match.arg(query_type)
  stopifnot(length(kw_list) >= 1L, length(kw_list) <= 5L)

  input_keywords <- as.character(kw_list)
  fetch_keywords <- input_keywords
  col_labels <- norm_col(input_keywords)

  if (identical(query_type, "topic")) {
    resolved <- lapply(input_keywords, topic_mid)
    fetch_keywords <- vapply(resolved, `[[`, character(1), "mid")
    col_labels <- make.unique(
      vapply(resolved, function(z) norm_col(z$title), character(1)),
      sep = "_"
    )
    for (i in seq_along(resolved)) {
      message(
        "  topic: '", input_keywords[[i]], "' -> ",
        resolved[[i]]$title, " (", resolved[[i]]$type, "), mid=",
        fetch_keywords[[i]]
      )
    }
  }

  timeframe <- normalize_timeframe(timeframe)
  Sys.sleep(stats::runif(1, 0, 3))

  last_err <- NULL
  for (attempt in seq_len(tries)) {
    result <- tryCatch(
      {
        gtrendsR::gtrends(
          keyword = fetch_keywords,
          geo = geo,
          time = timeframe,
          onlyInterest = TRUE
        )
      },
      error = function(e) {
        last_err <<- e
        NULL
      }
    )

    iot <- if (!is.null(result)) result$interest_over_time else NULL
    if (!is.null(iot) && nrow(iot) > 0L) {
      long <- iot %>%
        dplyr::transmute(
          date = as.Date(date),
          keyword = as.character(keyword),
          hits = parse_hits(hits)
        ) %>%
        dplyr::filter(!is.na(date), !is.na(keyword)) %>%
        dplyr::group_by(date, keyword) %>%
        dplyr::summarise(hits = mean(hits, na.rm = TRUE), .groups = "drop")

      # Map returned keyword strings back to stable column labels.
      label_lookup <- stats::setNames(col_labels, fetch_keywords)
      long <- long %>%
        dplyr::mutate(
          col = unname(label_lookup[keyword]),
          col = ifelse(is.na(col), norm_col(keyword), col)
        ) %>%
        dplyr::select(date, col, hits) %>%
        tidyr::pivot_wider(names_from = col, values_from = hits) %>%
        dplyr::arrange(date)

      # Keep column order: date, then requested labels that are present.
      ordered_cols <- c("date", intersect(col_labels, names(long)))
      return(dplyr::select(long, dplyr::all_of(ordered_cols)))
    }

    msg <- if (!is.null(last_err)) conditionMessage(last_err) else "empty frame"
    rate_limited <- grepl("429|Too Many|rate|empty", msg, ignore.case = TRUE)
    if (rate_limited && attempt < tries) {
      wait <- wait_sec * attempt
      message(
        "Rate-limited (attempt ", attempt, "/", tries,
        "); waiting ", wait, "s and retrying..."
      )
      Sys.sleep(wait)
    } else {
      message("Live Google Trends pull failed: ", msg)
      return(NULL)
    }
  }
  NULL
}

load_cache <- function(cache_paths) {
  for (p in cache_paths) {
    if (file.exists(p)) {
      message("Using cached example: ", p)
      return(
        readr::read_csv(p, show_col_types = FALSE) %>%
          dplyr::mutate(date = as.Date(date))
      )
    }
  }
  stop("Cached example not found; check the data/ folder.")
}
