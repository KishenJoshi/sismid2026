# Google Trends helpers: topic resolution + single-topic × single-geo fetches,
# plus loaders for subnational (adm1) dated downloads.

parse_hits <- function(x) {
  x <- as.character(x)
  x[x == "<1"] <- "0"
  as.numeric(x)
}

norm_col <- function(x) {
  gsub("[^A-Za-z0-9]+", "_", trimws(as.character(x)))
}

# Optional hard-coded mid overrides — only keep mids verified to return data.
# Leave empty by default; autocomplete + term fallback handle resolution.
TOPIC_MID_OVERRIDES <- c()

#' Resolve a phrase to a Google Trends topic mid via autocomplete.
#' Prefers science/health topic types when multiple suggestions exist.
#' Returns list(mid, title, type); falls back to the literal phrase.
topic_mid <- function(phrase,
                      hl = "en-US",
                      preferred_types = c(
                        "Disease", "Virus", "Organism", "Topic",
                        "Medical Condition", "Species", "Insect"
                      )) {
  if (phrase %in% names(TOPIC_MID_OVERRIDES)) {
    return(list(
      mid = unname(TOPIC_MID_OVERRIDES[[phrase]]),
      title = phrase,
      type = "override mid"
    ))
  }
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
      topics <- as.data.frame(topics, stringsAsFactors = FALSE)
      # Prefer exact/case-insensitive title match among preferred types
      title_l <- tolower(as.character(topics$title))
      phrase_l <- tolower(phrase)
      type_l <- tolower(as.character(topics$type))
      pref_l <- tolower(preferred_types)

      score <- rep(0, nrow(topics))
      score[title_l == phrase_l] <- score[title_l == phrase_l] + 10
      score[grepl(paste0("^", gsub("([\\W])", "\\\\\\1", phrase_l)), title_l)] <-
        score[grepl(paste0("^", gsub("([\\W])", "\\\\\\1", phrase_l)), title_l)] + 5
      score[type_l %in% pref_l] <- score[type_l %in% pref_l] + 3
      # Penalise fashion/brand-like types
      score[grepl("brand|company|clothing|retail", type_l)] <-
        score[grepl("brand|company|clothing|retail", type_l)] - 20

      best <- which.max(score)
      list(
        mid = as.character(topics$mid[[best]]),
        title = as.character(topics$title[[best]]),
        type = as.character(topics$type[[best]])
      )
    },
    error = function(e) {
      message("suggestions lookup failed for '", phrase, "': ", conditionMessage(e))
      list(mid = phrase, title = phrase, type = "raw term")
    }
  )
}

normalize_timeframe <- function(timeframe) {
  tf <- trimws(as.character(timeframe))
  tf <- gsub("^today\\s+", "today+", tf)
  tf <- gsub("^now\\s+", "now+", tf)
  tf
}

#' Fetch one keyword (term or already-resolved mid) for one geo.
fetch_one_keyword_geo <- function(keyword,
                                  geo,
                                  timeframe,
                                  tries = 4L,
                                  wait_sec = 45) {
  timeframe <- normalize_timeframe(timeframe)
  last_err <- NULL
  for (i in seq_len(tries)) {
    result <- tryCatch(
      {
        gtrendsR::gtrends(
          keyword = keyword,
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
      out <- iot %>%
        dplyr::transmute(
          date = as.Date(date),
          hits = parse_hits(hits),
          geo = as.character(geo),
          keyword = as.character(keyword)
        ) %>%
        dplyr::filter(!is.na(date)) %>%
        dplyr::group_by(date, geo, keyword) %>%
        dplyr::summarise(hits = mean(hits, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(date)
      return(out)
    }
    msg <- if (!is.null(last_err)) conditionMessage(last_err) else "empty frame"
    # Empty interest (sparse geo×term) is not rate-limiting — don't back off hard.
    empty_data <- grepl("No data returned|empty frame", msg, ignore.case = TRUE)
    rate_limited <- grepl("429|Too Many|rate|status_code|widget", msg, ignore.case = TRUE)
    if (empty_data) {
      message("  No data for this geo/keyword; skipping retries")
      return(NULL)
    }
    if (i < tries && rate_limited) {
      wait <- wait_sec * i
      message("  Attempt ", i, "/", tries, " failed (", msg, "); wait ", wait, "s")
      Sys.sleep(wait)
    } else if (i < tries) {
      message("  Attempt ", i, "/", tries, " failed (", msg, "); brief retry")
      Sys.sleep(5)
    } else {
      message("  Failed after ", tries, " tries: ", msg)
      return(NULL)
    }
  }
  NULL
}

#' Resolve query_type then fetch one topic/term × one geo.
#' @param query character label (topic phrase or Spanish term)
#' @param query_type "topic" or "term"
fetch_one_topic_one_geo <- function(query,
                                    geo,
                                    timeframe,
                                    query_type = c("topic", "term"),
                                    canon_name = NULL,
                                    tries = 4L,
                                    wait_sec = 45) {
  query_type <- match.arg(query_type)
  canon_name <- if (is.null(canon_name)) norm_col(query) else norm_col(canon_name)

  if (identical(query_type, "topic")) {
    resolved <- topic_mid(query)
    fetch_kw <- resolved$mid
    message(
      "  topic '", query, "' -> ", resolved$title,
      " (", resolved$type, "), mid=", fetch_kw, " | geo=", geo
    )
    meta <- resolved
  } else {
    fetch_kw <- query
    message("  term '", query, "' | geo=", geo)
    meta <- list(mid = query, title = query, type = "raw term")
  }

  series <- fetch_one_keyword_geo(
    keyword = fetch_kw,
    geo = geo,
    timeframe = timeframe,
    tries = tries,
    wait_sec = wait_sec
  )

  # If topic mid fails, fall back to the literal search phrase as a term.
  if (is.null(series) && identical(query_type, "topic") && !identical(fetch_kw, query)) {
    message("  topic mid failed; falling back to term '", query, "'")
    series <- fetch_one_keyword_geo(
      keyword = query,
      geo = geo,
      timeframe = timeframe,
      tries = tries,
      wait_sec = wait_sec
    )
    if (!is.null(series)) {
      meta$type <- paste0(meta$type, "+term_fallback")
      fetch_kw <- query
    }
  }

  if (is.null(series)) return(NULL)

  series %>%
    dplyr::mutate(
      topic_label = canon_name,
      query_input = query,
      query_type = query_type,
      mid = meta$mid,
      topic_title = meta$title,
      topic_type = meta$type,
      fetched_keyword = fetch_kw
    )
}

#' Day-split weekly Trends hits to monthly means (equal weight per day).
trends_weekly_to_monthly <- function(df,
                                     date_col = "date",
                                     value_col = "hits",
                                     id_cols = c("geo", "topic_label")) {
  # gtrends weekly rows are start-of-week; treat each as a 7-day interval.
  df <- df %>%
    dplyr::mutate(
      .start = as.Date(.data[[date_col]]),
      .end = .start + 6L
    )
  week_intervals_to_monthly(
    df,
    start_col = ".start",
    end_col = ".end",
    id_cols = id_cols,
    value_col = value_col
  ) %>%
    dplyr::rename(hits = !!value_col)
}

#' Load all per-geo/topic CSVs under the newest (or specified) downloads date folder.
load_gtrends_adm1_panel <- function(downloads_dir,
                                    date_folder = NULL,
                                    pattern = "^gtrends_.*\\.csv$") {
  if (is.null(date_folder)) {
    dates <- list.dirs(downloads_dir, full.names = FALSE, recursive = FALSE)
    dates <- dates[grepl("^\\d{4}-\\d{2}-\\d{2}$", dates)]
    if (!length(dates)) stop("No dated download folders in ", downloads_dir)
    date_folder <- max(dates)
  }
  folder <- file.path(downloads_dir, date_folder)
  files <- list.files(folder, pattern = pattern, full.names = TRUE)
  if (!length(files)) stop("No Trends CSVs in ", folder)

  dplyr::bind_rows(lapply(files, function(f) {
    readr::read_csv(f, show_col_types = FALSE)
  })) %>%
    dplyr::mutate(date = as.Date(date))
}
