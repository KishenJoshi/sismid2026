# Allocate weekly totals onto calendar months assuming equal cases each day of the week.

#' Split a closed date interval into per-day rows and apportion `value` equally.
apportion_interval_to_days <- function(start_date, end_date, value) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) {
    return(NULL)
  }
  days <- seq.Date(start_date, end_date, by = "day")
  n <- length(days)
  data.frame(
    date = days,
    value_day = as.numeric(value) / n,
    stringsAsFactors = FALSE
  )
}

#' Aggregate weekly (or interval) series to monthly totals via equal day-split.
#'
#' @param df data.frame with start/end dates, id, and value columns
#' @param start_col,end_col date columns defining the interval
#' @param id_cols character vector of grouping ids (e.g. state)
#' @param value_col numeric column to apportion
#' @return data.frame with id_cols, year, month, month_start, and monthly total
week_intervals_to_monthly <- function(df,
                                      start_col = "calendar_start_date",
                                      end_col = "calendar_end_date",
                                      id_cols = "rne_iso_code",
                                      value_col = "dengue_total") {
  stopifnot(all(c(start_col, end_col, value_col, id_cols) %in% names(df)))

  pieces <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    day_df <- apportion_interval_to_days(
      df[[start_col]][[i]],
      df[[end_col]][[i]],
      df[[value_col]][[i]]
    )
    if (is.null(day_df)) next
    for (id in id_cols) {
      day_df[[id]] <- df[[id]][[i]]
    }
    pieces[[i]] <- day_df
  }
  day_long <- dplyr::bind_rows(pieces)
  if (nrow(day_long) == 0L) {
    return(day_long[0, , drop = FALSE])
  }

  day_long %>%
    dplyr::mutate(
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m")),
      month_start = as.Date(sprintf("%04d-%02d-01", year, month))
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, "year", "month", "month_start")))) %>%
    dplyr::summarise(!!value_col := sum(value_day, na.rm = TRUE), .groups = "drop")
}
