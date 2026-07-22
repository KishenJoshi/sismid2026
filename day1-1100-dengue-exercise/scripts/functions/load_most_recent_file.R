# Load the most recent dated download of a named file.
#
# Expects subdirectories under `directory` named YYYY-MM-DD (e.g. from Sys.Date()).
# Optional `geo` appends _{GEO} before the file extension when `filename` does not
# already contain a geo tag, e.g. googletrends_dengue.csv + geo="BR"
#   -> googletrends_dengue_BR.csv
#
# Requires: stringr, lubridate, magrittr/dplyr (for %>%), readr (for CSV).

load_most_recent_file <- function(directory, filename, geo = NULL) {
  if (!is.null(geo) && nzchar(geo)) {
    geo <- toupper(geo)
    ext <- tools::file_ext(filename)
    stem <- sub(paste0("\\.", ext, "$"), "", filename, ignore.case = TRUE)
    # Avoid double-tagging if caller already passed a geo-suffixed name
    if (!grepl(paste0("_", geo, "$"), stem, ignore.case = TRUE)) {
      filename <- paste0(stem, "_", geo, ".", ext)
    }
  }

  dirs <- list.dirs(directory, full.names = FALSE, recursive = FALSE)
  date_dirs <- dirs[str_detect(dirs, "^\\d{4}-\\d{2}-\\d{2}$")] %>%
    ymd()

  if (length(date_dirs) == 0L) {
    stop("No dated subdirectories found in: ", directory)
  }

  most_recent_date <- format(max(date_dirs), "%Y-%m-%d")
  full_path <- file.path(directory, most_recent_date, filename)

  if (!file.exists(full_path)) {
    stop("File not found: ", full_path)
  }

  ext <- tolower(tools::file_ext(filename))

  obj <- switch(
    ext,
    csv = readr::read_csv(full_path, show_col_types = FALSE),
    rds = readRDS(full_path),
    rda = {
      env <- new.env(parent = emptyenv())
      load(full_path, envir = env)
      as.list(env)
    },
    stop("Unsupported file type: .", ext, " (file: ", filename, ")")
  )

  return(obj)
}
