# Shared paths for the day3 Mexico vector-control forecasting exercise.

day3_root <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) == 1L) {
    script_path <- normalizePath(
      sub("^--file=", "", file_arg),
      winslash = "/",
      mustWork = FALSE
    )
    # scripts/*.R -> day3 root is parent of scripts/
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE))
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (basename(wd) == "scripts") return(dirname(wd))
  if (basename(wd) == "functions") return(dirname(dirname(wd)))
  wd
}

DSD_ROOT_FALLBACK <- "C:/Users/Har-KishenJoshi/Documents/New folder/Sync_independent/Project/dengue_seasonal_drivers"

resolve_dsd_root <- function(day3 = day3_root()) {
  # day3 -> modelling_novel_data_streams -> Training -> Sync_independent/Project/...
  sync_root <- dirname(dirname(dirname(day3)))
  candidate <- normalizePath(
    file.path(sync_root, "Project", "dengue_seasonal_drivers"),
    winslash = "/",
    mustWork = FALSE
  )
  if (dir.exists(candidate)) return(candidate)
  if (dir.exists(DSD_ROOT_FALLBACK)) return(DSD_ROOT_FALLBACK)
  stop("Cannot locate dengue_seasonal_drivers directory")
}

DAY3_ROOT <- day3_root()
DSD_ROOT <- resolve_dsd_root(DAY3_ROOT)

MX_STATES_32 <- c(
  "MX-AGU", "MX-BCN", "MX-BCS", "MX-CAM", "MX-CHH", "MX-CHP",
  "MX-COA", "MX-COL", "MX-DIF", "MX-DUR", "MX-GRO", "MX-GUA", "MX-HID",
  "MX-JAL", "MX-MEX", "MX-MIC", "MX-MOR", "MX-NAY", "MX-NLE", "MX-OAX",
  "MX-PUE", "MX-QUE", "MX-ROO", "MX-SIN", "MX-SLP", "MX-SON", "MX-TAB",
  "MX-TAM", "MX-TLA", "MX-VER", "MX-YUC", "MX-ZAC"
)

ensure_dir <- function(...) {
  p <- file.path(...)
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  p
}
