# 03_prepare_mx_spatial_graph.R
# Mexico-only k=10 inverse-distance KNN INLA graph (at-risk pop-weighted centroids).

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(terra)
  library(spdep)
  library(rnaturalearth)
  library(purrr)
  library(tibble)
  library(readr)
  library(INLA)
})

source("day3-0900-coding-exercise/scripts/functions/spatial_graph_helpers.R")

MX_STATES_32 <- c(
  "MX-AGU", "MX-BCN", "MX-BCS", "MX-CAM", "MX-CHH", "MX-CHP",
  "MX-COA", "MX-COL", "MX-DIF", "MX-DUR", "MX-GRO", "MX-GUA", "MX-HID",
  "MX-JAL", "MX-MEX", "MX-MIC", "MX-MOR", "MX-NAY", "MX-NLE", "MX-OAX",
  "MX-PUE", "MX-QUE", "MX-ROO", "MX-SIN", "MX-SLP", "MX-SON", "MX-TAB",
  "MX-TAM", "MX-TLA", "MX-VER", "MX-YUC", "MX-ZAC"
)

raster_path <- "day3-0900-coding-exercise/data/at_risk_denv_pop.tif"
out_dir <- "day3-0900-coding-exercise/outputs/spatial"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

res <- build_mx_knn_graph(
  rne_iso_codes = MX_STATES_32,
  at_risk_pop_raster_path = raster_path,
  k = 10L
)

# Convert weight matrix to INLA graph (formerly 03b)
graph <- if (!is.null(res$graph)) {
  res$graph
} else {
  INLA::inla.read.graph(res$weight_matrix)
}

saveRDS(graph, file.path(out_dir, "mx_knn10_idw_graph.RDS"))
saveRDS(res$weight_matrix, file.path(out_dir, "mx_knn10_idw_weight_matrix.RDS"))
write_csv(res$location_index, file.path(out_dir, "mx_location_index.csv"))
saveRDS(res$listw, file.path(out_dir, "mx_knn10_idw_listw.RDS"))
