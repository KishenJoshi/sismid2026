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
})

root <- if (basename(getwd()) == "scripts") dirname(getwd()) else getwd()
source(file.path(root, "scripts/functions/paths.R"))
source(file.path(root, "scripts/functions/spatial_graph_helpers.R"))

raster_path <- file.path(
  DSD_ROOT,
  "results/02_08_generate_at_risk_pop_raster/at_risk_denv_pop.tif"
)
stopifnot(file.exists(raster_path))

message("Building Mexico KNN graph for ", length(MX_STATES_32), " states...")
res <- build_mx_knn_graph(
  rne_iso_codes = MX_STATES_32,
  at_risk_pop_raster_path = raster_path,
  k = 10L
)

out_dir <- ensure_dir(DAY3_ROOT, "outputs", "spatial")
if (!is.null(res$graph)) {
  saveRDS(res$graph, file.path(out_dir, "mx_knn10_idw_graph.RDS"))
} else {
  message("INLA not installed — saving weight matrix only; convert later with inla.read.graph()")
}
saveRDS(res$weight_matrix, file.path(out_dir, "mx_knn10_idw_weight_matrix.RDS"))
readr::write_csv(res$location_index, file.path(out_dir, "mx_location_index.csv"))
saveRDS(res$listw, file.path(out_dir, "mx_knn10_idw_listw.RDS"))
message("Wrote spatial outputs to ", out_dir)
print(res$location_index)
