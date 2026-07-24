# Mexico-only inverse-distance KNN spatial graph (mirrors dengue_seasonal_drivers).

identify_pop_centre <- function(shapeunit_sf, pop_raster) {
  shape_vect <- terra::vect(shapeunit_sf)
  shapeunit_raster <- terra::crop(pop_raster, shape_vect)
  shapeunit_raster <- terra::mask(shapeunit_raster, shape_vect)
  cell_df <- terra::as.data.frame(shapeunit_raster, xy = TRUE, na.rm = TRUE)
  value_col <- setdiff(names(cell_df), c("x", "y"))
  cell_df <- cell_df %>%
    dplyr::transmute(
      x = .data$x,
      y = .data$y,
      pop = .data[[value_col]]
    ) %>%
    dplyr::filter(!is.na(pop), pop > 0)
  if (nrow(cell_df) == 0L) {
    return(tibble::tibble(pop_centre_long = NA_real_, pop_centre_lat = NA_real_))
  }
  tibble::tibble(
    pop_centre_long = stats::weighted.mean(cell_df$x, w = cell_df$pop),
    pop_centre_lat = stats::weighted.mean(cell_df$y, w = cell_df$pop)
  )
}

identify_pop_weighted_centroid <- function(target_shapefiles, at_risk_pop_raster) {
  target_shapefiles <- target_shapefiles %>%
    dplyr::mutate(.row_id = dplyr::row_number())
  centroid_tbl <- purrr::map_dfr(seq_len(nrow(target_shapefiles)), function(i) {
    row_sf <- target_shapefiles[i, ]
    centre_tbl <- identify_pop_centre(row_sf, at_risk_pop_raster)
    row_sf %>%
      sf::st_drop_geometry() %>%
      dplyr::bind_cols(centre_tbl)
  })
  centroid_tbl %>% dplyr::select(-.row_id)
}

#' Build symmetric k-NN inverse-distance INLA graph for Mexico adm1 units.
build_mx_knn_graph <- function(rne_iso_codes,
                               at_risk_pop_raster_path,
                               k = 10L) {
  at_risk_pop_raster <- terra::rast(at_risk_pop_raster_path)
  target_crs <- sf::st_crs(at_risk_pop_raster)

  admin1_shapes <- rnaturalearth::ne_states(iso_a2 = "MX", returnclass = "sf")
  admin1_target_shapes <- admin1_shapes %>%
    dplyr::select(iso_3166_2, geometry) %>%
    dplyr::rename(rne_iso_code = iso_3166_2) %>%
    dplyr::filter(rne_iso_code %in% rne_iso_codes) %>%
    dplyr::group_by(rne_iso_code) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") %>%
    sf::st_transform(target_crs)

  missing <- setdiff(rne_iso_codes, admin1_target_shapes$rne_iso_code)
  if (length(missing)) {
    warning("Missing NE polygons for: ", paste(missing, collapse = ", "))
  }

  centroids <- identify_pop_weighted_centroid(admin1_target_shapes, at_risk_pop_raster)
  if (any(is.na(centroids$pop_centre_long) | is.na(centroids$pop_centre_lat))) {
    stop("NA pop-weighted centroids — review at-risk raster coverage")
  }

  centroids_sf <- centroids %>%
    dplyr::arrange(rne_iso_code) %>%
    dplyr::mutate(loc_idx = dplyr::row_number()) %>%
    sf::st_as_sf(
      coords = c("pop_centre_long", "pop_centre_lat"),
      crs = target_crs
    ) %>%
    sf::st_transform("ESRI:54009")

  coords_km <- sf::st_coordinates(centroids_sf) / 1000
  knn_nb <- spdep::knearneigh(coords_km, k = k) %>% spdep::knn2nb()
  knn_nb_sym <- spdep::make.sym.nb(knn_nb)
  if (any(spdep::card(knn_nb_sym) == 0)) {
    stop("At least one node is disconnected")
  }

  knn_dlist <- spdep::nbdists(knn_nb_sym, coords_km)
  knn_inv_d <- lapply(knn_dlist, function(x) 1 / x)
  knn_listw <- spdep::nb2listw(knn_nb_sym, glist = knn_inv_d, style = "B")
  w_mat <- spdep::listw2mat(knn_listw)

  knn_graph <- NULL
  if (requireNamespace("INLA", quietly = TRUE)) {
    knn_graph <- INLA::inla.read.graph(w_mat)
  }

  list(
    graph = knn_graph,
    weight_matrix = w_mat,
    location_index = centroids_sf %>%
      sf::st_drop_geometry() %>%
      dplyr::select(rne_iso_code, loc_idx),
    listw = knn_listw
  )
}
