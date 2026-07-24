# Convert weight matrix to INLA graph once INLA is available.
w <- readRDS("outputs/spatial/mx_knn10_idw_weight_matrix.RDS")
g <- INLA::inla.read.graph(w)
saveRDS(g, "outputs/spatial/mx_knn10_idw_graph.RDS")
cat("graph n=", g$n, "\n")
