load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrix_spatial.RData")
load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrizes_sed_burial_type.RData")

library(vegan)
mantel(distance_matrix_spatial, distance_matrizes_sed_burial_type[[1]], method = "pearson", permutations=999)

lapply(
  distance_matrizes_sed_burial_type, function(x, y) {
    mantel(x, y, method = "pearson", permutations=999)
  },
  y = distance_matrix_spatial
)
