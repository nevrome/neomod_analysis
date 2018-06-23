load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrix_spatial.RData")
load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrizes_sed_burial_type.RData")

library(vegan)
mantel(distance_matrix_spatial, distance_matrizes_sed_burial_type[[1]], method = "spear", permutations=999)

mantel_test_results <- lapply(
  1:length(distance_matrizes_sed_burial_type), function(i, x, y, z) {
    mantel_result <- mantel(x[[i]], y, method = "spear", permutations=999)
    data.frame(
      time = z[[i]],
      statistic = mantel_result$statistic, 
      signif = mantel_result$signif
    )
  },
  x = distance_matrizes_sed_burial_type,
  y = distance_matrix_spatial,
  z = names(distance_matrizes_sed_burial_type)
) %>%
  do.call(rbind, .)

save(
  mantel_test_results, 
  file = "../neomod_datapool/bronze_age/mantel_sed_spatial_burial_type.RData"
)
