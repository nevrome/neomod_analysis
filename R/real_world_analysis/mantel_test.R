load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrix_spatial.RData")
#load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrizes_sed_burial_type.RData")
load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrizes_sed_burial_construction.RData")

#### 

mantel_test_results <- lapply(
  1:length(distance_matrizes_sed), function(i, x, y, z) {
    mantel_result <- vegan::mantel(x[[i]], y, method = "spear", permutations=999)
    data.frame(
      time = z[[i]],
      statistic = mantel_result$statistic, 
      signif = mantel_result$signif
    )
  },
  x = distance_matrizes_sed,
  y = distance_matrix_spatial,
  z = names(distance_matrizes_sed)
) %>%
  do.call(rbind, .)

save(
  mantel_test_results, 
  #file = "../neomod_datapool/bronze_age/mantel_sed_spatial_burial_type.RData"
  file = "../neomod_datapool/bronze_age/mantel_sed_spatial_burial_construction.RData"
)


####

load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrizes_sed_burial_type.RData")
dms_burial_type <- distance_matrizes_sed

load("/home/clemens/neomod/neomod_datapool/bronze_age/distance_matrizes_sed_burial_construction.RData")
dms_burial_construction <- distance_matrizes_sed

mantel_test_results <- lapply(
  1:length(dms_burial_type), function(i, x, y, z) {
    mantel_result <- vegan::mantel(x[[i]], y[[i]], method = "pearson", permutations=999)
    data.frame(
      time = z[[i]],
      statistic = mantel_result$statistic, 
      signif = mantel_result$signif
    )
  },
  x = dms_burial_type,
  y = dms_burial_construction,
  z = names(dms_burial_type)
) %>%
  do.call(rbind, .)

save(
  mantel_test_results, 
  file = "../neomod_datapool/bronze_age/mantel_sed_spatial_burial_type_burial_construction.RData"
)

