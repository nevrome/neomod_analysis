load("../neomod_datapool/simulation_data/squared_euclidian_distance_over_time_sim_multiple.RData")
load("../neomod_datapool/R_data/distance_matrix_spatial_long.RData")

test <- regions_grid

test <- pbapply::pblapply(
  base::split(test, f = test$model_group), function(z) { 
    lapply(
      base::split(z, f = test$model_id), function(y) { 
        lapply(
          base::split(y, f = y$time), function(x) {
            mn <- pmin(x$regionA, x$regionB)
            mx <- pmax(x$regionA, x$regionB)
            int <- as.numeric(interaction(mn, mx))
            x <- x[match(unique(int), int),]
            return(x)
          }) %>%
          do.call(rbind, .)
      }) %>%
      do.call(rbind, .)    
  }) %>%
  do.call(rbind, .)


sed_spatial_distance <- test %>% dplyr::left_join(
  distance_matrix_spatial_long, by = c("regionA", "regionB")
) %>% 
  dplyr::filter(
    distance != 0
  ) %>%
  dplyr::mutate(
    relation = paste(regionA, "+", regionB),
    time = base::cut(
      time, 
      seq(-2200, -800, 200), labels = paste(seq(-2200, -1000, 200), seq(-2000, -800, 200), sep = " - "),
      include.lowest = TRUE, 
      right = FALSE)
  ) %>%
  dplyr::group_by(
    model_id, model_group, time, regionA, regionB, distance
  ) %>%
  dplyr::summarise(
    mean_sed = mean(sed, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    !is.na(mean_sed)
  )

save(
  sed_spatial_distance, 
  file = "../neomod_datapool/simulation_data/squared_euclidian_distance_over_timeblocks_multiple_simulations.RData"
)
