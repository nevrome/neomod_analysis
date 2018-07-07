load("../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_burial_construction.RData")

time_regions_grid <- regions_grid %>% dplyr::mutate(
  time = base::cut(
    time, 
    seq(-2200, -800, 200), labels = paste(seq(-2200, -1000, 200), seq(-2000, -800, 200), sep = " - "),
    include.lowest = TRUE, 
    right = FALSE)
) %>%
  dplyr::group_by(
    time, regionA, regionB
  ) %>%
  dplyr::summarise(
    mean_sed = mean(sed, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  # that's dangerous...
  dplyr::mutate(
    mean_sed = tidyr::replace_na(mean_sed, 0)
  )

distance_matrizes_sed <- lapply(
  base::split(time_regions_grid, time_regions_grid$time), function(x){
    x %>%
      dplyr::select(
        -time
      ) %>%
      tidyr::spread(regionA, mean_sed) %>%
      dplyr::select(
        -regionB
      ) %>%
      as.matrix()
  } 
)

save(distance_matrizes_sed, file = "../neomod_datapool/bronze_age/distance_matrizes_sed_burial_construction.RData")
