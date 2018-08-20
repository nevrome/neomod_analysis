load("data_analysis/development_proportions_burial_construction.RData")

prop <- proportion_development_burial_construction

sed <- function(pi, pj) {
  pi <- pi / sum(pi)
  pj <- pj / sum(pj)
  sum((pi - pj)^2)
}

long_prop <- prop %>%
  tidyr::spread(
    idea, proportion
  )

regions <- prop$region_name %>% unique()
timesteps <- prop$timestep %>% unique()

regions_grid <- 
  expand.grid(
    regionA = regions, regionB = regions, time = timesteps, 
    stringsAsFactors = FALSE
  ) %>%
  tibble::as.tibble() %>%
  dplyr::left_join(
    long_prop,
    by = c("regionA" = "region_name", "time" = "timestep")
  ) %>%
  dplyr::left_join(
    long_prop,
    by = c("regionB" = "region_name", "time" = "timestep"),
    suffix = c("_regionA", "_regionB")
  )

regions_grid <- regions_grid %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    sed = sed(c(flat_regionA, mound_regionA), c(flat_regionB, mound_regionB))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    regionA, regionB, time, sed
  )

save(regions_grid, file = "data_analysis/squared_euclidian_distance_over_time_burial_construction.RData")
