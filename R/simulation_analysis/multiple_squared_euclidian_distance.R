load("R/simulation_results/sim1.RData")

prop <- models_grid$idea_proportions %>%
  do.call(rbind, .)

sed <- function(pi, pj) {
  pi <- pi / sum(pi)
  pj <- pj / sum(pj)
  sum((pi - pj)^2)
}

long_prop <- prop %>%
  tidyr::spread(
    idea, proportion
  )

regions_grid <- 
  expand.grid(
    regionA = prop$region %>% unique(), 
    regionB = prop$region %>% unique(), 
    time = prop$timestep %>% unique(), 
    model_id =  prop$model_id %>% unique(),
    multiplier =  prop$multiplier %>% unique(),
    stringsAsFactors = FALSE
  ) %>%
  tibble::as.tibble() %>%
  dplyr::left_join(
    long_prop,
    by = c("regionA" = "region", "time" = "timestep", "model_id" = "model_id", "multiplier" = "multiplier" )
  ) %>%
  dplyr::left_join(
    long_prop,
    by = c("regionB" = "region", "time" = "timestep", "model_id" = "model_id", "multiplier" = "multiplier"),
    suffix = c("_regionA", "_regionB")
  )

regions_grid <- regions_grid %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    sed = sed(c(idea_1_regionA, idea_2_regionA), c(idea_1_regionB, idea_2_regionB))
    #sed = sed(c(flat_regionA, mound_regionA), c(flat_regionB, mound_regionB))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    regionA, regionB, time, sed, model_id, multiplier
  )

save(regions_grid, file = "../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_sim_multiple.RData")
