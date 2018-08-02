#### burial type ####

load( "data_analysis/development_proportions_burial_type.RData")

start_proportion_burial_type <- proportion_development_burial_type %>%
  dplyr::filter(timestep == -2200) %>%
  tidyr::spread(idea, proportion) %>%
  dplyr::arrange(region_name) %>%
  magrittr::set_rownames(.$region_name) %>%
  dplyr::select(cremation, inhumation) %>%
  dplyr::rename(
    idea_1 = cremation,
    idea_2 = inhumation
  )

save(
  start_proportion_burial_type,
  file = "data_analysis/start_proportion_burial_type.RData"
)



#### burial construction ####

load("data_analysis/development_proportions_burial_construction.RData")

start_proportion_burial_construction <- proportion_development_burial_construction %>%
  dplyr::filter(timestep == -2200) %>%
  tidyr::spread(idea, proportion) %>%
  dplyr::arrange(region_name) %>%
  magrittr::set_rownames(.$region_name) %>%
  dplyr::select(flat, mound) %>%
  dplyr::rename(
    idea_1 = flat,
    idea_2 = mound
  )

save(
  start_proportion_burial_construction,
  file = "data_analysis/start_proportion_burial_construction.RData"
)
