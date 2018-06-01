#### burial type ####

load( "../neomod_datapool/bronze_age/development_proportions_burial_type.RData")

regions_factor <- as.factor(proportion_development_burial_type$region_name)
proportion_development_burial_type$region_name <- factor(regions_factor, levels = c(
  "Austria and Czech Republic",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

start_proportion_burial_type <- proportion_development_burial_type %>%
  dplyr::filter(timestep == 2200) %>%
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
  file = "../neomod_datapool/bronze_age/start_proportion_burial_type.RData"
)



#### burial construction ####

load("../neomod_datapool/bronze_age/development_proportions_burial_construction.RData")

regions_factor <- as.factor(proportion_development_burial_construction$region_name)
proportion_development_burial_construction$region_name <- factor(regions_factor, levels = c(
  "Austria and Czech Republic",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

start_proportion_burial_construction <- proportion_development_burial_construction %>%
  dplyr::filter(timestep == 2200) %>%
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
  file = "../neomod_datapool/bronze_age/start_proportion_burial_construction.RData"
)
