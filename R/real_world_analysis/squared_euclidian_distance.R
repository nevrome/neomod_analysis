load("../neomod_datapool/bronze_age/development_proportions_burial_type.RData")
load("../neomod_datapool/bronze_age/development_proportions_burial_construction.RData")

proportion_development_burial_type %>% tibble::as.tibble()

# proportion_development_burial_type %>%
#   tidyr::complete(
#     region_name, timestep, idea,
#     fill = list(proportion = as.integer(0))
#   )

by_regions <-  base::split(proportion_development_burial_type, proportion_development_burial_type$region_name)

by_regions_and_time <- lapply(
  by_regions,
  function(x) {
    base::split(x, x$timestep)
  }
)

regions <- proportion_development_burial_type$region_name %>% unique()
timesteps <- proportion_development_burial_type$timestep %>% unique()
regions_grid <- expand.grid(regionA = regions, regionB = regions, time = timesteps, stringsAsFactors = FALSE)


sed <- function(pi, pj) {
  pi <- pi / sum(pi)
  pj <- pj / sum(pj)
  sum((pi - pj)^2)
}

for (p1 in 1:nrow(regions_grid)) {
  p_region_A <- by_regions_and_time[[regions_grid[p1, 1]]][[as.character(regions_grid[p1, 3])]]$proportion
  p_region_B <- by_regions_and_time[[regions_grid[p1, 2]]][[as.character(regions_grid[p1, 3])]]$proportion
  regions_grid$sed[p1] <- sed(p_region_A, p_region_B)
}

library(ggplot2)
regions_grid %>%
  ggplot() +
  geom_line(aes(time, sed)) +
  facet_grid(
    regionA ~ regionB
  )
  

