load("../neomod_datapool/bronze_age/development_proportions_burial_type.RData")
load("../neomod_datapool/bronze_age/development_proportions_burial_construction.RData")

prop <- proportion_development_burial_type
# prop <- proportion_development_burial_construction

# proportion_development_burial_type %>%
#   tidyr::complete(
#     region_name, timestep, idea,
#     fill = list(proportion = as.integer(0))
#   )

by_regions <-  base::split(prop, prop$region_name)

by_regions_and_time <- lapply(
  by_regions,
  function(x) {
    base::split(x, x$timestep)
  }
)

regions <- prop$region_name %>% unique()
timesteps <- prop$timestep %>% unique()
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


regions_factorA <- as.factor(regions_grid$regionA)
regions_grid$regionA <- factor(regions_factorA, levels = c(
  "Austria and Czech Republic",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

regions_factorB <- as.factor(regions_grid$regionB)
regions_grid$regionB <- factor(regions_factorB, levels = c(
  "Austria and Czech Republic",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

library(ggplot2)
hu <- regions_grid %>%
  ggplot() +
  geom_line(
    aes(time, sed),
    alpha = 0.3
  ) +
  geom_smooth(
    aes(time, sed, color = regionB),
    method = "loess",
    span = 0.3
  ) +
  facet_grid(
    regionA ~ regionB,
    switch = "y"
  ) +
  scale_x_reverse() +
  theme_bw() +
  scale_color_discrete(guide = FALSE)
  
hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/regions_squared_euclidian_distance_burial_type.jpeg",
    #"/home/clemens/neomod/neomod_datapool/bronze_age/development_proportions_regions_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 300, height = 300, units = "mm",
    limitsize = F
  )

regions_grid %>%
  ggplot() +
  geom_smooth(
    aes(time, sed, color = regionB),
    method = "loess",
    span = 0.3
  ) +
  facet_grid(
    regionA~.,
    switch = "y"
  ) +
  scale_x_reverse() +
  theme_bw() +
  scale_color_discrete(guide = FALSE)

