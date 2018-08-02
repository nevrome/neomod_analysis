load("data_simulation/sim1.RData")

prop <- models_grid$idea_proportions[[4]]

prop <- prop %>%
  dplyr::transmute(
    region_name = as.factor(region),
    timestep = timestep,
    idea = idea,
    proportion = proportion
  )

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
    sed = sed(c(idea_1_regionA, idea_2_regionA), c(idea_1_regionB, idea_2_regionB))
    #sed = sed(c(flat_regionA, mound_regionA), c(flat_regionB, mound_regionB))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    regionA, regionB, time, sed
  )

save(regions_grid, file = "../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_sim1.RData")

library(ggplot2)
hu <- regions_grid %>%
  ggplot() +
  geom_line(
    aes(time, sed),
    alpha = 0.3,
    size = 0.5
  ) +
  geom_smooth(
    aes(time, sed, color = regionB),
    method = "loess",
    span = 0.3,
    size = 1.5
  ) +
  facet_grid(
    regionA ~ regionB,
    switch = "y"
  ) +
  scale_x_continuous(
    breaks = c(-2000, -1500, -1000), 
    limits = c(-2200, -800)
  ) +
  theme_bw() +
  scale_color_manual(
    guide = FALSE,
    values = c("#999999", "#ffe500", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00", "#CC79A7")
  ) +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 9)
  ) +
  ylab("Sqared Euclidian Distance") +
  xlab("Time")

hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/regions_regions_squared_euclidian_distance_sim1.jpeg",
    #"/home/clemens/neomod/neomod_datapool/bronze_age/regions_regions_squared_euclidian_distance_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 300, height = 300, units = "mm",
    limitsize = F
  )
