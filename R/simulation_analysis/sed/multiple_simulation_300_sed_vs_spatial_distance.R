load("../neomod_datapool/simulation_data/squared_euclidian_distance_over_timeblocks_multiple_simulations.RData")
sed_simulation <- sed_spatial_distance
load("../neomod_datapool/R_data/squared_euclidian_distance_over_timeblocks_burial_type.RData")
sed_burial_type <- sed_spatial_distance %>% dplyr::mutate(context = "burial_type")
load("../neomod_datapool/R_data/squared_euclidian_distance_over_timeblocks_burial_construction.RData")
sed_burial_construction <-sed_spatial_distance %>% dplyr::mutate(context = "burial_construction")

library(ggplot2)
plu <- ggplot() +
  geom_boxplot(
    data = sed_simulation,
    mapping = aes(
      x = as.factor(distance), 
      y = mean_sed
    ),
    width = 0.3
  ) +
  geom_point(
    data = sed_burial_type,
    mapping  = aes(
      x = as.factor(distance), 
      y = mean_sed,
      colour = context
    ),
    size = 4,
    position = position_nudge(x = -0.3)
  ) +
  geom_point(
    data = sed_burial_construction,
    mapping  = aes(
      x = as.factor(distance), 
      y = mean_sed,
      colour = context
    ),
    size = 4,
    position = position_nudge(x = -0.5)
  ) +
  facet_wrap(
    nrow = 2,
    ~time
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  ) +
  xlab("Spatial Distance Classes") +
  ylab("Squared Euclidian Distance") +
  NULL

plu %>%
  ggsave(
    "../neomod_datapool/plots/sed_simulation/squared_euclidian_distance_vs_spatial_distance_sim_multiple.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 550, height = 280, units = "mm",
    limitsize = F
  )
  
