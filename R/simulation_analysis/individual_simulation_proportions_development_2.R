load("R/simulation_results/sim1.RData")

prop <- models_grid[1:10, ]$idea_proportions %>%
  do.call(rbind, .)

long_prop <- prop %>%
  tidyr::spread(
    idea, proportion
  )

library(ggplot2)
fu <- ggplot(long_prop) +
  # geom_line(
  #   aes(
  #     x = timestep,
  #     y = idea_1,
  #     colour = region
  #   )
  # ) +
  geom_smooth(
    aes(
      x = timestep,
      y = idea_1,
      colour = region
    ),
    method = "loess",
    span = 0.1
  ) +
  facet_wrap(
    ~model_id,
    nrow = 5,
    ncol = 2
  ) +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Austria and Czechia" = "#999999", 
      "Poland" = "#E69F00", 
      "Southern Germany" = "#56B4E9", 
      "Northeast France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Skandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7"
    )
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  )

fu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/development_proportions_regions_simulation_2.jpeg",
    #"/home/clemens/neomod/neomod_datapool/bronze_age/development_proportions_regions_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )
