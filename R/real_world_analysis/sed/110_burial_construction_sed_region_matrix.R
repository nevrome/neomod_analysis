load("data_analysis/squared_euclidian_distance_over_time_burial_construction.RData")
load("data_analysis/region_colors.RData")

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
    switch = "y",
    labeller = label_wrap_gen()
  ) +
  scale_x_continuous(
    breaks = c(-2000, -1500, -1000), 
    limits = c(-2200, -800)
  ) +
  scale_y_continuous(
    limits = c(0, 2)
  ) +
  theme_bw() +
  scale_color_manual(
    guide = FALSE,
    values = region_colors
  ) +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 9)
  ) +
  ylab("Squared Euclidian Distance") +
  xlab("Time")

hu %>%
  ggsave(
    "figures_plots/sed/regions_regions_squared_euclidian_distance_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 300, height = 300, units = "mm",
    limitsize = F
  )
