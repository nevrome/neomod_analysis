load("data_analysis/squared_euclidian_distance_over_time_burial_type.RData")

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
    values = c(
      "Austria and Czechia" = "#999999", 
      "Poland" = "#ffe500", 
      "Southern Germany" = "#56B4E9", 
      "Northeastern France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Skandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7"
    )
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
    "/home/clemens/neomod/neomod_datapool/plots/sed/regions_regions_squared_euclidian_distance_burial_type.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 300, height = 300, units = "mm",
    limitsize = F
  )
