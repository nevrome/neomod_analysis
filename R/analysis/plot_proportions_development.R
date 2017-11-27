load("../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")

library(ggplot2)

hu <- proportion_per_region_df %>%
  ggplot() +
  geom_line(
    aes(x = timestep, y = proportion, color = ideas, linetype = ideas, alpha = ideas)
  ) +
  facet_wrap(~vertices) +
  scale_x_reverse() +
  scale_linetype_manual(
    values = c(
      "cremation" = "solid",
      "inhumation" = "solid",
      "flat" = "dashed",
      "mound" = "dashed"
    )
  ) +
  scale_alpha_manual(
    values = c(
      "cremation" = 1,
      "inhumation" = 0.3,
      "flat" = 1,
      "mound" = 0.3
    )
  ) +
  theme_bw()
  
hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/proportions_development_regions.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 297, height = 210, units = "mm",
    limitsize = F
  )
