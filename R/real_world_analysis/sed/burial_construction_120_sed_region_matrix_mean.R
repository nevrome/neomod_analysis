load("data_analysis/squared_euclidian_distance_over_time_burial_construction.RData")

regions_grid_mean <- regions_grid %>%
  dplyr::group_by(
    regionA, regionB
  ) %>%
  dplyr::summarise(
    mean_sed = mean(sed, na.rm = T)
  )

regions_grid_mean$regionB <- forcats::fct_rev(regions_grid_mean$regionB)

save(regions_grid_mean, file = "data_analysis/regions_mean_sed_burial_construction.RData")

kur <- regions_grid_mean %>%
  ggplot() +
  geom_raster(
    aes(
      x = regionA,
      y = regionB,
      fill = mean_sed
    )
  ) +
  geom_text(
    aes(
      x = regionA,
      y = regionB,
      label = round(mean_sed, 2)
    )
  ) +
  scale_x_discrete(position = "top") +
  scale_fill_continuous(
    guide = FALSE,
    high = "#6d6d6d", low = "#FFFFFF"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 0),
    axis.text.y = element_text(size = 10, angle = 45, vjust = 0, hjust = 1),
    axis.title = element_blank()
  )

kur %>%
  ggsave(
    "figures_plots/sed/regions_regions_mean_squared_euclidian_distance_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 200, height = 200, units = "mm",
    limitsize = F
  )
