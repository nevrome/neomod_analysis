load("data_analysis/squared_euclidian_distance_over_timeblocks.RData")
load("data_analysis/mantel_sed_spatial_burial_type_burial_construction.RData")
load("data_analysis/region_order.RData")
load("data_analysis/region_colors.RData")

distance %<>%
  tidyr::spread(context, mean_sed)

library(ggplot2)
plu <- ggplot(distance) +
  geom_smooth(
    method = 'lm', 
    mapping = aes(burial_type, burial_construction),
    color = "black",
    se = FALSE,
    fullrange = TRUE,
    size = 0.5
  ) +
  geom_point(
    aes(x = burial_type, y = burial_construction, color = regionA),
    size = 4,
    position = position_nudge(x = -0.03)
  ) +
  geom_point(
    aes(x = burial_type, y = burial_construction, color = regionB),
    size = 4,
    position = position_nudge(x = 0.03)
  ) +
  geom_text(
    data = mantel_test_results,
    aes(
      label = paste0("Mantel Test r: ", round(statistic, 3), ", p: ", signif),
      colour = ifelse(signif < 0.1, "h0canberejected", "h0cannotberejected")
    ),
    x = 1.1, y = 2.2,
    size = 6
  ) +
  facet_wrap(~time, nrow = 2) +
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
  scale_color_manual(
    values = c(
      region_colors,
      "h0canberejected" = "red",
      "h0cannotberejected" = "black"
    ),
    breaks = region_order,
    labels = region_order
  ) +
  xlab("Squared Euclidian Distance Burial Type") +
  ylab("Squared Euclidian Distance Burial Construction") +
  ylim(0, 2.3) +
  guides(
    color = guide_legend(title = NULL, override.aes = list(size = 8, shape = 15), nrow = 2, byrow = TRUE),
    shape = FALSE,
    size = FALSE
  )

plu %>%
  ggsave(
    "figures_plots/sed/squared_euclidian_distance_burial_type_vs_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    #width = 210, height = 297, units = "mm",
    width = 550, height = 280, units = "mm",
    limitsize = F
  )
