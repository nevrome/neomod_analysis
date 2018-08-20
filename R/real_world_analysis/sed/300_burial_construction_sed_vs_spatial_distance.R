load("data_analysis/mantel_sed_spatial_burial_construction.RData")
load("data_analysis/squared_euclidian_distance_over_timeblocks_burial_construction.RData")

library(ggplot2)
plu <- ggplot(sed_spatial_distance) +
  geom_boxplot(
    aes(x = distance, y = mean_sed, group = distance),
    width = 0.3
  ) +
  geom_point(
    aes(x = distance, y = mean_sed, color = regionA),
    size = 4,
    position = position_nudge(x = -0.4)
  ) +
  geom_point(
    aes(x = distance, y = mean_sed, color = regionB),
    size = 4,
    position = position_nudge(x = -0.31)
  ) +
  geom_text(
    data = mantel_test_results,
    aes(
      label = paste0("Mantel Test r: ", round(statistic, 3), ", p: ", signif),
      colour = ifelse(signif < 0.05, "h0canberejected", "h0cannotberejected")
    ),
    x = 2.7, y = 2.2,
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
      "Southeastern Central Europe" = "#999999", 
      "Poland" = "#ffe500", 
      "Southern Germany" = "#56B4E9", 
      "Northeastern France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Scandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7",
      "h0canberejected" = "red",
      "h0cannotberejected" = "black"
    ),
    breaks = c(
      "Southeastern Central Europe",
      "Poland", 
      "Southern Germany", 
      "Northeastern France", 
      "Northern Germany", 
      "Southern Scandinavia", 
      "Benelux", 
      "England"
    ),
    labels = c(
      "Southeastern Central Europe",
      "Poland", 
      "Southern Germany", 
      "Northeastern France", 
      "Northern Germany", 
      "Southern Scandinavia", 
      "Benelux", 
      "England"
    )
  ) +
  xlab("Spatial Distance Classes") +
  ylab("Squared Euclidian Distance") +
  ylim(0, 2.3)

plu %>%
  ggsave(
    "figures_plots/sed/squared_euclidian_distance_vs_spatial_distance_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 550, height = 280, units = "mm",
    limitsize = F
  )
  
