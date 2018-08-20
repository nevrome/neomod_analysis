load("data_analysis/squared_euclidian_distance_over_time_burial_construction.RData")

schnu <- regions_grid %>%
  ggplot() +
  geom_smooth(
    aes(time, sed, color = regionB),
    method = "loess",
    span = 0.3
  ) +
  facet_wrap(
    ~regionA, 
    nrow = 8
  ) +
  scale_x_continuous(
    breaks = c(-2200, -2000, -1500, -1000, -800), 
    limits = c(-2500, -800)
  ) +
  scale_y_continuous(
    limits = c(0, 2)
  ) +
  theme_bw() +
  scale_color_manual(
    guide = FALSE,
    values = c(
      "Southeastern Central Europe" = "#999999", 
      "Poland" = "#ffe500", 
      "Southern Germany" = "#56B4E9", 
      "Northeastern France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Scandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7"
    )
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  ylab("Squared Euclidian Distance") +
  xlab("Time")


region_file_list <- unique(regions_grid$regionA) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("figures_plots/region_pictograms_colour/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.14, height = 1.2
  )
})
dummy <- tibble::tibble(regionA = unique(regions_grid$regionA), grob = gl )

source("R/helper_functions/geom_grob.R")

schnu <- schnu +
  geom_custom(
    data = dummy, 
    aes(grob = grob), 
    inherit.aes = FALSE,
    x = 0.1, y = 0.5
  )

schnu %>%
  ggsave(
    "figures_plots/sed/regions_squared_euclidian_distance_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )

