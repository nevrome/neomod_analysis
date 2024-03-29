load("data_analysis/development_proportions_burial_type.RData")

library(ggplot2)

prop <- proportion_development_burial_type
  
prop$idea <- as.factor(prop$idea)
prop$idea <- factor(prop$idea , levels = rev(levels(prop$idea )))

hu <- ggplot() +
  geom_area(
    data = prop,
    mapping = aes(x = timestep, y = proportion, fill = idea),
    position = 'stack',
    linetype = "blank"
    #alpha = 0.6
  ) +
  # geom_line(
  #   data = dplyr::filter(prop, idea == "cremation"),
  #   mapping = aes(x = timestep, y = proportion),
  #   color = "black",
  #   size = 0.2
  # ) +
  # geom_rect(
  #   aes(NULL, NULL, xmin = start, xmax = end),
  #   ymin = 0, ymax = 1, 
  #   fill = "black", 
  #   alpha = 0.5,
  #   color = NA,
  #   data = amount_devel
  # ) +
  scale_alpha_continuous(range = c(0.0, 0.7)) +
  facet_wrap(~region_name, nrow = 8) +
  xlab("Time in years calBC") +
  ylab("Proportion of 14C dates from burials") +
  labs(fill = "Ideas (mutually exclusive)") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    values = c(
      "cremation" = "#D55E00",
      "inhumation" = "#0072B2",
      "mound" = "#CC79A7",
      "flat" = "#009E73"
    )
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%")
  ) +
  scale_x_continuous(
    breaks = c(-2200, -2000, -1500, -1000, -800), 
    limits = c(-2500, -800)
  )



region_file_list <- unique(prop$region_name) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("figures_plots/region_pictograms_colour/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.14, height = 1.2
  )
})
dummy <- tibble::tibble(region_name = unique(prop$region_name), grob = gl )

source("R/helper_functions/geom_grob.R")

hu <- hu +
  geom_custom(
    data = dummy, 
    aes(grob = grob), 
    inherit.aes = FALSE,
    x = 0.1, y = 0.5
  )

hu %>%
  ggsave(
    "figures_plots/development/development_proportions_regions_burial_type.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )

