load("R/simulation_results/sim1.RData")

library(ggplot2)

prop <- models_grid$idea_proportions %>%
  do.call(rbind, .)

prop$idea <- as.factor(prop$idea)
#prop$idea <- factor(prop$idea , levels = rev(levels(prop$idea )))

hu <- ggplot() +
  geom_line(
    data = dplyr::filter(prop, idea == "idea_1"),
    mapping = aes(
      x = timestep, 
      y = proportion,
      color = model_id,
      group = model_id
    ),
    size = 0.2
  ) +
  scale_alpha_continuous(range = c(0.0, 0.7)) +
  facet_wrap(~region, nrow = 8) +
  xlab("Simulation run time in years") +
  ylab("Proportion of artificial humans") +
  labs(fill = "Ideas (mutually exclusive)") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    values = c(
      "idea_1" = "#f0e442",
      "idea_2" = "#56b4e9"
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

region_file_list <- unique(prop$region) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("../neomod_datapool/bronze_age/region_pictograms_colour/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.14, height = 1.2
  )
})
dummy <- tibble::tibble(region = unique(prop$region), grob = gl )

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
    "/home/clemens/neomod/neomod_datapool/bronze_age/development_proportions_regions_simulation.jpeg",
    #"/home/clemens/neomod/neomod_datapool/bronze_age/development_proportions_regions_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )


