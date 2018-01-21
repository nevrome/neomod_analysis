load("../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")

library(ggplot2)

prop <- proportion_per_region_df %>% 
  # dplyr::filter(
  #   idea != "flat" & idea != "mound"
  # )
  dplyr::filter(
    idea != "inhumation" & idea != "cremation"
  )

prop$idea <- as.factor(prop$idea)
prop$idea <- factor(prop$idea , levels = rev(levels(prop$idea )))

hu <- ggplot() +
  geom_area(
    data = prop,
    aes(x = timestep, y = proportion, fill = idea),
    position = 'stack',
    #alpha = 0.6,
    linetype = "blank"
  ) +
  geom_line(
    # data = dplyr::filter(prop, idea == "cremation"),
    data = dplyr::filter(prop, idea == "flat"),
    mapping = aes(x = timestep, y = proportion),
    color = "black",
    size = 0.2
  ) +
  facet_wrap(~region_name, nrow = 11) +
  scale_x_reverse() +
  xlab("Time in years calBC") +
  labs(fill = "Memes (mutually exclusive)") + 
  theme_bw() +
  theme(
    legend.position="bottom",
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(colour = "black", size = 0.3)
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
  xlim(2700, 500)

region_file_list <- unique(prop$region_name) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
    img <- png::readPNG(paste0("../neomod_datapool/bronze_age/region_pictograms/", x, ".png"))
    g <- grid::rasterGrob(
      img, interpolate = TRUE,
      width = 0.06, height = 0.8
    )
  })
dummy <- tibble(region_name = unique(prop$region_name), grob = gl )

source("R/helper/geom_grob.R")

hu <- hu +
  geom_custom(
    data = dummy, 
    aes(grob = grob), 
    inherit.aes = FALSE,
    x = 0.07, y = 0.5
  )

hu %>%
  ggsave(
    # "/home/clemens/neomod/neomod_datapool/bronze_age/proportions_development_regions_cremation_inhumation.jpeg",
    "/home/clemens/neomod/neomod_datapool/bronze_age/proportions_development_regions_mound_flat.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )

