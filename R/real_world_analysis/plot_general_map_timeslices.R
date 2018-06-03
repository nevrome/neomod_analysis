load("../neomod_datapool/bronze_age/bronze2.RData")
land_outline <- sf::st_read("../neomod_datapool/geodata/land_shapes/ne_50m_land.shp")
countries <- sf::st_read("../neomod_datapool/geodata/country_areas/ne_50m_admin_0_countries.shp")
research_area <- sf::st_read("manually_changed_data/research_area.shp")
load("../neomod_datapool/bronze_age/regions.RData")

bronze2_slices <- bronze2 %>%
  dplyr::filter(
    age %in% seq(2200, 800, by = -200)
  ) %>%
  dplyr::mutate(
    age_slice = factor(age, levels = seq(2200, 800, by = -200))
  ) %>% sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )

ex <- raster::extent(research_area %>% sf::st_transform(sf::st_crs(102013)))
xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

library(ggplot2)
library(sf)

hu <- ggplot() +
  geom_sf(
    data = land_outline,
    fill = "white", colour = "black", size = 0.4
  ) +
  # geom_sf(
  #   data = regions,
  #   fill = NA, colour = "red", size = 0.5
  # ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "red", size = 0.5
  ) +
  geom_sf(
    data = bronze2_slices,
    mapping = aes(
      color = burial_type,
      shape = burial_construction,
      size = burial_construction,
      alpha = norm_dens
    ),
    show.legend = "point"
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = st_crs(102013)
  ) + 
  scale_shape_manual(
    values = c(
      "flat" = "\u268A",
      "mound" = "\u25E0",
      "unknown" = "\u2715"
    )
  ) +
  scale_size_manual(
    values = c(
      "flat" = 10,
      "mound" = 10,
      "unknown" = 5
    )
  ) +
  scale_color_manual(
    values = c(
      "cremation" = "#D55E00",
      "inhumation" = "#0072B2",
      "mound" = "#CC79A7",
      "flat" = "#009E73",
      "unknown" = "darkgrey"
    )
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "black", size = 0.3),
    strip.text.x = element_text(size = 20)
  ) +
  guides(
    color = guide_legend(title = "Burial type", override.aes = list(size = 10), nrow = 2, byrow = TRUE),
    shape = guide_legend(title = "Burial construction", override.aes = list(size = 10), nrow = 2, byrow = TRUE),
    size = FALSE,
    alpha = FALSE
  ) +
  facet_wrap(
    nrow = 3,
    ~age_slice
  )

hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/general_map_research_area_timeslices.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    #width = 210, height = 297, units = "mm",
    width = 350, height = 360, units = "mm",
    limitsize = F
  )
