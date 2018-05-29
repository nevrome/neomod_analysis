#load("../neomod_datapool/bronze_age/space_and_network/land_outline_sf.RData")
load("../neomod_datapool/bronze_age/bronze1.RData")

land_outline <- sf::st_read(
  "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
)
countries <- sf::st_read(
  "../neomod_datapool/geodata/country_areas/ne_50m_admin_0_countries.shp"
)

rivers <- sf::st_read(
  "../neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_rivers_lake_centerlines_scale_rank.shp"
)

lakes <- sf::st_read(
  "../neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_lakes.shp"
)

load("../neomod_datapool/bronze_age/regions.RData")

research_area <- sf::st_read(
  "manually_changed_data/research_area.shp"
)

bronze1_sf <- bronze1 %>% sf::st_as_sf(
  coords = c("lon", "lat"),
  crs = 4326
)

library(ggplot2)
library(sf)

xlimit <- c(-1600000, 1300000)
ylimit <- c(800000, 3800000)

hu <- ggplot() +
  geom_sf(
    data = land_outline,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = rivers,
    fill = NA, colour = "black", size = 0.2
  ) +
  geom_sf(
    data = lakes,
    fill = NA, colour = "black", size = 0.2
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
    data = bronze1_sf,
    mapping = aes(
      color = burial_type,
      shape = burial_construction,
      size = burial_construction
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
      "flat" = 8,
      "mound" = 8,
      "unknown" = 4
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
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "black", size = 0.3)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 10)),
    shape = guide_legend(nrow = 2, byrow = TRUE)
  )

hu %>%
  ggsave(
    #"/home/clemens/neomod/neomod_datapool/bronze_age/disp_map.jpeg",
    "/home/clemens/neomod/neomod_datapool/bronze_age/disp_map_full.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    #width = 210, height = 297, units = "mm",
    width = 380, height = 350, units = "mm",
    limitsize = F
  )
