list.files("data_geo", pattern = "*.RData", full.names = T) %>% lapply(load, .GlobalEnv)
research_area <- sf::st_read("data_manually_prepared/research_area.shp")
load("data_analysis/bronze1.RData")
load("data_analysis/regions.RData")
load("data_analysis/region_order.RData")
load("data_analysis/region_colors.RData")

regions_name_points <- regions %>%
  dplyr::mutate(
    x = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
    y = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
  )

bronze1_sf <- bronze1 %>% sf::st_as_sf(
  coords = c("lon", "lat"),
  crs = 4326
)

library(ggplot2)
library(sf)

ex <- raster::extent(research_area %>% sf::st_transform(sf::st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")))

xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

hu <- ggplot() +
  geom_sf(
    data = land_outline,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = countries,
    fill = NA, colour = "black", size = 0.2
  ) +
  geom_sf(
    data = regions,
    mapping = aes(
      colour = NAME
    ),
    fill = NA, size = 2.5
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
    crs = st_crs("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
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
      "unknown" = "darkgrey",
      region_colors
    ),
    breaks = region_order,
    labels = region_order
  ) +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 17),
    panel.grid.major = element_line(colour = "black", size = 0.3)
  ) +
  guides(
    color = guide_legend(title = NULL, override.aes = list(size = 8, shape = 15), nrow = 2, byrow = TRUE),
    shape = FALSE,
    size = FALSE
  )

hu %>%
  ggsave(
    "figures_plots/general_maps/general_map_regions_countries.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 350, height = 320, units = "mm",
    limitsize = F
  )
