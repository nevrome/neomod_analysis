list.files("data_geo", pattern = "*.RData", full.names = T) %>% lapply(load, .GlobalEnv)
research_area <- sf::st_read("data_manually_prepared/research_area.shp")
load("data_analysis/regions.RData")
load("data_analysis/distance_matrix_spatial_long.RData")

region_centers <- regions %>%
  sf::st_centroid()

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

region_centers %>%
  sfc_as_cols() %>%
  dplyr::select(
    NAME, x, y
  )

save(region_centers, file = "data_analysis/region_centers.RData")

distance_lines <- distance_matrix_spatial_long %>%
  dplyr::left_join(
    region_centers,
    by = c("regionA" = "NAME")
  ) %>%
  dplyr::left_join(
    region_centers,
    by = c("regionB" = "NAME"),
    suffix = c("_regionA", "_regionB")
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x_a = st_coordinates(geometry_regionA)[,1],
    y_a = st_coordinates(geometry_regionA)[,2],
    x_b = st_coordinates(geometry_regionB)[,1],
    y_b = st_coordinates(geometry_regionB)[,2]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    regionA, regionB, distance, x_a, y_a, x_b, y_b
  ) %>%
  dplyr::filter(
    regionA != regionB
  )

# remove duplicates
mn <- pmin(distance_lines$regionA, distance_lines$regionB)
mx <- pmax(distance_lines$regionA, distance_lines$regionB)
int <- as.numeric(interaction(mn, mx))
distance_lines <- distance_lines[match(unique(int), int),]


library(ggplot2)
library(sf)

ex <- raster::extent(regions %>% sf::st_transform(sf::st_crs(102013)))

xlimit <- c(ex[1], ex[2])
ylimit <- c(ex[3], ex[4])

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
  geom_sf(
    data = research_area,
    fill = NA, colour = "red", size = 0.5
  ) +
  geom_curve(
    data = distance_lines,
    mapping = aes(
      x = x_a, y = y_a, xend = x_b, yend = y_b,
      size = distance    
    ), 
    alpha = 0.5,
    curvature = 0.2,
    colour = "black"
  ) +
  scale_size_continuous(
    name = "Spatial closeness",
    range = c(5, 0.5)
  ) +
  geom_sf(
    data = region_centers, 
    mapping = aes(
      colour = NAME
    ), 
    fill = NA, size = 16
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = st_crs(102013)
  ) + 
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    # axis.title = element_blank(),
    axis.text = element_text(size = 15),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "black", size = 0.3)
  ) +
  guides(
    colour = FALSE,
    shape = FALSE
  ) +
  xlab("") +
  ylab("") +
  scale_color_manual(
    values = c(
      "cremation" = "#D55E00",
      "inhumation" = "#0072B2",
      "mound" = "#CC79A7",
      "flat" = "#009E73",
      "unknown" = "darkgrey",
      "Austria and Czechia" = "#999999", 
      "Poland" = "#ffe500", 
      "Southern Germany" = "#56B4E9", 
      "Northeastern France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Skandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7"
    ),
    breaks = c(
      "Austria and Czechia",
      "Poland", 
      "Southern Germany", 
      "Northeastern France", 
      "Northern Germany", 
      "Southern Skandinavia", 
      "Benelux", 
      "England"
    ),
    labels = c(
      "Austria and Czechia",
      "Poland", 
      "Southern Germany", 
      "Northeastern France", 
      "Northern Germany", 
      "Southern Skandinavia", 
      "Benelux", 
      "England"
    )
  )

hu %>%
  ggsave(
    "figures_plots/general_maps/general_map_distance_network.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 350, height = 320, units = "mm",
    limitsize = F
  )
