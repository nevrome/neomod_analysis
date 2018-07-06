load("../neomod_datapool/bronze_age/regions.RData")
load("../neomod_datapool/bronze_age/regions_mean_sed")

land_outline <- sf::st_read("../neomod_datapool/geodata/land_shapes/ne_50m_land.shp")
rivers <- sf::st_read("../neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_rivers_lake_centerlines_scale_rank.shp")
lakes <- sf::st_read("../neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_lakes.shp")
research_area <- sf::st_read("manually_changed_data/research_area.shp")

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

# regions_sed_lines <- regions_grid_mean %>%
#   dplyr::left_join(
#     region_centers,
#     by = c("regionA" = "NAME")
#   ) %>%
#   dplyr::left_join(
#     region_centers,
#     by = c("regionB" = "NAME"),
#     suffix = c("_regionA", "_regionB")
#   ) %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(
#     geometry = st_cast(
#       sf::st_combine(c(geometry_regionA, geometry_regionB)),
#       "LINESTRING"
#     )
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(
#     regionA, regionB, mean_sed, geometry
#   ) %>%
#   sf::st_as_sf(crs = st_crs(102013))

mean_sed <- regions_grid_mean %>%
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
    regionA, regionB, mean_sed, x_a, y_a, x_b, y_b
  ) %>%
  dplyr::filter(
    regionA != regionB
  )

# remove duplicates
mn <- pmin(mean_sed$regionA, mean_sed$regionB)
mx <- pmax(mean_sed$regionA, mean_sed$regionB)
int <- as.numeric(interaction(mn, mx))
mean_sed <- mean_sed[match(unique(int), int),]


library(ggplot2)
library(sf)

ex <- raster::extent(research_area %>% sf::st_transform(sf::st_crs(102013)))

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
  # geom_sf(
  #   data = regions,
  #   fill = NA, colour = "red", size = 0.5
  # ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "red", size = 0.5
  ) +
  geom_curve(
    data = mean_sed,
    mapping = aes(
      x = x_a, y = y_a, xend = x_b, yend = y_b,
      color = regionA, size = mean_sed    
    ), 
    alpha = 0.5,
    curvature = 0.2
  ) +
  # scale_color_manual(
  #   #values = c("#999999", "#ffe500", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00", "#CC79A7")
  # ) +
  scale_size_continuous(
    range = c(5, 0.5)
  ) +
  geom_sf(
    data = region_centers,
    fill = NA, colour = "red", size = 0.5
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
    color = FALSE,
    shape = FALSE,
    size = FALSE
  )

hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/general_map_squared_euclidian_distance.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 350, height = 360, units = "mm",
    limitsize = F
  )
