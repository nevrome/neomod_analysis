# get research area border
research_area_border <- rgdal::readOGR(
  dsn = "../neomod_datapool/bronze_age/research_area/research_area.shp"
)

# load already present data
load("../neomod_datapool/bronze_age/research_area/research_area_hex.RData")
load("../neomod_datapool/bronze_age/research_area/hex_graph_egdes.RData")
load("../neomod_datapool/bronze_age/research_area/hex_graph_nodes_no_ioi.RData")

wgs <- 4326

# load and clip rivers and lake shapes
rivers <- raster::shapefile(
  "../neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_rivers_lake_centerlines_scale_rank"
) %>% rgeos::gIntersection(
  ., research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

lakes <- raster::shapefile(
  "../neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_lakes"
) %>% rgeos::gIntersection(
  ., research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

# intersect rivers and lakes with hexagons

rivers_hex <- rivers %>% raster::intersect(
  research_area_hex, .
) %>%
  as(., "SpatialPolygonsDataFrame")

# plot(research_area_hex)
# plot(rivers_hex, col = "red", add = T)

lakes_hex <- as(lakes, 'SpatialLines') %>% raster::intersect(
  research_area_hex, .
) %>%
  as(., "SpatialPolygonsDataFrame")

# plot(research_area_hex)
# plot(lakes_hex, col = "red", add = T)

# combine river and lake hex fields 

rivers_lakes_hex <- rbind(rivers_hex, lakes_hex)

# get hex ideas with rivers and nodes

river_lake_nodes_ids <- rivers_lakes_hex %>%
  as(., "SpatialPolygons") %>%
  ggplot2::fortify(region = "id") %$%
  unique(id)

# add info if they are part of the river-lake network to nodes

nodes_resistance <- nodes %>% 
  # calculate resistance value from start
  dplyr::mutate(
    river_lakes = ifelse(.$id %in% river_lake_nodes_ids, TRUE, FALSE)
  )

# load and crop elevation raster
# from: https://topotools.cr.usgs.gov/gmted_viewer/
elevation_world <- raster::raster(
  "../neomod_datapool/geodata/world_elevation_raster/mn30_grd/hdr.adf"
)

elevation_research_area <- raster::crop(
  elevation_world,
  research_area_border
)

elevation_research_area_small <- raster::aggregate(
  elevation_research_area, fact = 50
)

mean_elevation_per_hex <- raster::extract(
  x = elevation_research_area_small,
  y = research_area_hex,
  fun = mean,
  df = TRUE
)

diffy <- function(x, ...) {
  diff(range(x))
}

range_elevation_per_hex <- raster::extract(
  x = elevation_research_area_small,
  y = research_area_hex,
  fun = diffy,
  df = TRUE
)

elevation_per_hex <- dplyr::full_join(
  mean_elevation_per_hex, range_elevation_per_hex, by = "ID"
) %>% dplyr::rename(
  "elevation" = "hdr.x",
  "range" = "hdr.y"
)

high_hex_selection <- elevation_per_hex %>%
  dplyr::filter(
    elevation >= 1000 | range > 700
  ) %$%
  ID

# plot(research_area_hex)
# plot(research_area_hex[high_hex], col = "red", add = T)

high_hex <- research_area_hex[high_hex_selection]

# get hex ids with mountains

high_hex_nodes_ids <- high_hex %>%
  ggplot2::fortify(region = "id") %$%
  unique(id)

# add info if they are part of the river-lake network to nodes

nodes_resistance <- nodes_resistance %>% 
  # calculate resistance value from start
  dplyr::mutate(
    mountains = ifelse(.$id %in% high_hex_nodes_ids, TRUE, FALSE)
  )


###

# add resistance to edges 
edges %<>% dplyr::mutate(
  spatial_dist = distance,
  distance = purrr::map2_dbl(from, to, function(x, y){
    rl1 <- nodes_resistance[which(x == nodes_resistance$name), ]$river_lakes
    rl2 <- nodes_resistance[which(y == nodes_resistance$name), ]$river_lakes
    m1 <- nodes_resistance[which(x == nodes_resistance$name), ]$mountains
    m2 <- nodes_resistance[which(y == nodes_resistance$name), ]$mountains
    if (rl1 && rl2) {
      res <- 1
    } else if (m1 | m2) {
      res <- 3
    } else (
      res <- 2
    )
    return(res)
  }
  )
)

save(edges, file = "../neomod_datapool/bronze_age/research_area/hex_graph_egdes.RData")
