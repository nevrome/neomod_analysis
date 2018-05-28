research_area <- sf::st_read(
  "manually_changed_data/research_area.shp"
) %>% sf::st_transform(102013)

land_outline <- sf::st_read(
  "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
) %>%
  sf::st_crop(xmin = -20, ymin = 0, xmax = 40, ymax = 65) %>% 
  sf::st_transform(102013)

area <- sf::st_intersection(sf::st_buffer(land_outline, 0), research_area)

library(sp)
library(raster)
library(rgeos)
area_sp <- as(area, 'Spatial')
make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}

set.seed(19)
hex_grid <- make_grid(area_sp, cell_diameter = 520000, clip = FALSE)

hex_grid %<>% sf::st_as_sf() 

load("../neomod_datapool/bronze_age/bronze1.RData")
bronze1 %<>% sf::st_as_sf(coords = c("lon", "lat"))
sf::st_crs(bronze1) <- 4326

library(ggplot2)
ggplot() +
  geom_sf(data = area) +
  geom_sf(data = hex_grid, fill = NA) +
  geom_sf(data = bronze1)


