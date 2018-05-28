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
area <- as(area, 'Spatial')
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

load("../neomod_datapool/bronze_age/bronze1.RData")
bronze1 %<>% dplyr::select(-calage_density_distribution)

sp::coordinates(bronze1) <- ~lon+lat
sp::proj4string(bronze1) <- "+proj=longlat +datum=WGS84 +no_defs"

bronze1 %<>% spTransform("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")


hex_grid <- make_grid(area, cell_diameter = 520000, clip = TRUE)

plot(area, col = "grey50", bg = "light blue", axes = FALSE)
plot(hex_grid, border = "orange", add = TRUE)

plot(bronze1, add = TRUE)

hex_grid <- as(hex_grid, "SpatialPolygonsDataFrame")

plot(hex_grid)


