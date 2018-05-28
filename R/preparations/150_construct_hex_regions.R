research_area <- sf::st_read(
  "manually_changed_data/research_area.shp"
) %>% sf::st_transform(102013)

land_outline <- sf::st_read(
  "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
) %>%
  sf::st_crop(xmin = -20, ymin = 0, xmax = 40, ymax = 65) %>% 
  sf::st_transform(102013)

area <- sf::st_intersection(sf::st_buffer(land_outline, 0), research_area)

region_circles <- tibble::tibble(
  geometry = sf::st_make_grid(area, 400000, what = "centers"),
  region_id = 1:length(geometry)
  ) %>% sf::st_as_sf()
region_circles <- sf::st_intersection(region_circles, research_area)
region_circles %<>% sf::st_buffer(dist = 300000)

library(ggplot2)
ggplot() +
  geom_sf(data = area) +
  geom_sf(data = region_circles, fill = NA)

load("../neomod_datapool/bronze_age/bronze1.RData")
bronze1 %<>% sf::st_as_sf(coords = c("lon", "lat"))
sf::st_crs(bronze1) <- 4326
bronze1 %<>% sf::st_transform(102013)

schnu <- sf::st_intersection(bronze1, region_circles)

ggplot() +
  geom_sf(data = area) +
  geom_sf(data = region_circles, fill = NA) +
  geom_sf(data = schnu)

regions_with_enough_graves <- schnu %>% 
  dplyr::group_by(region_id) %>%
  dplyr::summarise(
    n = n()
  ) %>%
  dplyr::filter(
    n >= 100
  ) %$%
  region_id

selected_regions <- region_circles %>%
  dplyr::filter(region_id %in% regions_with_enough_graves)

ggplot() +
  geom_sf(data = area) +
  geom_sf(data = selected_regions, fill = NA) +
  geom_sf(data = schnu)

