research_area <- sf::st_read(
  "manually_changed_data/research_area.shp"
) %>% sf::st_transform(102013)

land_outline <- sf::st_read(
  "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
) %>%
  sf::st_crop(xmin = -20, ymin = 0, xmax = 40, ymax = 65) %>% 
  sf::st_transform(102013)

area <- sf::st_intersection(sf::st_buffer(land_outline, 0), research_area)

hu <- tibble::tibble(geometry = sf::st_make_grid(area, 400000, what = "centers")) %>% sf::st_as_sf()

hu <- sf::st_intersection(hu, research_area)

hu %<>% sf::st_buffer(dist = 300000)

load("../neomod_datapool/bronze_age/bronze1.RData")
bronze1 %<>% sf::st_as_sf(coords = c("lon", "lat"))
sf::st_crs(bronze1) <- 4326

library(ggplot2)
ggplot() +
  geom_sf(data = area) +
  geom_sf(data = hu, fill = NA) +
  geom_sf(data = bronze1)





