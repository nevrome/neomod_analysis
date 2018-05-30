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
  geometry = sf::st_make_grid(area, 400000, what = "centers", offset = c(-900000,-130000)),
  ID = 1:length(geometry)
  ) %>% sf::st_as_sf()
region_circles <- sf::st_intersection(region_circles, research_area)
region_circles %<>% sf::st_buffer(dist = 240000)

load("../neomod_datapool/bronze_age/bronze1.RData")
bronze1 %<>% sf::st_as_sf(coords = c("lon", "lat"))
sf::st_crs(bronze1) <- 4326
bronze1 %<>% sf::st_transform(102013)
gu <- sf::st_intersection(bronze1, research_area)

library(ggplot2)
ggplot() +
  geom_sf(data = area) +
  geom_sf(data = region_circles, fill = NA) +
  geom_sf(data = gu)

schnu <- sf::st_intersection(gu, region_circles)

ggplot() +
  geom_sf(data = area) +
  geom_sf(data = region_circles, fill = NA) +
  geom_sf(data = schnu)

regions_with_enough_graves <- schnu %>% 
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    n = n()
  ) %>%
  dplyr::filter(
    n >= 70
  ) %$%
  ID

regions <- region_circles %>%
  dplyr::filter(ID %in% regions_with_enough_graves)

ggplot() +
  geom_sf(data = area) +
  geom_sf(data = regions, fill = NA) +
  geom_sf(data = gu)

regions %>%
  dplyr::mutate(
    x = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
    y = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
  ) %>%
  ggplot() +
    geom_sf(data = area) +
    geom_sf(fill = NA) +
    geom_text(aes(x = x, y = y, label = ID))

regions$ID <- 1:nrow(regions)
regions$NAME <- c(
  "Northeast France", 
  "Southern Germany", 
  "Austria and Czech Republic", 
  "England", "Benelux", 
  "Northern Germany",
  "Poland",
  "Southern Skandinavia"
  )

regions %>%
  dplyr::mutate(
    x = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
    y = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
  ) %>%
  ggplot() +
  geom_sf(data = area) +
  geom_sf(fill = NA) +
  geom_text(aes(x = x, y = y, label = NAME))

save(regions, file = "../neomod_datapool/bronze_age/regions.RData")
