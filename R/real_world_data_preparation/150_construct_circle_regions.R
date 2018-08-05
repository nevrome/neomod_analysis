#### load spatial data ####

load("data_analysis/research_area.RData")
load("data_analysis/area.RData")

#### define region circles ####

region_circles <- tibble::tibble(
  geometry = sf::st_make_grid(area, 400000, what = "centers", offset = c(-900000,-130000)),
  ID = 1:length(geometry)
  ) %>% sf::st_as_sf()
region_circles <- sf::st_intersection(region_circles, research_area)
region_circles %<>% sf::st_buffer(dist = 240000)

load("data_analysis/bronze1.RData")
bronze1 %<>% sf::st_as_sf(coords = c("lon", "lat"))
sf::st_crs(bronze1) <- 4326
bronze1 %<>% sf::st_transform(102013)
gu <- sf::st_intersection(bronze1, research_area)

# library(ggplot2)
# ggplot() +
#   geom_sf(data = area) +
#   geom_sf(data = region_circles, fill = NA) +
#   geom_sf(data = gu)

schnu <- sf::st_intersection(gu, region_circles)

# ggplot() +
#   geom_sf(data = area) +
#   geom_sf(data = region_circles, fill = NA) +
#   geom_sf(data = schnu)

number_of_dates_per_circle <- schnu %>% 
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    n = n()
  )

regions_with_enough_graves <- number_of_dates_per_circle %>%
  dplyr::filter(
    n >= 70
  ) %$%
  ID

dates_per_region <- schnu %>% dplyr::filter(
  ID %in% regions_with_enough_graves
) %>% sf::st_set_geometry(NULL)

save(
  dates_per_region,
  file = "data_analysis/dates_per_region.RData"
)

regions <- region_circles %>%
  dplyr::mutate(
    number_of_dates = number_of_dates_per_circle$n
  ) %>%
  dplyr::filter(ID %in% regions_with_enough_graves)


# ggplot() +
#   geom_sf(data = area) +
#   geom_sf(data = regions, fill = NA) +
#   geom_sf(data = gu)

# regions %>%
#   dplyr::mutate(
#     x = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
#     y = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
#   ) %>%
#   ggplot() +
#     geom_sf(data = area) +
#     geom_sf(fill = NA) +
#     geom_text(aes(x = x, y = y, label = ID))

regions$ID <- 1:nrow(regions)
regions$NAME <- c(
  "Northeast France", 
  "Southern Germany", 
  "Austria and Czechia", 
  "England", "Benelux", 
  "Northern Germany",
  "Poland",
  "Southern Skandinavia"
  )

# regions %>%
#   dplyr::mutate(
#     x = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[1]]),
#     y = purrr::map_dbl(geometry, ~sf::st_centroid(.x)[[2]])
#   ) %>%
#   ggplot() +
#   geom_sf(data = area) +
#   geom_sf(fill = NA) +
#   geom_text(aes(x = x, y = y, label = NAME))

save(regions, file = "data_analysis/regions.RData")

