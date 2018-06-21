load("../neomod_datapool/bronze_age/regions.RData")
load("../neomod_datapool/bronze_age/squared_euclidian_distance_over_time.RData")

region_centers <- regions %>%
  sf::st_centroid()

region_distances <- region_centers %>% 
  sf::st_distance() %>%
  magrittr::divide_by(min(.[. != min(.)])) %>%
  tibble::as.tibble() %>%
  magrittr::set_colnames(region_centers$NAME) %>%
  dplyr::mutate(regionA = region_centers$NAME) %>%
  tidyr::gather(key = regionB, value = distance, -regionA) %>%
  dplyr::mutate(
    distance = as.double(distance)
  )

# remove duplicates
mn <- pmin(region_centers$regionA, region_centers$regionB)
mx <- pmax(region_centers$regionA, region_centers$regionB)
int <- as.numeric(interaction(mn, mx))
region_centers <- region_centers[match(unique(int), int),]

regions_grid %>%
  dplyr::filter(
    time == 800
  ) %>%
  dplyr::

spatial_distance_matrix <- 
  
  
  