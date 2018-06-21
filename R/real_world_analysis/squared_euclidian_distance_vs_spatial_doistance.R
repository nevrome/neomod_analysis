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
mn <- pmin(region_distances$regionA, region_distances$regionB)
mx <- pmax(region_distances$regionA, region_distances$regionB)
int <- as.numeric(interaction(mn, mx))
region_distances <- region_distances[match(unique(int), int),]

test <- regions_grid %>%
  dplyr::filter(
    time == 800
  ) %>%
  dplyr::mutate(
    regionA = as.character(regionA),
    regionB = as.character(regionB)
  )

mn <- pmin(test$regionA, test$regionB)
mx <- pmax(test$regionA, test$regionB)
int <- as.numeric(interaction(mn, mx))
test <- test[match(unique(int), int),]

hu <- test %>% dplyr::left_join(
  region_distances, by = c("regionA", "regionB")
)

library(ggplot2)
ggplot(hu, aes(x = distance, y = sed, color = regionA)) +
  geom_jitter()
  