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
  ) %>%
  # classification
  dplyr::mutate(
    # distance = dplyr::case_when(
    #   distance < 0.5 ~ 0,
    #   distance >= 0.5 & distance < 1.5 ~ 1,
    #   distance >= 1.5 & distance < 2.5 ~ 2,
    #   distance >= 2.5 ~ 3
    # )
    distance = base::cut(distance, seq(0, 4, 0.5), include.lowest = TRUE, right = FALSE)
  )

# remove duplicates
mn <- pmin(region_distances$regionA, region_distances$regionB)
mx <- pmax(region_distances$regionA, region_distances$regionB)
int <- as.numeric(interaction(mn, mx))
region_distances <- region_distances[match(unique(int), int),]

test <- regions_grid %>%
  # dplyr::filter(
  #   time == 800
  # ) %>%
  dplyr::mutate(
    regionA = as.character(regionA),
    regionB = as.character(regionB)
  ) 

test <- lapply(
  split(test, f = test$time),
  function(x) {
    mn <- pmin(x$regionA, x$regionB)
    mx <- pmax(x$regionA, x$regionB)
    int <- as.numeric(interaction(mn, mx))
    x <- x[match(unique(int), int),]
    return(x)
  }
) %>%
  do.call(rbind, .)

hu <- test %>% dplyr::left_join(
  region_distances, by = c("regionA", "regionB")
) %>% 
  dplyr::filter(
    sed != 0 & distance != 0
  )

hu <- hu %>%
  dplyr::mutate(
    relation = paste(regionA, "+", regionB)
  )

hu <- hu %>% 
  dplyr::mutate(
    time = base::cut(time, seq(800, 2200, 200), include.lowest = TRUE, right = FALSE)
  ) %>%
  dplyr::group_by(
    time, regionA, regionB, distance
  ) %>%
  dplyr::summarise(
    mean_sed = mean(sed, na.rm = TRUE)
  )
  
library(ggplot2)
ggplot(hu) +
  geom_boxplot(
    aes(x = distance, y = mean_sed)
  ) +
  geom_point(
    aes(x = distance, y = mean_sed, color = regionA),
    size = 5,
    position = position_nudge(x = -0.1)
  ) +
  geom_point(
    aes(x = distance, y = mean_sed, color = regionB),
    size = 5,
    position = position_nudge(x = 0.1)
  ) +
  facet_wrap(~time)

ggplot(hu) +
  geom_line(
    aes(x = time, y = sed, color = relation),
  ) +
  facet_grid(rows = vars(distance))

ggplot(hu) +
  geom_point(
    aes(x = distance, y = sed,),
    size = 1
  )

ggplot(hu) +
  geom_point(
    aes(x = distance, y = sed, color = regionA),
    size = 5,
    position = position_nudge(x = -0.1)
  ) +
  geom_point(
    aes(x = distance, y = sed, color = regionB),
    size = 5,
    position = position_nudge(x = 0.1)
  )
  