load("../neomod_datapool/bronze_age/regions.RData")
load("../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_burial_type.RData")

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
    distance = base::cut(
      distance, 
      seq(0, 4, 0.5), paste(seq(0, 3.5, 0.5), seq(0.5, 4.0, 0.5), sep = "-"),
      include.lowest = TRUE, 
      right = FALSE)
  )

# remove duplicates
mn <- pmin(region_distances$regionA, region_distances$regionB)
mx <- pmax(region_distances$regionA, region_distances$regionB)
int <- as.numeric(interaction(mn, mx))
region_distances <- region_distances[match(unique(int), int),]

test <- regions_grid %>%
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
  ) %>%
  dplyr::mutate(
    relation = paste(regionA, "+", regionB),
    time = base::cut(
      time, 
      seq(800, 2200, 200), labels = paste(seq(1000, 2200, 200), seq(800, 2000, 200), sep = "-"),
      include.lowest = TRUE, 
      right = FALSE)
  ) %>%
  dplyr::group_by(
    time, regionA, regionB, distance
  ) %>%
  dplyr::summarise(
    mean_sed = mean(sed, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

hu$time <- forcats::fct_rev(hu$time)

# lapply(
#   base::split(hu, hu$time), function(x) {
#     reshape2::acast(x, regionA~regionB, value.var = "distance")
#   }
# )

regions_factorA <- as.factor(hu$regionA)
hu$regionA <- factor(regions_factorA, levels = c(
  "Austria and Czechia",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

regions_factorB <- as.factor(hu$regionB)
hu$regionB <- factor(regions_factorB, levels = c(
  "Austria and Czechia",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

library(ggplot2)
plu <- ggplot(hu) +
  geom_boxplot(
    aes(x = distance, y = mean_sed),
    position = position_nudge(x = 0.2),
    width = 0.3 
  ) +
  geom_point(
    aes(x = distance, y = mean_sed, color = regionA),
    size = 4,
    position = position_nudge(x = -0.25)
  ) +
  geom_point(
    aes(x = distance, y = mean_sed, color = regionB),
    size = 4,
    position = position_nudge(x = -0.1)
  ) +
  facet_wrap(~time) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    strip.text.x = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20)
  ) +
  scale_color_manual(
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00", "#CC79A7")
  ) +
  xlab("Distance Classes") +
  ylab("Squared Euclidian Distance")

plu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/squared_euclidian_distance_vs_spatial_distance.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    #width = 210, height = 297, units = "mm",
    width = 350, height = 360, units = "mm",
    limitsize = F
  )
  