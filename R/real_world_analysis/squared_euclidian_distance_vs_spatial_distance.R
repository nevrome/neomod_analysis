#load("../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_burial_type.RData")
load("../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_burial_construction.RData")
load("../neomod_datapool/bronze_age/distance_matrix_spatial_long_half.RData")
#load("../neomod_datapool/bronze_age/mantel_sed_spatial_burial_type.RData")
load("../neomod_datapool/bronze_age/mantel_sed_spatial_burial_construction.RData")

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
    distance_matrix_spatial_long_half, by = c("regionA", "regionB")
  ) %>% 
  dplyr::filter(
    distance != 0
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
  dplyr::ungroup() %>%
  dplyr::filter(
    !is.na(mean_sed)
  )

hu$time <- forcats::fct_rev(hu$time)

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
    aes(x = distance, y = mean_sed, group = distance),
    width = 0.3
  ) +
  geom_point(
    aes(x = distance, y = mean_sed, color = regionA),
    size = 4,
    position = position_nudge(x = -0.4)
  ) +
  geom_point(
    aes(x = distance, y = mean_sed, color = regionB),
    size = 4,
    position = position_nudge(x = -0.31)
  ) +
  geom_text(
    data = mantel_test_results,
    aes(
      label = paste0("Mantel Test r: ", round(statistic, 3), ", p: ", signif),
      colour = ifelse(signif < 0.05, "h0canberejected", "h0cannotberejected")
    ),
    x = 2.7, y = 2.2,
    size = 6
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
    values = c(
      "Austria and Czechia" = "#999999", 
      "Poland" = "#E69F00", 
      "Southern Germany" = "#56B4E9", 
      "Northeast France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Skandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7",
      "h0canberejected" = "red",
      "h0cannotberejected" = "black"
    ),
    breaks = c(
      "Austria and Czechia",
      "Poland", 
      "Southern Germany", 
      "Northeast France", 
      "Northern Germany", 
      "Southern Skandinavia", 
      "Benelux", 
      "England"
    ),
    labels = c(
      "Austria and Czechia",
      "Poland", 
      "Southern Germany", 
      "Northeast France", 
      "Northern Germany", 
      "Southern Skandinavia", 
      "Benelux", 
      "England"
    )
  ) +
  xlab("Spatial Distance Classes") +
  ylab("Squared Euclidian Distance") +
  ylim(0, 2.3)

plu %>%
  ggsave(
    #"/home/clemens/neomod/neomod_datapool/bronze_age/squared_euclidian_distance_vs_spatial_distance_burial_type.jpeg",
    "/home/clemens/neomod/neomod_datapool/bronze_age/squared_euclidian_distance_vs_spatial_distance_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 350, height = 360, units = "mm",
    limitsize = F
  )
  
