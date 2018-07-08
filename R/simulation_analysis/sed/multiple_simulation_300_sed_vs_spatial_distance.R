load("../neomod_datapool/simulation_data/squared_euclidian_distance_over_time_sim_multiple.RData")
load("../neomod_datapool/R_data/distance_matrix_spatial_long.RData")
load("../neomod_datapool/R_data/squared_euclidian_distance_over_timeblocks_burial_type.RData")
sed_burial_type <- sed_spatial_distance
load("../neomod_datapool/R_data/squared_euclidian_distance_over_timeblocks_burial_construction.RData")
sed_burial_construction <- sed_spatial_distance

sed_burial_type %<>%
  dplyr::mutate(
    context = "burial_type"
  )

sed_burial_construction %<>%
  dplyr::mutate(
    context = "burial_construction"
  )

sed_spatial_real_world <- rbind(sed_burial_type, sed_burial_construction)

# test <- regions_grid %>%
#   dplyr::mutate(
#     regionA = as.character(regionA),
#     regionB = as.character(regionB)
#   ) 

test <- regions_grid

test <- pbapply::pblapply(
  base::split(test, f = test$model_group), function(z) { 
    lapply(
      base::split(z, f = test$model_id), function(y) { 
        lapply(
          base::split(y, f = y$time), function(x) {
            mn <- pmin(x$regionA, x$regionB)
            mx <- pmax(x$regionA, x$regionB)
            int <- as.numeric(interaction(mn, mx))
            x <- x[match(unique(int), int),]
            return(x)
          }) %>%
        do.call(rbind, .)
    }) %>%
    do.call(rbind, .)    
}) %>%
  do.call(rbind, .)


hu <- test %>% dplyr::left_join(
    distance_matrix_spatial_long, by = c("regionA", "regionB")
  ) %>% 
  dplyr::filter(
    distance != 0
  ) %>%
  dplyr::mutate(
    relation = paste(regionA, "+", regionB),
    time = base::cut(
      time, 
      seq(-2200, -800, 200), labels = paste(seq(-2200, -1000, 200), seq(-2000, -800, 200), sep = " - "),
      include.lowest = TRUE, 
      right = FALSE)
  ) %>%
  dplyr::group_by(
    model_id, model_group, time, regionA, regionB, distance
  ) %>%
  dplyr::summarise(
    mean_sed = mean(sed, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    !is.na(mean_sed)
  )

library(ggplot2)
plu <- ggplot() +
  geom_boxplot(
    data = hu,
    mapping = aes(
      x = as.factor(distance), 
      y = mean_sed
    ),
    width = 0.3
  ) +
  geom_point(
    data = sed_burial_type,
    mapping  = aes(
      x = as.factor(distance), 
      y = mean_sed,
      colour = context
    ),
    size = 4,
    position = position_nudge(x = -0.3)
  ) +
  geom_point(
    data = sed_burial_construction,
    mapping  = aes(
      x = as.factor(distance), 
      y = mean_sed,
      colour = context
    ),
    size = 4,
    position = position_nudge(x = -0.5)
  ) +
  facet_wrap(
    nrow = 2,
    ~time
  ) +
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
  xlab("Spatial Distance Classes") +
  ylab("Squared Euclidian Distance") +
  NULL

plu %>%
  ggsave(
    "../neomod_datapool/plots/sed_simulation/squared_euclidian_distance_vs_spatial_distance_sim_multiple.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 550, height = 280, units = "mm",
    limitsize = F
  )
  
