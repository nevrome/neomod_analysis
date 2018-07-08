load("../neomod_datapool/simulation_data/squared_euclidian_distance_over_time_sim_multiple.RData")
load("../neomod_datapool/R_data/distance_matrix_spatial_long.RData")

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
plu <- ggplot(hu) +
  geom_boxplot(
    aes(
      x = as.factor(distance), 
      y = mean_sed, 
      fill = as.factor(model_group)
    ),
    width = 0.3
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
  scale_fill_manual(
    values = c(
      "1" = "#999999", 
      "2" = "#ffe500", 
      "3" = "#56B4E9", 
      "4" = "#009E73"
    )
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
  
