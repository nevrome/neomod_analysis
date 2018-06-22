load("../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_burial_type.RData")
regions_grid_burial_type <- regions_grid

load("../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_burial_construction.RData")
regions_grid_burial_construction <- regions_grid

regions_grid_burial_type <- regions_grid_burial_type %>%
  dplyr::mutate(
    regionA = as.character(regionA),
    regionB = as.character(regionB)
  ) 

regions_grid_burial_type <- lapply(
  split(regions_grid_burial_type, f = regions_grid_burial_type$time),
  function(x) {
    mn <- pmin(x$regionA, x$regionB)
    mx <- pmax(x$regionA, x$regionB)
    int <- as.numeric(interaction(mn, mx))
    x <- x[match(unique(int), int),]
    return(x)
  }
) %>%
  do.call(rbind, .)

regions_grid_burial_construction <- regions_grid_burial_construction %>%
  dplyr::mutate(
    regionA = as.character(regionA),
    regionB = as.character(regionB)
  ) 

regions_grid_burial_construction <- lapply(
  split(regions_grid_burial_construction, f = regions_grid_burial_construction$time),
  function(x) {
    mn <- pmin(x$regionA, x$regionB)
    mx <- pmax(x$regionA, x$regionB)
    int <- as.numeric(interaction(mn, mx))
    x <- x[match(unique(int), int),]
    return(x)
  }
) %>%
  do.call(rbind, .)

hu <- regions_grid_burial_type %>% dplyr::left_join(
  regions_grid_burial_construction, by = c("regionA", "regionB", "time")
) %>% 
  dplyr::rename(
    sed_burial_type = sed.x,
    sed_burial_construction = sed.y
  ) %>%
  dplyr::filter(
    (sed_burial_type != 0 & sed_burial_construction != 0) & !is.na(sed_burial_construction)
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
    time, regionA, regionB
  ) %>%
  dplyr::summarise(
    mean_sed_burial_type = mean(sed_burial_type, na.rm = TRUE),
    mean_sed_burial_construction = mean(sed_burial_construction, na.rm = TRUE)
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
  geom_point(
    aes(x = mean_sed_burial_type, y = mean_sed_burial_construction, color = regionA),
    size = 4,
    position = position_nudge(x = -0.03)
  ) +
  geom_point(
    aes(x = mean_sed_burial_type, y = mean_sed_burial_construction, color = regionB),
    size = 4,
    position = position_nudge(x = 0.03)
  ) +
  geom_smooth(
    method = 'lm', 
    mapping = aes(mean_sed_burial_type, mean_sed_burial_construction),
    color = "black",
    se = FALSE,
    size = 2
  ) +
  ggpubr::stat_cor(
    mapping = aes(mean_sed_burial_type, mean_sed_burial_construction),
    method = "pearson", 
    label.x = 1.4, 
    label.y = 1.8,
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
      "England" = "#CC79A7"
    )
  ) +
  xlab("Squared Euclidian Distance Burial Type") +
  ylab("Squared Euclidian Distance Burial Construction")

plu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/squared_euclidian_distance_burial_type_vs_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    #width = 210, height = 297, units = "mm",
    width = 350, height = 360, units = "mm",
    limitsize = F
  )
