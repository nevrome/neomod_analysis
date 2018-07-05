load("../neomod_datapool/bronze_age/development_proportions_burial_type.RData")
load("../neomod_datapool/bronze_age/development_proportions_burial_construction.RData")

prop <- proportion_development_burial_type
#prop <- proportion_development_burial_construction

# proportion_development_burial_type %>%
#   tidyr::complete(
#     region_name, timestep, idea,
#     fill = list(proportion = as.integer(0))
#   )

sed <- function(pi, pj) {
  pi <- pi / sum(pi)
  pj <- pj / sum(pj)
  sum((pi - pj)^2)
}

long_prop <- prop %>%
  tidyr::spread(
    idea, proportion
  )

regions <- prop$region_name %>% unique()
timesteps <- prop$timestep %>% unique()

regions_grid <- 
  expand.grid(
    regionA = regions, regionB = regions, time = timesteps, 
    stringsAsFactors = FALSE
  ) %>%
  tibble::as.tibble() %>%
  dplyr::left_join(
    long_prop,
    by = c("regionA" = "region_name", "time" = "timestep")
  ) %>%
  dplyr::left_join(
    long_prop,
    by = c("regionB" = "region_name", "time" = "timestep"),
    suffix = c("_regionA", "_regionB")
  )

regions_grid <- regions_grid %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    sed = sed(c(cremation_regionA, inhumation_regionA), c(cremation_regionB, inhumation_regionB))
    #sed = sed(c(flat_regionA, mound_regionA), c(flat_regionB, mound_regionB))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    regionA, regionB, time, sed
  )

save(regions_grid, file = "../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_burial_type.RData")
#save(regions_grid, file = "../neomod_datapool/bronze_age/squared_euclidian_distance_over_time_burial_construction.RData")

library(ggplot2)
hu <- regions_grid %>%
  ggplot() +
  geom_line(
    aes(time, sed),
    alpha = 0.3,
    size = 0.5
  ) +
  geom_smooth(
    aes(time, sed, color = regionB),
    method = "loess",
    span = 0.3,
    size = 1.5
  ) +
  facet_grid(
    regionA ~ regionB,
    switch = "y"
  ) +
  scale_x_continuous(
    breaks = c(-2200, -2000, -1500, -1000, -800), 
    limits = c(-2500, -800)
  ) +
  theme_bw() +
  scale_color_manual(
    guide = FALSE,
    values = c(
      "Austria and Czechia" = "#999999", 
      "Poland" = "#ffe500", 
      "Southern Germany" = "#56B4E9", 
      "Northeast France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Skandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7"
    )
  ) +
  theme(
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 9)
  ) +
  ylab("Sqared Euclidian Distance") +
  xlab("Time")
  
hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/regions_regions_squared_euclidian_distance_burial_type.jpeg",
    #"/home/clemens/neomod/neomod_datapool/bronze_age/regions_regions_squared_euclidian_distance_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 300, height = 300, units = "mm",
    limitsize = F
  )

#### mean sed ####

regions_grid_mean <- regions_grid %>%
  dplyr::group_by(
    regionA, regionB
  ) %>%
  dplyr::summarise(
    mean_sed = mean(sed, na.rm = T)
  )

regions_grid_mean$regionB <- forcats::fct_rev(regions_grid_mean$regionB)

save(regions_grid_mean, file = "../neomod_datapool/bronze_age/regions_mean_sed")

kur <- regions_grid_mean %>%
  ggplot() +
  geom_raster(
    aes(
      x = regionA,
      y = regionB,
      fill = mean_sed
    )
  ) +
  geom_text(
    aes(
      x = regionA,
      y = regionB,
      label = round(mean_sed, 2)
    )
  ) +
  scale_x_discrete(position = "top") +
  scale_fill_continuous(
    guide = FALSE,
    high = "#6d6d6d", low = "#FFFFFF"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 0),
    axis.text.y = element_text(size = 10, angle = 45, vjust = 0, hjust = 1),
    axis.title = element_blank()
  )

kur %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/regions_regions_mean_squared_euclidian_distance_burial_type.jpeg",
    #"/home/clemens/neomod/neomod_datapool/bronze_age/development_proportions_regions_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 200, height = 200, units = "mm",
    limitsize = F
  )

#### second sed plot ####

schnu <- regions_grid %>%
  ggplot() +
  geom_smooth(
    aes(time, sed, color = regionB),
    method = "loess",
    span = 0.3
  ) +
  facet_wrap(
    ~regionA, 
    nrow = 8
  ) +
  scale_x_continuous(
    breaks = c(-2200, -2000, -1500, -1000, -800), 
    limits = c(-2500, -800)
  ) +
  theme_bw() +
  scale_color_manual(
    guide = FALSE,
    values = c(
      "Austria and Czechia" = "#999999", 
      "Poland" = "#ffe500", 
      "Southern Germany" = "#56B4E9", 
      "Northeast France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Skandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7"
    )
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) +
  ylab("Sqared Euclidian Distance") +
  xlab("Time")


region_file_list <- unique(regions_grid$regionA) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("../neomod_datapool/bronze_age/region_pictograms_colour/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.14, height = 1.2
  )
})
dummy <- tibble::tibble(regionA = unique(regions_grid$regionA), grob = gl )

source("R/helper_functions/geom_grob.R")

schnu <- schnu +
  geom_custom(
    data = dummy, 
    aes(grob = grob), 
    inherit.aes = FALSE,
    x = 0.1, y = 0.5
  )

schnu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/regions_squared_euclidian_distance_burial_type.jpeg",
    #"/home/clemens/neomod/neomod_datapool/bronze_age/development_proportions_regions_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )
