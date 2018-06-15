load("../neomod_datapool/bronze_age/development_proportions_burial_type.RData")
load("../neomod_datapool/bronze_age/development_proportions_burial_construction.RData")

prop <- proportion_development_burial_type
# prop <- proportion_development_burial_construction

# proportion_development_burial_type %>%
#   tidyr::complete(
#     region_name, timestep, idea,
#     fill = list(proportion = as.integer(0))
#   )

by_regions <-  base::split(prop, prop$region_name)

by_regions_and_time <- lapply(
  by_regions,
  function(x) {
    base::split(x, x$timestep)
  }
)

regions <- prop$region_name %>% unique()
timesteps <- prop$timestep %>% unique()
regions_grid <- expand.grid(regionA = regions, regionB = regions, time = timesteps, stringsAsFactors = FALSE)


sed <- function(pi, pj) {
  pi <- pi / sum(pi)
  pj <- pj / sum(pj)
  sum((pi - pj)^2)
}

for (p1 in 1:nrow(regions_grid)) {
  p_region_A <- by_regions_and_time[[regions_grid[p1, 1]]][[as.character(regions_grid[p1, 3])]]$proportion
  p_region_B <- by_regions_and_time[[regions_grid[p1, 2]]][[as.character(regions_grid[p1, 3])]]$proportion
  regions_grid$sed[p1] <- sed(p_region_A, p_region_B)
}


regions_factorA <- as.factor(regions_grid$regionA)
regions_grid$regionA <- factor(regions_factorA, levels = c(
  "Austria and Czechia",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

regions_factorB <- as.factor(regions_grid$regionB)
regions_grid$regionB <- factor(regions_factorB, levels = c(
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
  scale_x_reverse(
    breaks = c(2000, 1500, 1000), 
    limits = c(2200, 800)
  ) +
  theme_bw() +
  scale_color_manual(
    guide = FALSE,
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00", "#CC79A7")
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
  scale_x_reverse(
    breaks = c(2200, 2000, 1500, 1000, 800), 
    limits = c(2500, 800)
  ) +
  theme_bw() +
  scale_color_manual(
    guide = FALSE,
    values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00", "#CC79A7")
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
    width = 0.1, height = 0.9
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
