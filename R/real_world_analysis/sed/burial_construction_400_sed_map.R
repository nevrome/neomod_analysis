land_outline <- sf::st_read("../neomod_datapool/geodata/land_shapes/ne_50m_land.shp")
countries <- sf::st_read("../neomod_datapool/geodata/country_areas/ne_50m_admin_0_countries.shp")
research_area <- sf::st_read("data_manually_prepared/research_area.shp")
load("data_analysis/regions.RData")
load("data_analysis/sed_time_spatial_network.RData")
load("data_analysis/region_centers.RData")

ex <- raster::extent(regions %>% sf::st_transform(sf::st_crs(102013)))
xlimit <- c(ex[1] + 100000, ex[2] - 100000)
ylimit <- c(ex[3], ex[4])

distance_lines %<>% dplyr::filter(
  context == "burial_construction"
)

library(ggplot2)
library(sf)

hu <- ggplot() +
  geom_sf(
    data = land_outline,
    fill = "white", colour = "black", size = 0.4
  ) +
  geom_sf(
    data = research_area,
    fill = NA, colour = "red", size = 0.5
  ) +
  geom_curve(
    data = distance_lines,
    mapping = aes(
      x = x_a, y = y_a, xend = x_b, yend = y_b,
      size = mean_sed    
    ), 
    alpha = 0.5,
    curvature = 0.2,
    colour = "black"
  ) +
  facet_wrap(
    nrow = 2,
    ~time
  ) +
  geom_sf(
    data = region_centers,
    mapping = aes(
      colour = NAME
    ),
    fill = NA, size = 12
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit,
    crs = st_crs(102013)
  ) + 
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.text = element_text(size = 20),
    panel.grid.major = element_line(colour = "black", size = 0.3),
    strip.text.x = element_text(size = 20)
  ) +
  guides(
    size = guide_legend(title = "Behavioural closeness"),
    alpha = FALSE,
    colour = FALSE
  ) +
  scale_size(
    trans = 'reverse',
    range = c(0.5, 3)
  ) +
  scale_color_manual(
    values = c(
      "cremation" = "#D55E00",
      "inhumation" = "#0072B2",
      "mound" = "#CC79A7",
      "flat" = "#009E73",
      "unknown" = "darkgrey",
      "Austria and Czechia" = "#999999", 
      "Poland" = "#ffe500", 
      "Southern Germany" = "#56B4E9", 
      "Northeast France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Skandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7"
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
  )

hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/plots/sed/sed_map_research_area_timeslices_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    #width = 210, height = 297, units = "mm",
    width = 550, height = 280, units = "mm",
    limitsize = F
  )
