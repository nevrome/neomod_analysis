library(magrittr)

area <- rgdal::readOGR(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
) %>% ggplot2::fortify()

load("../neomod_datapool/bronze_age/bronze2.RData")

hu <- ggplot()+
  geom_polygon(
    data = area,
    aes_string(
      x = "long", y = "lat",
      group = "group"
    ),
    fill = NA, colour = "black",
    size = 0.1
  ) +
  geom_point(
    data = bronze2, 
    aes(x = lon, y = lat, color = burial_type, shape = burial_construction),
    size = 2.5
  ) +
  theme_bw() +
  coord_map(
    "ortho", orientation = c(48, 13, 0),
    xlim = c(-10, 30), ylim = c(35, 65)
  ) + 
  facet_wrap(
    nrow = 2,
    ~age_class
  ) +
  scale_shape_manual(
    values = c(
      "flat" = "\u268A",
      "mound" = "\u25E0",
      "unknown" = "\u2715"
    )
  ) +
  scale_color_manual(
    values = c(
      "cremation" = "red",
      "inhumation" = "darkgreen",
      "unknown" = "darkgrey"
    )
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(colour = "lightgrey", size = 0.1),
    # legend.position = c(1, 0), legend.justification = c(1, 0)
    legend.position="bottom"
  )


# hu %>%
#   ggsave(
#     "/home/clemens/neomod/neomod_datapool/bronze_age/disp_map.jpeg",
#     plot = .,
#     device = "jpeg",
#     scale = 5,
#     dpi = 600,
#     width = 22, height = 3, units = "cm"
#   )

hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/disp_map.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 297, height = 210, units = "mm",
    limitsize = F
  )


# bronze2 %>%  
#   ggplot +
#     geom_density(aes(x = calage))

