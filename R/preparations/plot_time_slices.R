library(magrittr)

area <- rgdal::readOGR(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
) %>% ggplot2::fortify()

load("../neomod_datapool/bronze_age/bronze2.RData")

bronze2_slices <- bronze2 %>%
  dplyr::filter(
    age %in% seq(2500, 500, by = -250)
  ) %>%
  dplyr::mutate(
    age_slice = factor(age, levels = seq(2500, 500, by = -250))
  )

hu <- ggplot() +
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
    data = bronze2_slices, 
    aes(
      x = lon, y = lat, 
      color = burial_type, 
      shape = burial_construction,
      size = burial_construction
    )
  ) +
  theme_bw() +
  coord_map(
    "ortho", orientation = c(48, 13, 0),
    xlim = c(-10, 30), ylim = c(35, 65)
  ) + 
  facet_wrap(
    nrow = 2,
    ~age_slice
  ) +
  scale_shape_manual(
    values = c(
      "flat" = "\u268A",
      "mound" = "\u25E0",
      "unknown" = "\u2715"
    )
  ) +
  scale_size_manual(
    values = c(
      "flat" = 3,
      "mound" = 3,
      "unknown" = 2
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

#### movie ####

for(i in 2500:500) {
  bronze2_fine_slices <- bronze2 %>%
    dplyr::filter(
      age == i
    )

  hu <- ggplot() +
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
      data = bronze2_fine_slices, 
      aes(
        x = lon, y = lat, 
        color = burial_type, 
        shape = burial_construction,
        size = burial_construction
      )
    ) +
    theme_bw() +
    coord_map(
      "ortho", orientation = c(48, 13, 0),
      xlim = c(-10, 30), ylim = c(35, 65)
    ) +
    scale_shape_manual(
      values = c(
        "flat" = "\u268A",
        "mound" = "\u25E0",
        "unknown" = "\u2715"
      )
    ) +
    scale_size_manual(
      values = c(
        "flat" = 3,
        "mound" = 3,
        "unknown" = 2
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
      panel.grid.major = element_line(colour = "lightgrey", size = 0.1)#,
      # legend.position = c(1, 0), legend.justification = c(1, 0)
      #legend.position = "bottom"
    )
  
  hu %>%
    ggsave(
      paste0("/home/clemens/neomod/neomod_datapool/bronze_age/movie/", 2500 - i ,".jpeg"),
      plot = .,
      device = "jpeg",
      scale = 1,
      dpi = 300,
      width = 200, height = 150, units = "mm",
      limitsize = F
    )

}

# ffmpeg -r 20 -start_number 0 -i movie/%d.jpeg -r 7 -vcodec mpeg4 -r 20 the_movie.avi
