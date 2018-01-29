load("../neomod_datapool/bronze_age/space_and_network/land_outline_sf.RData")
load("../neomod_datapool/bronze_age/bronze2.RData")

regions <- sf::st_read(
  "manually_changed_data/regionen2017g.shp"
) %>% 
  sf::st_transform(4326)

bronze2_slices <- bronze2 %>%
  dplyr::filter(
    age %in% seq(2500, 500, by = -250)
  ) %>%
  dplyr::mutate(
    age_slice = factor(age, levels = seq(2500, 500, by = -250))
  )

library(ggplot2)

limit <- sf::st_bbox(land_outline)
xlimit <- limit[c(1, 3)]  
ylimit <- limit[c(2, 4)]  

hu <- ggplot() +
  geom_sf(
    data = land_outline,
    fill = NA, colour = "black", size = 0.1
  ) +
  geom_sf(
    data = regions,
    fill = NA, colour = "red", size = 0.3
  ) +
  geom_point(
    data = bronze2_slices, 
    aes(
      x = lon, y = lat, 
      color = burial_type, 
      shape = burial_construction,
      size = burial_construction
      #alpha = norm_dens
    )
  ) +
  theme_bw() +
  coord_sf(
    xlim = xlimit, ylim = ylimit
  ) + 
  facet_wrap(
    nrow = 3,
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
      "flat" = 4,
      "mound" = 4,
      "unknown" = 3
    )
  ) +
  scale_color_manual(
    values = c(
      "cremation" = "#D55E00",
      "inhumation" = "#0072B2",
      "mound" = "#CC79A7",
      "flat" = "#009E73",
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
    # panel.grid.major = element_line(colour = "lightgrey", size = 0.1),
    # legend.position = c(1, 0), legend.justification = c(1, 0)
    legend.position = "bottom"
  ) +
  guides(
    title = "Grabtyp", color = guide_legend(nrow = 3, byrow = TRUE),
    shape = guide_legend(nrow = 3, byrow = TRUE)
    #alpha = guide_legend(nrow = 3, byrow = TRUE)
  )

hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/disp_map.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
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
        size = burial_construction,
        alpha = norm_dens
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
    scale_alpha(guide = FALSE) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = c(1, 0), legend.justification = c(1, 0)
      #legend.position = "bottom"
    ) +
    ggtitle(paste0(i, "calBC"))
  
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

# ffmpeg -r 30 -start_number 0 -i movie/%d.jpeg -vcodec mpeg4 -r 30 the_movie.avi
