#load("../neomod_datapool/bronze_age/space_and_network/land_outline_sf.RData")
load("../neomod_datapool/bronze_age/bronze2.RData")

land_outline <- sf::st_read(
  "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
) %>% 
  sf::st_transform(4326)

countries <- sf::st_read(
  "../neomod_datapool/geodata/country_areas/ne_50m_admin_0_countries.shp"
) %>% 
  sf::st_transform(4326)

rivers <- sf::st_read(
  "../neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_rivers_lake_centerlines_scale_rank.shp"
) %>% 
  sf::st_transform(4326)

lakes <- sf::st_read(
  "../neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_lakes.shp"
) %>% 
  sf::st_transform(4326)

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
library(sf)

xlimit <- c(-1600000, 1200000)
ylimit <- c(800000, 3600000)

hu <- ggplot() +
  geom_sf(
    data = countries,
    fill = NA, colour = "black", size = 0.2
  ) +
  # geom_sf(
  #   data = rivers,
  #   fill = NA, colour = "black", size = 0.2
  # ) +
  # geom_sf(
  #   data = lakes,
  #   fill = NA, colour = "black", size = 0.2
  # ) +
  # geom_sf(
  #   data = land_outline,
  #   fill = NA, colour = "black", size = 0.2
  # ) +
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
  # facet_wrap(
  #   nrow = 3,
  #   ~age_slice
  # ) +
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
    #"/home/clemens/neomod/neomod_datapool/bronze_age/disp_map.jpeg",
    "/home/clemens/neomod/neomod_datapool/bronze_age/disp_map_full.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )

#### movie ####

for(i in seq(2500, 500, -10)) {
  bronze2_fine_slices <- bronze2 %>%
    dplyr::filter(
      age == i
    ) %>% st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    )

  hu <- ggplot() +
    geom_sf(
      data = land_outline,
      fill = NA, colour = "black", size = 0.4
    ) +
    geom_sf(
      data = rivers,
      fill = NA, colour = "black", size = 0.2
    ) +
    geom_sf(
      data = lakes,
      fill = NA, colour = "black", size = 0.2
    ) +
    #geom_point(
    geom_sf(
      data = bronze2_fine_slices, 
      aes(
        #x = lon, y = lat,
        color = burial_type, 
        shape = burial_construction,
        size = burial_construction,
        alpha = norm_dens
      )
    ) +
    theme_bw() +
    coord_sf(
      xlim = xlimit, ylim = ylimit,
      crs = st_crs(102013)
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
        "flat" = 10,
        "mound" = 10,
        "unknown" = 7.5
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
      plot.title = element_text(size = 30, face = "bold"),
      legend.position = "right",
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 15)
    ) +
    guides(
      title = "Grabtyp", color = guide_legend(nrow = 3, byrow = TRUE),
      shape = guide_legend(nrow = 3, byrow = TRUE),
      alpha = FALSE
    ) +
    ggtitle(paste0(i, "calBC"))
  
  hu %>%
    ggsave(
      paste0("/home/clemens/neomod/neomod_datapool/bronze_age/movie/", 2500 - i ,".jpeg"),
      plot = .,
      device = "jpeg",
      scale = 1,
      dpi = 300,
      width = 400, height = 300, units = "mm",
      limitsize = F
    )

}

# ffmpeg -r 30 -start_number 0 -i movie/%d.jpeg -vcodec mpeg4 -r 30 the_movie.avi
