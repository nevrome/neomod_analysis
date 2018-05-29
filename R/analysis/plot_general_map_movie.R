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
    geom_sf(
      data = bronze2_fine_slices, 
      aes(
        color = burial_type, 
        shape = burial_construction,
        size = burial_construction,
        alpha = norm_dens
      ),
      show.legend = "point"
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
      color = guide_legend(override.aes = list(size = 10)),
      alpha = FALSE
    ) +
    ggtitle(paste0(i, "calBC"))
  
  hu %>%
    ggsave(
      paste0("/home/clemens/neomod/neomod_datapool/bronze_age/movie/", 250 - (i/10) ,".jpeg"),
      plot = .,
      device = "jpeg",
      scale = 1,
      dpi = 300,
      width = 400, height = 300, units = "mm",
      limitsize = F
    )
  
}

#ffmpeg -r 8 -vb 20M -i movie/%*.jpeg -vcodec libx264 -vf scale=1280:-2 -r 8 the_movie_3.mp4
