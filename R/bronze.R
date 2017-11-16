library(magrittr)
library(ggplot2)
library(ggmap)

load("/home/clemens/neomod/neomod_datapool/model_data/research_area_df.RData")

bronze <- c14bazAAR::get_RADONB() %>%
  c14bazAAR::calibrate()

save(bronze, file = "../neomod_datapool/bronze_age/bronze.RData")

bronze2 <- bronze %>%
  dplyr::select(
    -sourcedb, -c14age, -c14std, - c13val, -country, -shortref
  ) %>%
  dplyr::filter(
    sitetype %in% c( 
      "Grave", "Grave (mound)", "Grave (flat) inhumation",
      "Grave (cremation)", "Grave (inhumation)", "Grave (mound) cremation",
      "Grave (mound) inhumation", "Grave (flat) cremation", "Grave (flat)",
      "cemetery"
    )   
  ) %>%
  dplyr::mutate(
    burial_type = ifelse(
      grepl("cremation", sitetype), "cremation", 
      ifelse(
        grepl("inhumation", sitetype), "inhumation",
        "unknown"
      )
    ),
    burial_construction = ifelse(
      grepl("mound", sitetype), "mound", 
      ifelse(
        grepl("flat", sitetype), "flat",
        "unknown"
      )
    )
  ) %>% 
  dplyr::mutate(
    calage = calage - 1950
  ) %>%
  dplyr::mutate(
    age_class = cut(
      calage, 
      breaks = seq(500, 3000, 250), 
      labels = paste0(seq(750, 3000, 250), "-", seq(500, 2750, 250), "calBC")
    ) %>% as.factor %>% factor(levels = rev(levels(.)))
  ) %>%
  dplyr::filter(
    lat > 20, lon < 37
  ) %>%
  dplyr::filter(
    !(burial_type == "unknown" & burial_construction == "unknown")
  ) %>%
  dplyr::arrange(
    desc(burial_construction)
  )

hu <- ggplot()+
  geom_polygon(
    data = research_area_df,
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
    size = 1
  ) +
  theme_bw() +
  coord_map(
    "ortho", orientation = c(48, 13, 0)
  ) + 
  facet_wrap(
    nrow = 1,
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
      "unknown" = "grey"
    )
  ) 

hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/disp_map.jpeg",
    plot = .,
    device = "jpeg",
    scale = 5,
    dpi = 600,
    width = 33, height = 3, units = "cm"
  )


bronze2 %>%  
  ggplot +
    geom_density(aes(x = calage))
