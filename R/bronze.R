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
    age_class = cut(
      calage, 
      breaks = seq(2500, 5000, 250), 
      labels = paste0(seq(2500, 4750, 250), "-", seq(2750, 5000, 250))
    )
  ) %>%
  dplyr::filter(
    lat > 20, lon < 37
  ) %>%
  dplyr::filter(
    !(burial_type == "unknown" & burial_construction == "unknown")
  )

hu <- ggplot()+
  geom_polygon(
    data = research_area_df,
    aes_string(
      x = "long", y = "lat",
      group = "group"
    ),
    fill = NA, colour = "black"
  ) +
  geom_point(
    data = bronze2, 
    aes(x = lon, y = lat, color = burial_type, shape = burial_construction),
    size = 2
  ) +
  theme_bw() +
  coord_map(
    "ortho", orientation = c(48, 13, 0)
  ) + 
  facet_wrap(
    ~age_class
  ) +
  scale_shape_manual(
    values = c(
      "flat" = 13,
      "mound" = 20,
      "unknown" = 3
    )
  )



bronze2 %>%  
  ggplot +
    geom_density(aes(x = calage))
