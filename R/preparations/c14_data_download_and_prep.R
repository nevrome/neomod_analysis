library(magrittr)
library(ggplot2)

bronze <- c14bazAAR::get_RADONB()




save(bronze, file = "../neomod_datapool/bronze_age/bronze.RData")
load("../neomod_datapool/bronze_age/bronze.RData")

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
  dplyr::filter(
    calage >= 500 & calage <= 2500
  ) %>%
  dplyr::mutate(
    age_class = cut(
      calage, 
      breaks = seq(500, 2500, 250), 
      labels = paste0(seq(750, 2500, 250), "-", seq(500, 2250, 250), "calBC")
    ) %>% as.factor %>% factor(levels = rev(levels(.)))
  ) %>%
  dplyr::filter(
    lat > 20, lon < 37
  ) %>%
  # dplyr::filter(
  #   !(burial_type == "unknown" & burial_construction == "unknown")
  # ) %>%
  dplyr::arrange(
    desc(burial_construction)
  )

save(bronze2, file = "../neomod_datapool/bronze_age/bronze2.RData")
