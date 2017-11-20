library(magrittr)
library(ggplot2)

bronze <- c14bazAAR::get_RADONB() %>%
  # remove dates without age
  dplyr::filter(!is.na(c14age) & !is.na(c14std)) %>%
  # remove dates outside of calibration range
  dplyr::filter(!(c14age < 71) & !(c14age > 46401))

#bronze <- bronze[1:1000,]

bronze <- bronze %>%
  dplyr::mutate(
    calage_density_distribution = Bchron::BchronCalibrate(
      ages      = bronze$c14age,
      ageSds    = bronze$c14std,
      calCurves = rep("intcal13", nrow(bronze)),
      eps       = 1e-06
    ) %>% lapply(
      function(x) {
        tibble::tibble(age = x$ageGrid, dens_dist = x$densities) 
      }
    ) 
  )

save(bronze, file = "../neomod_datapool/bronze_age/bronze.RData")

# plot(
#   bronze$calage_density_distribution[[3]]$age, 
#   bronze$calage_density_distribution[[3]]$dens_dist
# )

load("../neomod_datapool/bronze_age/bronze.RData")

bol <- 1950

# time filter
bronze0 <- bronze %>% 
  dplyr::mutate(
    in_time_of_interest = 
      purrr::map(calage_density_distribution, function(x){
        any(x$age <= (2500 + bol) & x$age >= (500 + bol))
      }
    )
  ) %>%
  dplyr::filter(
    in_time_of_interest == TRUE
  ) %>%
  dplyr::select(-in_time_of_interest)

bronze1 <- bronze0 %>%
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
  dplyr::filter(
    lat > 20, lon < 37
  )

bronze2 <- bronze1 %>%
  tidyr::unnest(calage_density_distribution) %>%
  dplyr::mutate(
    age = age - bol
  ) %>%
  dplyr::filter(
    age <= 2500 & age >= 500
  ) %>%
  dplyr::arrange(
    desc(burial_construction)
  )

save(bronze2, file = "../neomod_datapool/bronze_age/bronze2.RData")
