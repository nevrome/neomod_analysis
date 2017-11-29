library(magrittr)

# birth of libby
bol <- 1950
# 2sigma range probability threshold
threshold <- (1 - 0.9545) / 2

# data download
bronze <- c14bazAAR::get_RADONB() %>%
  # remove dates without age
  dplyr::filter(!is.na(c14age) & !is.na(c14std)) %>%
  # remove dates outside of calibration range
  dplyr::filter(!(c14age < 71) & !(c14age > 46401))

#bronze <- bronze[1:100,]

# calibration
bronze <- bronze %>%
  dplyr::mutate(
    calage_density_distribution = Bchron::BchronCalibrate(
      ages      = bronze$c14age,
      ageSds    = bronze$c14std,
      calCurves = rep("intcal13", nrow(bronze)),
      eps       = 1e-06
    ) %>% pbapply::pblapply(
      function(x) {
        x$densities %>% cumsum -> a      # cumulated density
        bottom <- x$ageGrid[which(a <= threshold) %>% max]
        top <- x$ageGrid[which(a > 1-threshold) %>% min]
        tibble::tibble(
          age = x$ageGrid, 
          dens_dist = x$densities,
          norm_dens = x$densities/max(x$densities),
          two_sigma = x$ageGrid >= bottom & x$ageGrid <= top 
        ) 
      }
    ) 
  )

save(bronze, file = "../neomod_datapool/bronze_age/bronze.RData")

# library(ggplot2)
# bronze$calage_density_distribution[[3]] %>%
#   ggplot() +
#   geom_point(aes(age, norm_dens, color = two_sigma))

load("../neomod_datapool/bronze_age/bronze.RData")

# time filter
bronze0 <- bronze %>% 
  dplyr::mutate(
    in_time_of_interest = 
      purrr::map(calage_density_distribution, function(x){
        any(
          x$age <= (2500 + bol) & 
            x$age >= (500 + bol) &
            x$two_sigma
        )
      }
    )
  ) %>%
  dplyr::filter(
    in_time_of_interest == TRUE
  ) %>%
  dplyr::select(-in_time_of_interest)

# question filter and prep
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
   !is.na(lat) | !is.na(lon)
  )

save(bronze1, file = "../neomod_datapool/bronze_age/bronze1.RData")
load("../neomod_datapool/bronze_age/bronze1.RData")

# open up to diachron perspective
bronze2 <- bronze1 %>%
  tidyr::unnest(calage_density_distribution) %>%
  dplyr::filter(
    two_sigma == TRUE
  ) %>%
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
