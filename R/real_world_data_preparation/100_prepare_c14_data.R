library(magrittr)

#### set constants ####

# birth of libby
bol <- 1950
# 2sigma range probability threshold
threshold <- (1 - 0.9545) / 2



#### data download ####

# get all radon dates with c14bazAAR  
bronze <- c14bazAAR::get_RADONB() %>%
  # remove dates without age
  dplyr::filter(!is.na(c14age) & !is.na(c14std)) %>%
  # remove dates outside of theoretical calibration range
  dplyr::filter(!(c14age < 71) & !(c14age > 46401))



#### calibration #### 

bronze <- bronze %>%
  dplyr::mutate(
    # add list column with the age density distribution for every date
    calage_density_distribution = Bchron::BchronCalibrate(
      ages      = bronze$c14age,
      ageSds    = bronze$c14std,
      calCurves = rep("intcal13", nrow(bronze)),
      eps       = 1e-06
    ) %>% 
      # transform BchronCalibrate result to a informative tibble
      # this tibble includes the years, the density per year,
      # the normalized density per year and the information,
      # if this year is in the two_sigma range for the current date
      pbapply::pblapply(
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



#### transform calBP age to calBC ####

bronze$calage_density_distribution %<>% lapply(
  function(x) {
    x$age = -x$age + bol
    return(x)
  }
)

save(bronze, file = "../neomod_datapool/R_data/bronze.RData")

# plot to check the calibration result 
# library(ggplot2)
# bronze$calage_density_distribution[[3]] %>%
#   ggplot() +
#   geom_point(aes(age, norm_dens, color = two_sigma))



#### filter time ####

load("../neomod_datapool/R_data/bronze.RData")

# filter dates to only include dates in in time range of interest
bronze0 <- bronze %>% 
  dplyr::mutate(
    in_time_of_interest = 
      purrr::map(calage_density_distribution, function(x){
        any(
          x$age >= -2200 & 
            x$age <= -800 &
            x$two_sigma
        )
      }
    )
  ) %>%
  dplyr::filter(
    in_time_of_interest == TRUE
  ) %>%
  dplyr::select(-in_time_of_interest)

save(bronze0, file = "../neomod_datapool/R_data/bronze0.RData")



#### filter research question ####

load("../neomod_datapool/R_data/bronze0.RData")

bronze1 <- bronze0 %>%
  # reduce variable selection to necessary information
  dplyr::select(
    -sourcedb, -c14age, -c14std, -c13val, -country, -shortref
  ) %>%
  # filter by relevant sitetypes
  dplyr::filter(
    sitetype %in% c( 
      "Grave", "Grave (mound)", "Grave (flat) inhumation",
      "Grave (cremation)", "Grave (inhumation)", "Grave (mound) cremation",
      "Grave (mound) inhumation", "Grave (flat) cremation", "Grave (flat)",
      "cemetery"
    )   
  ) %>%
  # transform sitetype field to tidy data about burial_type and burial_construction
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
  # remove dates without coordinates
  dplyr::filter(
   !is.na(lat) | !is.na(lon)
  )

save(bronze1, file = "../neomod_datapool/R_data/bronze1.RData")



# export dates as shapefile
bronze1 %>% sf::st_as_sf(
  coords = c("lon", "lat"),
  crs = 4326
) %>% 
  sf::write_sf("../neomod_datapool/R_data/bronze1.shp")



#### unnest dates ####

load("../neomod_datapool/R_data/bronze1.RData")

# unnest calage_density_distribution to have per year information: 
# a diachron perspective
bronze2 <- bronze1 %>%
  tidyr::unnest(calage_density_distribution) %>%
  dplyr::filter(
    two_sigma == TRUE
  ) %>%
  dplyr::filter(
    age >= -2200 & age <= -800
  ) %>%
  dplyr::arrange(
    desc(burial_construction)
  )

save(bronze2, file = "../neomod_datapool/R_data/bronze2.RData")
