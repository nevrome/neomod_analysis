# load libraries
library(sf)
library(magrittr)
library(neolithicRtools)
library(readr)
library(purrr)

wgs <- 4326

neo_gron <- sf::st_read(
  dsn = "~/neomod/neomod_datapool/neolithic_expansion/gronenborn_map/neolithic.shp"
  ) %>% sf::st_transform(wgs) 

starts <- read_csv("~/neomod/neomod_datapool/neolithic_expansion/gronenborn_map/gronenborn.txt")

neo_gron <- neo_gron %>% dplyr::left_join(
  starts, "id"
  )

# load data
CALPAL_raw <- neolithicRtools::get_CALPAL()

  # treat two null as NA in the coordinate columns
CALPAL <- CALPAL_raw %>% transform(
  LONGITUDE = ifelse(LONGITUDE == 0 & LATITUDE == 0, NA, LONGITUDE), 
  LATITUDE = ifelse(LONGITUDE == 0 & LATITUDE == 0, NA, LATITUDE)
  # remove NA coordinates
  ) %>% dplyr::filter(
  !is.na(LONGITUDE) &
    !is.na(LATITUDE)
  # create sf object
  ) %>% st_as_sf(
  coords = c("LONGITUDE", "LATITUDE"),
  crs = wgs
  )

hu <- st_within(CALPAL, neo_gron) %>% 
  as.character() %>%
  as.numeric() 

CALPAL <- cbind(CALPAL, hu) %>%
  dplyr::filter(!is.na(hu))

plot(CALPAL["hu"])

