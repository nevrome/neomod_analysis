# load libraries
library(sf)
library(magrittr)
library(neolithicRtools)
library(readr)
library(purrr)
library(dplyr)
library(ggplot2)

wgs <- 4326

neo_gron <- sf::st_read(
  dsn = "~/neomod/neomod_datapool/neolithic_expansion/gronenborn_map/neolithic.shp"
  ) %>% sf::st_transform(wgs) 

starts <- read_csv("~/neomod/neomod_datapool/neolithic_expansion/gronenborn_map/gronenborn.txt")

neo_gron <- neo_gron %>% dplyr::left_join(
    starts, "id"
  ) %>% dplyr::mutate(
    shape_index = 1:nrow(.)
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

# calculate, which 14C points are within which neogron shape
shape_index <- st_within(CALPAL, neo_gron) %>% 
  as.character() %>%
  as.numeric() 

  
CALPAL2 <- CALPAL %>% 
  # add column for shape indizes
  cbind(shape_index) %>%
  # remove all dates outside of the neogron shapes
  dplyr::filter(!is.na(shape_index)) %>%
  # merge neo_gron shape information with 14C dates
  dplyr::left_join(
    as.data.frame(neo_gron)[c("id", "start", "shape_index")], 
    by = "shape_index"
  ) %>%
  # remove dates to far from the neogron-timeframe
  filter(
    !(CALAGE <= start - 500) &
      !(CALAGE >= start + 500)
  ) %>%
  # calculate time distance
  dplyr::mutate(
    time_distance = start - CALAGE
  )


plot(neo_gron["shape_index"])
plot(CALPAL2["time_distance"], add = TRUE)

ggplot() +
  geom_sf(data = neo_gron) +
  geom_sf(aes(color = time_distance), CALPAL2)

