#### research area ####

# load manually crafted research area shape file, transform it to 
# EPSG:102013 and store the result

research_area <- sf::st_read(
  "manually_changed_data/research_area.shp"
) %>% sf::st_transform(102013)

save(
  research_area, 
  file = "../neomod_datapool/bronze_age/spatial_data/research_area.RData"
)



#### land outline ####

# load natural earth data land outline shape, crop it approximately to 
# Europe, transform it to EPSG:102013 and store the result

land_outline <- sf::st_read(
  "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
) %>%
  sf::st_crop(xmin = -20, ymin = 35, xmax = 35, ymax = 65) %>%
  sf::st_transform(102013)

save(
  land_outline, 
  file = "../neomod_datapool/bronze_age/spatial_data/land_outline.RData"
  )



#### area ####

# crop land outline to research area and store the result

area <- sf::st_intersection(sf::st_buffer(land_outline, 0), research_area)

save(
  area, 
  file = "../neomod_datapool/bronze_age/spatial_data/area.RData"
)



#### extended area ####

# crop land outline to bbox of 

# plot(land_outline$geometry)
# plot(research_area$geometry, add = T, border = "blue")
# plot(extended_area, add = T, border = "red")

extended_research_area <- sf::st_bbox(research_area) %>% sf::st_as_sfc()

extended_area <- sf::st_intersection(sf::st_buffer(land_outline, 0), extended_research_area)

# plot(extended_area$geometry)
# plot(research_area$geometry, add = T, border = "blue")
# plot(extended_research_area, add = T, border = "red")

save(
  extended_area, 
  file = "../neomod_datapool/bronze_age/spatial_data/extended_area.RData"
)


