#### natural earth data ####

# land_outline
land_outline <- rnaturalearth::ne_download(
  scale = 50, type = 'land', category = 'physical'
) %>% sf::st_as_sf()
save(land_outline, file = "data_geo/land_outline.RData")

# countries
countries <- rnaturalearth::ne_download(
  scale = 50, type = 'countries', category = 'cultural'
) %>% sf::st_as_sf()
save(countries, file = "data_geo/countries.RData")

# rivers
rivers <- rnaturalearth::ne_download(
  scale = 50, type = 'rivers_lake_centerlines', category = 'physical'
) %>% sf::st_as_sf()
save(rivers, file = "data_geo/rivers.RData")

# lakes
lakes <- rnaturalearth::ne_download(
  scale = 50, type = 'lakes', category = 'physical'
) %>% sf::st_as_sf()
save(lakes, file = "data_geo/lakes.RData")



#### research area ####

# load manually crafted research area shape file, transform it to 
# EPSG:102013 and store the result

research_area <- sf::st_read(
  "data_manually_prepared/research_area.shp"
) %>% sf::st_transform(102013)
save(research_area, file = "data_analysis/research_area.RData")



#### area ####

# load natural earth data land outline shape, crop it approximately to 
# Europe, transform it to EPSG:102013, crop it to the research area and store the result

land_outline_small <- land_outline %>%
  sf::st_crop(xmin = -20, ymin = 35, xmax = 35, ymax = 65) %>%
  sf::st_transform(102013)
area <- sf::st_intersection(sf::st_buffer(land_outline_small, 0), research_area)
save(area, file = "data_analysis/area.RData")



#### extended area ####

# crop land outline to bbox of 

# plot(land_outline$geometry)
# plot(research_area$geometry, add = T, border = "blue")
# plot(extended_area, add = T, border = "red")
extended_research_area <- sf::st_bbox(research_area) %>% sf::st_as_sfc()
extended_area <- sf::st_intersection(sf::st_buffer(land_outline_small, 0), extended_research_area)
# plot(extended_area$geometry)
# plot(research_area$geometry, add = T, border = "blue")
# plot(extended_research_area, add = T, border = "red")
save(extended_area, file = "data_analysis/extended_area.RData")
