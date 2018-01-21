# load regions
regions <- sf::st_read(
  "manually_changed_data/regionen2017g.shp"
) %>% 
  sf::st_transform(4326)

land_outline_raw <- sf::st_read(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
) 

rectangle_around_regions <- sf::st_bbox(regions) %>%
  sf:::st_as_sfc.bbox()

plot(rectangle_around_regions)
plot(regions)
plot(land_outline_raw)

land_outline <- sf::st_intersection(land_outline_raw, rectangle_around_regions)

plot(land_outline)

save(land_outline, file = "../neomod_datapool/bronze_age/space_and_network/land_outline_sf.RData")


