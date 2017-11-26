crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# load regions
regions <- rgdal::readOGR(
  dsn = "manually_changed_data/regionen2017g.shp"
) %>%
  sp::spTransform(sp::CRS(crs_wgs))

land_outline_raw <- rgdal::readOGR(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
) 

rectangle_around_regions <- rgeos::gEnvelope(regions)

sp::plot(rectangle_around_regions)
sp::plot(regions, add = T)
sp::plot(land_outline_raw, add = T)

land_outline <- rgeos::gIntersection(land_outline_raw, rectangle_around_regions)

sp::plot(land_outline)
sp::plot(regions, add = T)

save(land_outline, file = "../neomod_datapool/bronze_age/space_and_network/land_outline_sp.RData")

land_outline_df <- land_outline %>% ggplot2::fortify()

save(land_outline_df, file = "../neomod_datapool/bronze_age/space_and_network/land_outline_df.RData")
