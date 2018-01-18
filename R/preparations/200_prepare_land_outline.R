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

# plot region pictograms

land_out_geom <- land_outline$geometry
one_region <- regions$geometry[1]

png(filename="../neomod_datapool/bronze_age/region_pictograms/test.png", width = 86, height = 100, units = "px", res = 100)
  par(mar = c(0,0,0,0),
    pin = c(4,2),
    pty = "m",
    xaxs = "i",
    xaxt = 'n',
    xpd = FALSE,
    yaxs = "i",
    yaxt = 'n')
  plot(land_out_geom, col = "black")
  plot(one_region, border = "red", col = NA, lwd = 4, add = TRUE)
dev.off()
