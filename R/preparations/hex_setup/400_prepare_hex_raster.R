load("../neomod_datapool/bronze_age/research_area/research_area_land.RData")

# create hex raster
research_area_hex <- gluesless::hexify(
  area = research_area,
  hexproj  = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  bufferwidth = 1000,
  #hexcellsize = 75000
  hexcellsize = 100000
)

save(
  research_area_hex, 
  file = "../neomod_datapool/bronze_age/research_area/research_area_hex.RData"
)

rgdal::writeOGR(
  obj = as(research_area_hex, "SpatialPolygonsDataFrame"), 
  dsn = "../neomod_datapool/bronze_age/research_area/",
  layer = "research_area_hex",
  driver = "ESRI Shapefile",
  overwrite_layer = TRUE
)

research_area_hex_df <- research_area_hex %>%
  ggplot2::fortify(region = "id")

save(research_area_hex_df, file = "../neomod_datapool/bronze_age/research_area/research_area_hex_df.RData")
