# load data
area <- rgdal::readOGR(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
)

#plot(area)

research_area_border <- rgdal::readOGR(
  dsn = "../neomod_datapool/bronze_age/research_area/research_area.shp"
)

# clip map to research area 
research_area_highres <- rgeos::gIntersection(
  area, research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

#plot(research_area_highres)

# adjust map with buffering and simplification
crs_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

rahm <- research_area_highres %>% 
  sp::spTransform(sp::CRS(crs_moll)) %>%
  rgeos::gBuffer(width = 10000) %>%
  rgeos::gSimplify(tol = 10000, topologyPreserve = TRUE) %>%
  sp::spTransform(sp::CRS(crs_wgs))

#plot(rahm)

research_area <- rahm

save(
  research_area, 
  file = "../neomod_datapool/bronze_age/research_area/research_area_land.RData"
)

research_area_df <- ggplot2::fortify(research_area)

save(
  research_area_df, 
  file = "../neomod_datapool/bronze_age/research_area/research_area_land_df.RData"
)
