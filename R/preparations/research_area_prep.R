# adjust map with buffering and simplification
crs_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

# load world land area shape
area <- rgdal::readOGR(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
)

# load bronze age data
load("../neomod_datapool/bronze_age/bronze2.RData")
bronze <- bronze2 

# date data frame to spatial data.frame
coordinates(bronze) <- ~lon+lat
proj4string(bronze) <- crs_wgs

# transform coordinates to geographic coordinate system
bronze_moll <- sp::spTransform(x = bronze, CRSobj = CRS(crs_moll))

# spatial data.frame to ppp
bronze_ppp <- as(as(bronze_moll, "SpatialPoints"), "ppp")
# increase window size
cur_win <- spatstat::Window(bronze_ppp)
inc <- 1000000
spatstat::Window(bronze_ppp) <- spatstat::owin(
  xrange = cur_win$xrange + c(-inc, inc), 
  yrange = cur_win$yrange + c(-inc, inc)
)

# remove spatial outliers
# filter_by_proximity <- function(pepepe, dist) {
#   d <- nndist(pepepe)
#   z <- which(!(d > dist))
#   return(pepepe[z,])
# }
# bronze_ppp_simple <- filter_by_proximity(bronze_ppp, dist = 100000)




#plot(bronze_ppp)

# calculate density raster
dens <- spatstat::density.ppp(bronze_ppp_simple, 70000)# %>% plot

# plot(dens)
# plot(bronze_ppp, add = T)
# graphics::contour(dens, levels = 0.00000000002, add = T) 

# extract contour lines
contourline <- grDevices::contourLines(
  x = dens$xcol, 
  y = dens$yrow, 
  z = t(dens$v), 
  levels = 0.0000000001
) %>%
  maptools::ContourLines2SLDF(proj4string = CRS(crs_moll)) %>%
  # transform to spatial polygons
  maptools::SpatialLines2PolySet() %>%
  maptools::PolySet2SpatialPolygons() %>% # needs library(PBSmapping)
  `proj4string<-`(CRS(crs_moll)) %>%
  as(., "SpatialPolygonsDataFrame")

# search biggest polygon
polies <- contourline@polygons[[1]]@Polygons
poly_area <- c()
for (i in 1:length(polies)) {
  cur_poly <- polies[[i]]
  poly_area[i] <- cur_poly@area
} 

# select biggest polygon and create a new SpatialPolygons object
research_area_raw_1 <- list(
    contourline@polygons[[1]]@Polygons[[which(poly_area == max(poly_area))]],
    contourline@polygons[[1]]@Polygons[[which(poly_area == max(poly_area[poly_area!=max(poly_area)]))]]
  ) %>%
  sp::Polygons(ID = 1) %>%
  list() %>%
  sp::SpatialPolygons() %>%
  `proj4string<-`(CRS(crs_moll)) 

research_area_raw_2 <- research_area_raw_1 %>% 
  rgeos::gBuffer(width = 10000)# %>%
  #rgeos::gSimplify(tol = 100000, topologyPreserve = TRUE) #%>% plot

# transform contour coordinates to geographic coordinate system
research_area_wgs <- sp::spTransform(x = research_area_raw_2, CRSobj = CRS(crs_wgs))

plot(research_area_wgs, col = "red", xlim = c(-10, 20), ylim = c(30, 65))
plot(area, add = TRUE)
plot(bronze, add = TRUE)

# research_area_border <- rgdal::readOGR(
#   dsn = "../neomod_datapool/geodata/research_areas/extent.shp"
# )
# 
# 
# # clip map to research area 
# research_area_highres <- rgeos::gIntersection(
#   area, research_area_border, byid = TRUE,
#   drop_lower_td = TRUE
# )
# 
# 
# #plot(research_area_highres)
# 
# 
# 
# rahm <- research_area_highres %>% 
#   sp::spTransform(CRS(crs_moll)) %>%
#   rgeos::gBuffer(width = 15000) %>%
#   rgeos::gSimplify(tol = 15000, topologyPreserve = TRUE) %>%
#   sp::spTransform(CRS(crs_wgs))
# 
# #plot(rahm)
# 
# research_area <- rahm
# 
# research_area_df <- ggplot2::fortify(research_area)
# 
# save(research_area_df, file = "../neomod_datapool/model_data/research_area_df.RData")