library(magrittr)

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
load("../neomod_datapool/bronze_age/bronze1.RData")
bronze <- bronze1 %>% as.data.frame()

# date data frame to spatial data.frame
library(sp)
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

# plot(bronze_ppp)

# calculate density raster
dens <- spatstat::density.ppp(bronze_ppp, 70000)# %>% plot

# plot(dens)
# plot(bronze_ppp, add = T)
# graphics::contour(dens, levels = 0.0000000001, add = T) 

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
research_area_raw_1 <- contourline@polygons[[1]]@Polygons %>%
  magrittr::extract(which(poly_area %in% sort(poly_area, decreasing = T)[1:7])) %>%
  sp::Polygons(ID = 1) %>%
  list() %>%
  sp::SpatialPolygons() %>%
  `proj4string<-`(CRS(crs_moll)) 

# reduce point selection to those inside of the biggest polygons
bronze_in_dens_area <- bronze_moll[!is.na(sp::over(bronze_moll, research_area_raw_1)),]

# calculate the convex hull of these points
convex_hull_dens <- rgeos::gConvexHull(bronze_in_dens_area)

# increase the size of the convex hull
research_area_raw_2 <- convex_hull_dens %>% 
  rgeos::gBuffer(width = 100000)

# transform enlarged hull coordinates to geographic coordinate system
research_area_wgs <- sp::spTransform(x = research_area_raw_2, CRSobj = CRS(crs_wgs))

# plot(research_area_wgs, col = "red", xlim = c(-10, 20), ylim = c(30, 65))
# plot(area, add = TRUE)
# plot(bronze, add = TRUE)

# export 
rgdal::writeOGR(
  as(research_area_wgs, "SpatialPolygonsDataFrame"), 
  dsn = "../neomod_datapool/bronze_age/research_area/",
  layer = "research_area",
  driver = "ESRI Shapefile"
)

research_area_df <- ggplot2::fortify(research_area_wgs)
save(research_area_df, file = "../neomod_datapool/bronze_age/research_area/research_area_df.RData")
