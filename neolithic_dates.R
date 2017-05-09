library(sp)
library(proj4)

neo_gronenborn <- rgdal::readOGR(
  dsn = "/home/clemens/neomod/neomod_datapool/neolithic_expansion/gronenborn_map/neolithic.shp"
)

wgs <- "+init=epsg:4326"

neo_gronenborn2 <- spTransform(neo_gronenborn, CRS(wgs))

CALPAL %<>% dplyr::filter(
  !is.na(LONGITUDE) &
  !is.na(LATITUDE)
)

CALPALspdf <- SpatialPointsDataFrame(
  coords = data.frame(
    as.numeric(CALPAL$LONGITUDE), 
    as.numeric(CALPAL$LATITUDE)
  ),
  data = CALPAL,
  proj4string = CRS(wgs)
)



plot(neo_gronenborn2, axes = TRUE)
plot(CALPALspdf, add = TRUE)

plot(CALPALspdf, axes = TRUE)
