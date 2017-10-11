# canceled...

# load and clip elevation contour lines
# from: https://www.arcgis.com/home/item.html?id=40285e34dd3a453db84ab3acbd81b37d
# to shapefile with: ogr2ogr -f 'ESRI SHAPEFILE' hushape hu.gdb
# reduced to all shapes with meters == 1200 with QGIS
# clipped with ogr2ogr -clipsrc clipping_area.shp contours_clipped.shp contours_polygons.shp
contours_world <- raster::shapefile(
  "~/neomod/neomod_datapool/geodata/world_contour_lines/contours_clipped.shp"
)

contours <- contours_world %>% rgeos::gIntersection(
  ., research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

contours_poly <- rgeos::gPolygonize(contours)