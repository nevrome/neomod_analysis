regions <- sf::st_read(
  "manually_changed_data/regionen2017g.shp"
) %>% 
  sf::st_transform(4326)

load("../neomod_datapool/bronze_age/space_and_network/land_outline_sf.RData")

# plot region pictograms

land_out_geom <- land_outline$geometry

path = "../neomod_datapool/bronze_age/region_pictograms/"

for (i in 1:nrow(regions)) {
  
  one_region <- regions[i, ]
  one_region_geom <- one_region$geometry
  
  one_region_name <- one_region$NAME %>% gsub(" ", "_", ., fixed = TRUE)
  
  png(
    filename = paste0(path, one_region_name, ".png"),
    width = 87*4, height = 100*4, units = "px", res = 300
  )
  par(mar = c(0,0,0,0),
      pin = c(4,2),
      pty = "m",
      xaxs = "i",
      xaxt = 'n',
      xpd = FALSE,
      yaxs = "i",
      yaxt = 'n',
      bg = NA)
  plot(land_out_geom, border = "black" , col = "grey85", lwd = 1)
  plot(one_region_geom, border = "red", col = NA, lwd = 3, add = TRUE)
  dev.off()

}
