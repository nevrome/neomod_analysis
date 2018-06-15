load("../neomod_datapool/bronze_age/regions.RData")

load("../neomod_datapool/bronze_age/space_and_network/land_outline_sf.RData")

land_outline %<>% sf::st_transform(102013)

# plot region pictograms

land_out_geom <- land_outline$geometry

#### grey ####

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

#### colour #### 

regions_ordered <- c(
  "Austria and Czechia",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
)

path = "../neomod_datapool/bronze_age/region_pictograms_colour/"

colour_vector <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00", "#CC79A7")

for (i in 1:nrow(regions)) {
  
  one_region <- regions[regions$NAME == regions_ordered[i], ]
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
  plot(land_out_geom, border = "black" , col = colour_vector[i], lwd = 1)
  plot(one_region_geom, border = "red", col = NA, lwd = 3, add = TRUE)
  dev.off()
  
}

