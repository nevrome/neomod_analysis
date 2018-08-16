load("data_analysis/regions.RData")
load("data_analysis/extended_area.RData")
extended_area <- extended_area$geometry

#### colour #### 

regions_ordered <- c(
  "Southeastern Central Europe",
  "Poland",
  "Southern Germany",
  "Northeastern France",
  "Northern Germany",
  "Southern Scandinavia",
  "Benelux",
  "England"
)

path = "figures_plots/region_pictograms_colour/"

colour_vector <- c(
  "Southeastern Central Europe" = "#999999", 
  "Poland" = "#ffe500", 
  "Southern Germany" = "#56B4E9", 
  "Northeastern France" = "#009E73", 
  "Northern Germany" = "#000000", 
  "Southern Scandinavia" = "#0072B2", 
  "Benelux" = "#D55E00", 
  "England" = "#CC79A7"
)

for (i in 1:nrow(regions)) {
  
  one_region <- regions[regions$NAME == regions_ordered[i], ]
  one_region_buffer <- one_region %>%
    sf::st_buffer(400000)
  
  one_region_geom <- one_region$geometry
  one_region_buffer_geom <- one_region_buffer$geometry
  
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
  plot(extended_area, border = NA , col = "grey85", lwd = 2)
  plot(one_region_buffer_geom, border = NA, col = scales::alpha(colour_vector[i], 0.4), add = TRUE)
  plot(one_region_geom, border = NA, col = colour_vector[i], add = TRUE)
  dev.off()
  
}

