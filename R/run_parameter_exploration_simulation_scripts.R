library(magrittr)

if (!dir.exists("../simulationdata")) {
  dir.create("../simulationdata")
}

lapply(
  c(
    "R/simulation/parameter_exploration/popsize_crossregions",
    "R/simulation/parameter_exploration/startprop_distancemat",
    "R/simulation/parameter_exploration/vertitrans"
  ), 
  function(x) {
    pbapply::pblapply(
      list.files(x, full.names = TRUE),
      function(y) {
        message("\n###### ", y, " ######\n")
        source(y)
        rm(list = ls())
      }
    )
  }
)
