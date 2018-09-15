library(magrittr)

lapply(
  c(
    "R/simulation/parameter_exploration/popsize_crossregions"
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
