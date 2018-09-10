library(magrittr)

lapply(
  c(
    "./R/real_world_data_preparation",
    "./R/real_world_analysis/general_maps",
    "./R/real_world_analysis/development",
    "./R/real_world_analysis/general_observations",
    "./R/real_world_analysis/sed",
    "./R/other_analysis"
  ), 
  function(x) {
    pbapply::pblapply(
      list.files(x, full.names = TRUE),
      function(y) {
        message("\n\n###### ", y, " ######\n\n")
        source(y)
        rm(list = ls())
      }
    )
  }
)
