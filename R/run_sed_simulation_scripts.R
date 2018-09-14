library(magrittr)

path_to_sed_sim_scripts <- "R/simulation/sed"

empty <- lapply(
  list.files(path_to_sed_sim_scripts, full.names = TRUE),
  function(y) {
    message("\n###### ", y, " ######\n")
    source(y)
    rm(list = ls())
  }
)
