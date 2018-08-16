pbapply::pblapply(
  list.files("./R/real_world_data_preparation", full.names = TRUE),
  function(x) {
    source(x)
    rm(list = ls())
  }
)
