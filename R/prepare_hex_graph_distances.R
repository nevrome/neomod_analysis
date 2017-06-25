# load libraries
library(sf)
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(purrr)


#### load graph data ####
load("../neomod_datapool/model_data/hex_graph_egdes.RData")
load("../neomod_datapool/model_data/hex_graph_nodes.RData")

# edges %<>% dplyr::mutate(
#   spatial_dist = distance,
#   distance = nrow(.) %>% runif(., min = 0, max = 3) %>% abs %>% round(0)
# )

#### load and prepare gronenborn map data ####

wgs <- 4326

# load map
neo_gron <- sf::st_read(
    dsn = "~/neomod/neomod_datapool/neolithic_expansion/gronenborn_map/neolithic.shp"
  )

# load meta info for polygons
starts <- read_csv("~/neomod/neomod_datapool/neolithic_expansion/gronenborn_map/gronenborn.txt")

neo_gron %<>% 
  # increase polygon size by buffering
  st_buffer(70000) %>%
  # transform to wgs84
  sf::st_transform(wgs) %>% 
  # merge with meta info
  dplyr::left_join(
    starts, "id"
  ) %>% dplyr::mutate(
    shape_index = 1:nrow(.)
  )

#### join graph info with gronenborn map info ####

# nodes as sf object
nodes_sf <- nodes %>% sf::st_as_sf(
  coords = c("x", "y"),
  crs = wgs
)

# intersect and find oldest present date
nudel <- nodes_sf %>% mutate(
    within = st_within(., neo_gron),
    within = map_int(within, function(x){
      if (length(x) > 0) {
        map_int(x, function(x){
          neo_gron$start[which(x == neo_gron$shape_index)]
        }) %>% max %>% return
      } else {
        NA
      }
    })
  )

#####

no <- nodes %>% 
  cbind(shape_index) %>%
  dplyr::left_join(
    as.data.frame(neo_gron)[c("id", "start", "shape_index")], 
    by = "shape_index"
  ) 

no %>% ggplot(aes(x, y, color = start)) + geom_point()



load("../neomod_datapool/model_data/research_area_hex_df.RData")
load("../neomod_datapool/model_data/research_area_df.RData")
load("../neomod_datapool/model_data/hex_graph.RData")
gluesless::plot_world(
  hex_graph, 
  world = research_area_df, 
  hex = research_area_hex_df,
  plotedges = FALSE
)