# formally part og the prepare_hex_raster_script: no excluded for the moment

#### determine distance values for graph edges ####

# random distance values: 
# edges %<>% dplyr::mutate(
#   spatial_dist = distance,
#   distance = nrow(.) %>% runif(., min = 0, max = 3) %>% abs %>% round(0)
# )

# load already present data
load("../neomod_datapool/model_data/hex_graph_egdes.RData")
load("../neomod_datapool/model_data/hex_graph_nodes.RData")

wgs <- 4326

# load gronenborn map shape
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

# nodes as sf object
nodes_sf <- nodes %>% sf::st_as_sf(
  coords = c("x", "y"),
  crs = wgs
)

# join graph info with gronenborn map info:
# intersect and find oldest present date
nodes_sf %<>% mutate(
  within = st_within(., neo_gron),
  start = map_int(within, function(x){
    if (length(x) > 0) {
      map_int(x, function(x){
        neo_gron$start[which(x == neo_gron$shape_index)]
      }) %>% max %>% return
    } else {
      NA
    }
  })
)

nodes %<>% 
  # add new info to nodes df
  mutate(start = nodes_sf$start) %>% 
  # calculate resistance value from start
  mutate(
    resistance = abs(start/max(start, na.rm = TRUE) - 1) %>% round(., 2)
  )

#nodes %>% ggplot(aes(x, y, color = resistance)) + geom_point()

# add resistance to edges 
edges %<>% dplyr::mutate(
  spatial_dist = distance,
  distance = map2_dbl(from, to, function(x, y){
    fr <- nodes[which(x == nodes$name), ]$resistance
    tr <- nodes[which(y == nodes$name), ]$resistance
    if (!is.na(fr) | !is.na(tr)) {
      res <- min(c(fr, tr), na.rm = TRUE)
    } else {
      res <- max(nodes$resistance, na.rm = TRUE) + 0.5
    }
    return(res)
  }
  )
)
