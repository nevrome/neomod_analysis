library(ggplot2)
library(magrittr)
library(igraph)
library(gluesless)
library(pbapply)
library(sf)
library(readr)
library(dplyr)
library(purrr)
library(sp)
library(rgeos)



#### research area ####

# load data
area <- rgdal::readOGR(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
)

#plot(area)

research_area_border <- rgdal::readOGR(
  dsn = "../neomod_datapool/geodata/research_areas/extent.shp"
)

research_area_highres <- rgeos::gIntersection(
  area, research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

#plot(research_area_highres)

# adjust map
crs_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

rahm <- research_area_highres %>% 
  sp::spTransform(CRS(crs_moll)) %>%
  rgeos::gBuffer(width = 15000) %>%
  rgeos::gSimplify(tol = 15000, topologyPreserve = TRUE) %>%
  sp::spTransform(CRS(crs_wgs))

#plot(rahm)

research_area <- rahm

research_area_df <- ggplot2::fortify(research_area)

save(research_area_df, file = "../neomod_datapool/model_data/research_area_df.RData")



#### research area hex ####
research_area_hex_df <- gluesless::hexify(
  area = research_area,
  hexproj  = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  bufferwidth = 2000,
  #hexcellsize = 75000
  hexcellsize = 70000
  ) %>%
  ggplot2::fortify(region = "id")

save(research_area_hex_df, file = "../neomod_datapool/model_data/research_area_hex_df.RData")



#### construct nodes and edges ####

# get node point coordinates as the mean coordinates of the hex cell corners
nodes <- research_area_hex_df %>%
  dplyr::group_by_("id") %>%
  dplyr::summarize_("x" = ~mean(long), "y" = ~mean(lat)) %>%
  dplyr::mutate_(
    "name" = ~seq(from = 0, to = length(id) - 1, by = 1)
  ) %>%
  dplyr::select(name, x, y)

save(nodes, file = "../neomod_datapool/model_data/hex_graph_nodes.RData")

# transform from wgs84 to mollweide and create SpatialPointsDataFrame
nodes_spdf <- sp::SpatialPointsDataFrame(
    coords = dplyr::select_(nodes, "x", "y"), data = nodes,
    proj4string = sp::CRS(crs_wgs)
  ) %>% 
  sp::spTransform(
    sp::CRS(crs_moll)
  )

# plot(nodes_spdf@coords)
# plot(nodes_spdf, cex = 0.01)
# text(nodes_spdf, cex = 0.4)

# node distance point matrix
distmat <- rgeos::gDistance(nodes_spdf, byid = TRUE)

# approach1: select 6 closest nodes and connect them: distance value is random.
# edges <- apply(distmat, 1, function(x) {
#     order(x, decreasing = F)[c(2:7)] - 1
#   }) %>% t %>%
#   as.data.frame() %>%
#   set_names(paste0("n_", 1:6)) %>%
#   dplyr::mutate(nodes = as.numeric(row.names(.)) - 1) %>%
#   tidyr::gather(
#     ., key = schnupp, value = neighs,
#     n_1, n_2, n_3, n_4, n_5, n_6
#   ) %>%
#   dplyr::select(nodes, neighs) %>%
#   dplyr::rename(from = nodes, to = neighs) %>%
#   dplyr::mutate(distance = runif(nrow(.), 0, 100) %>% round(0))

# aproach2:  

# prepare distance table
edges_complete <- distmat %>% 
  # convert distmat to a tall, tidy format
  reshape2::melt() %>%
  tibble::as.tibble() %>%
  dplyr::rename(
    "from" = "Var1",
    "to" = "Var2",
    "distance" = "value"
  ) %>%
  dplyr::mutate(
    # index != name
    from = from - 1,
    to = to - 1,
    # transform distance from meter to kilometer
    distance = distance/1000
  ) %>%
  dplyr::arrange(from) %>%
  # remove autocorrelation
  dplyr::filter(
    distance != 0
  ) %>%
  # group by "from"
  dplyr::group_by(from) %>%
  # count number of connections per "from" node
  dplyr::mutate(
    n_con = sum(distance <= 100)
  ) %>%
  # ungroup
  dplyr::ungroup() %>%
  # join with node coordinate information
  dplyr::left_join(
    nodes, by = c("from" = "name")
  ) %>% dplyr::rename(
    "x.from" = "x",
    "y.from" = "y"
  ) %>% dplyr::left_join(
    nodes, by = c("to" = "name")
  ) %>% dplyr::rename(
    "x.to" = "x",
    "y.to" = "y"
  )

# filter to get potential over-water connections
edges_over_water <- edges_complete %>%
  # remove connections with "from" nodes with the usual 6 or 
  # more connections
  dplyr::filter(
    n_con < 6
  ) %>%
  # remove connections with "to" partnernodes respectively
  dplyr::filter(
    to %in% unique(.$from)
  ) %>%
  # remove every edge with a distance value > 250km 
  dplyr::filter(
    distance <= 250
  )

research_area_lines <- as(research_area, "SpatialLines")
# plot(research_area_lines)

# determine number of intersections (Edges & Land)
number_of_intersections <- edges_over_water %>%
  apply(1, function(x) {SpatialLines(
    cbind(c(x["x.from"], x["x.to"]), c(x["y.from"], x["y.to"])) %>%
      sp::Line() %>%
      sp::Lines(ID = "a") %>%
      list,
    proj4string = sp::CRS(crs_wgs)
  )}) %>%
  # (!) most time consuming
  # lapply(
  #   function(x){
  #     rgeos::gIntersection(
  #       x, research_area_lines
  #     )
  #   }
  # ) %>%
  pbapply::pblapply(function(x){
        rgeos::gIntersection(
          x, research_area_lines
        )
      }
    ) %>%
  # get vector with number of intersections
  lapply(function(x){
    if(class(x) == "SpatialPoints") {
      x %>% coordinates %>% nrow
    } else {
      NA
    }
  }) %>% unlist

# add intersections information to connection information
edges_over_water %<>%
  dplyr::select(from, to) %>%
  dplyr::mutate(
    inter = number_of_intersections
  )

# join to have interaction information in complete distance table
edges_complete %<>%
  dplyr::left_join(
    edges_over_water, by = c("from", "to")
  )
  
# filter edges_complete to fit every criteria
edges <- edges_complete %>%
  dplyr::filter(
    distance <= 100 |
      (distance <= 250 & inter == 2)
  )

save(edges, file = "../neomod_datapool/model_data/hex_graph_egdes.RData")



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



#### create graph from nodes and edges with distance info ####  
g <- igraph::graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = nodes
  ) %>% igraph::simplify(
    edge.attr.comb = "mean"
  )

hex_graph <- set.graph.attribute(g, "graph_name", "testgraph")

#gluesless::plot_world(hex_graph, world = research_area_df, plotedges = TRUE)

save(hex_graph, file = "../neomod_datapool/model_data/hex_graph.RData")

#####

load("../neomod_datapool/model_data/research_area_hex_df.RData")
load("../neomod_datapool/model_data/research_area_df.RData")
load("../neomod_datapool/model_data/hex_graph.RData")
gluesless::plot_world(
  hex_graph, 
  world = research_area_df, 
  #hex = research_area_hex_df,
  plotedges = T
)
