library(ggplot2)
library(magrittr)
library(igraph)
library(gluesless)
library(pbapply)

#### research area ####
area <- rgdal::readOGR(
  dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
)

#plot(area)

research_area_border <- rgdal::readOGR(
  dsn = "../neomod_datapool/geodata/research_areas/extent.shp"
)

research_area_highres <- rgeos::gIntersection(
  area, research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

# plot(research_area_highres)
# research_area <- research_area_highres %>%
#   rgeos::gSimplify(tol = 0.4, topologyPreserve=TRUE) %>%
#   rgeos::gSimplify(tol = 0.2)
# plot(research_area)
research_area <- research_area_highres


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

#### graph ####
# get node point coordinates as the mean coordinates of the hex cell corners
nodes <- research_area_hex_df %>%
  dplyr::group_by_("id") %>%
  dplyr::summarize_("x" = ~mean(long), "y" = ~mean(lat)) %>%
  dplyr::mutate_(
    "name" = ~seq(from = 0, to = length(id) - 1, by = 1)
  ) %>%
  dplyr::select(name, x, y)

save(nodes, file = "../neomod_datapool/model_data/hex_graph_nodes.RData")

wgs_crs <- proj4string(research_area)
moll_crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# transform from wgs84 to mollweide and create SpatialPointsDataFrame
nodes_spdf <- sp::SpatialPointsDataFrame(
    coords = dplyr::select_(nodes, "x", "y"), data = nodes,
    proj4string = sp::CRS(wgs_crs)
  ) %>% 
  sp::spTransform(
    sp::CRS(moll_crs)
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
  # remove every edge with a distance value > 300km 
  dplyr::filter(
    distance <= 300
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
    proj4string = CRS(wgs_crs)
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
      (distance <= 300 & inter == 2)
  )

save(edges, file = "../neomod_datapool/model_data/hex_graph_egdes.RData")

# distance value
load("../neomod_datapool/model_data/hex_graph_egdes.RData")
load("../neomod_datapool/model_data/hex_graph_nodes.RData")

edges %<>% dplyr::mutate(
  spatial_dist = distance,
  distance = nrow(.) %>% runif(., min = 0, max = 3) %>% abs %>% round(0)
)

# create graph from nodes and edges  
g <- igraph::graph_from_data_frame(
  edges,
  directed = FALSE,
  vertices = nodes
  ) %>%
  igraph::simplify(
    edge.attr.comb = "mean"
  )

hex_graph <- set.graph.attribute(g, "graph_name", "testgraph")

gluesless::plot_world(hex_graph, world = research_area_df, plotedges = TRUE)

save(hex_graph, file = "../neomod_datapool/model_data/hex_graph.RData")

#####

load("../neomod_datapool/model_data/research_area_hex_df.RData")
load("../neomod_datapool/model_data/research_area_df.RData")
load("../neomod_datapool/model_data/hex_graph.RData")
gluesless::plot_world(
  hex_graph, 
  world = research_area_df, 
  hex = research_area_hex_df,
  plotedges = FALSE
)
