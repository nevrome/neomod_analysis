library(ggplot2)
library(magrittr)
library(igraph)
library(gluesless)

#### research area ####
area <- rgdal::readOGR(
  dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
)

research_area_border <- rgdal::readOGR(
  dsn = "../neomod_datapool/geodata/research_areas/extent.shp"
)

research_area <- rgeos::gIntersection(
  area, research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

research_area_df <- ggplot2::fortify(research_area)

save(research_area_df, file = "../neomod_datapool/model_data/research_area_df.RData")

#### research area hex ####
research_area_hex_df <- gluesless::hexify(
  area = research_area,
  hexproj  = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  bufferwidth = 2000,
  hexcellsize = 75000
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

# transform from wgs84 to mollweide and create SpatialPointsDataFrame
nodes_spdf <- sp::SpatialPointsDataFrame(
    coords = dplyr::select_(nodes, "x", "y"), data = nodes,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  ) %>% 
  sp::spTransform(
    sp::CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  )

#plot(nodes_spdf@coords)

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
edges <- distmat %>% 
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
  # group by from
  dplyr::group_by(from) %>%
  dplyr::filter(
    # remove autocorrelation
    distance != 0,
    # reduce connections to every node within 100km
    distance <= 100
  ) %>%
  dplyr::arrange(from)

# distance value
# ?

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