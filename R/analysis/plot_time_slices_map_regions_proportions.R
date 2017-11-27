load("../neomod_datapool/bronze_age/space_and_network/regions_graph.RData")
load("../neomod_datapool/bronze_age/space_and_network/region_graph_egdes.RData")
load("../neomod_datapool/bronze_age/space_and_network/region_graph_nodes.RData")
load("../neomod_datapool/bronze_age/space_and_network/region_graph_nodes_info.RData")
load("../neomod_datapool/bronze_age/space_and_network/land_outline_sf.RData")

regions <- sf::st_read(
  "manually_changed_data/regionen2017g.shp"
) %>% 
  sf::st_transform(4326)

nodes_info_spread <- nodes_info %>%
  lapply(function(x){
    x %>%
      tidyr::spread(ideas, proportion)
  })

gluesless::plot_world(
  graph = regions_graph,
  world_polygon = land_outline,
  regions = regions,
  nodes = NULL,
  plotedges = T
) -> worldplot

gluesless::plot_state(worldplot, states = nodes_info_spread, 2000)


