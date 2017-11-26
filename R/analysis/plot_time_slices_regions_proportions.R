load("../neomod_datapool/bronze_age/space_and_network/regions_graph.RData")
load("../neomod_datapool/bronze_age/space_and_network/region_graph_egdes.RData")
load("../neomod_datapool/bronze_age/space_and_network/region_graph_nodes.RData")
load("../neomod_datapool/bronze_age/space_and_network/region_graph_nodes_info.RData")
load("../neomod_datapool/bronze_age/space_and_network/land_outline_df.RData")

crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

regions <- rgdal::readOGR(
  dsn = "../neomod_analysis/manually_changed_data/regionen2017g.shp"
) %>%
  sp::spTransform(sp::CRS(crs_wgs)) %>%
  ggplot2::fortify()

nodes_info_spread <- nodes_info %>%
  lapply(function(x){
    x %>%
      tidyr::spread(ideas, proportion)
  })

gluesless::plot_world(
  graph = regions_graph,
  world = land_outline_df,
  regions = regions,
  nodes = NULL,
  plotedges = F
) -> worldplot

gluesless::plot_state(worldplot, states = nodes_info_spread, 2000)


