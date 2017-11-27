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
  plotedges = F
) -> worldplot

for(i in 2001:1) {
  hu <- gluesless::plot_state(worldplot, states = nodes_info_spread, i)
  hu %>%
    ggsave(
      paste0("/home/clemens/neomod/neomod_datapool/bronze_age/movie/", 2001 - i ,".jpeg"),
      plot = .,
      device = "jpeg",
      scale = 1,
      dpi = 300,
      width = 200, height = 150, units = "mm",
      limitsize = F
    )
}

# ffmpeg -r 30 -start_number 0 -i movie/%d.jpeg -vcodec mpeg4 -r 30 the_movie_2.avi
