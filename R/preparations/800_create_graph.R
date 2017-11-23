load("../neomod_datapool/bronze_age/space_and_network/region_graph_egdes.RData")
load("../neomod_datapool/bronze_age/space_and_network/region_graph_nodes.RData")

# create graph
g <- igraph::graph_from_data_frame(
  edges,
  directed = FALSE,
  vertices = nodes
) %>% igraph::simplify(
  edge.attr.comb = "mean"
)

regions_graph <- igraph::set.graph.attribute(g, "graph_name", "bronze_age_regions")

#gluesless::plot_world(hex_graph, world = research_area_df, plotedges = TRUE)

save(regions_graph, file = "../neomod_datapool/bronze_age/space_and_network/regions_graph.RData")

