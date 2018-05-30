load("../neomod_datapool/bronze_age/research_area/hex_graph_nodes.RData")
load("../neomod_datapool/bronze_age/research_area/hex_graph_egdes.RData")

# create graph
g <- igraph::graph_from_data_frame(
  edges,
  directed = FALSE,
  vertices = nodes
) %>% igraph::simplify(
  edge.attr.comb = "mean"
)

hex_graph <- igraph::set.graph.attribute(g, "graph_name", "testgraph")

#gluesless::plot_world(hex_graph, world = research_area_df, plotedges = TRUE)

save(hex_graph, file = "../neomod_datapool/bronze_age/research_area/hex_graph.RData")

#####

load("../neomod_datapool/bronze_age/research_area/research_area_hex_df.RData")
load("../neomod_datapool/bronze_age/research_area/research_area_df.RData")
load("../neomod_datapool/bronze_age/research_area/hex_graph.RData")
gluesless::plot_world(
  hex_graph, 
  world = research_area_df, 
  #hex = research_area_hex_df,
  plotedges = T
)
