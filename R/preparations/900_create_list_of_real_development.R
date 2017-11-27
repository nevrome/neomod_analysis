load("../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")
load("../neomod_datapool/bronze_age/space_and_network/regions_graph.RData")

nodes_info <- gluesless::link_ideas_world(proportion_per_region_df, regions_graph)

save(nodes_info, file = "../neomod_datapool/bronze_age/space_and_network/region_graph_nodes_info.RData")
