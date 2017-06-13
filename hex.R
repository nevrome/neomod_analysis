library(gluesless)

runres <- new(
  "model_builder",
  networkland_env = graphwrite(hex_graph),
  number_iterations = 30
) %>%
  run() %$%
  idea_exp

plot_world(hex_graph, world = research_area_df, hex = research_area_hex_df)
plot_world(hex_graph, world = research_area_df, plotedges = TRUE)

plot_devel(hex_graph, runres, store = TRUE, world = research_area_df, hex = research_area_hex_df)
