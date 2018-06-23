load("../popgenerator/testresults/pop.RData")
load("../popgenerator/testresults/rel.RData")

rel2 <- (rel %>% dplyr::filter(from <= max(pop$id), to <= max(pop$id)))

g <- igraph::graph_from_data_frame(
  rel2,
  directed = FALSE,
  vertices = (pop %>% dplyr::select(id, unit))
)

actual_vertices <- unique(c(rel2$from, rel2$to))
write.table(
  actual_vertices, 
  file = "../gluesless/test_data/vert.txt", 
  row.names = FALSE, col.names = FALSE
)

igraph::write_graph(g, file = "../gluesless/test_data/test.paj", format = "pajek")

#hu <- graphwrite(g)

modelobj <- new(
  "model_builder",
  networkland_env = "data_raw/test.graphml",
  number_iterations = 2001,
  ideas_list = c("a", "b"),
  ideas_proportions_matrix = matrix(c(1,2,3,4),2)
)

run(modelobj)

