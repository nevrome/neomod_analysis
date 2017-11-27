# load regions
regions <- sf::st_read(
  "manually_changed_data/regionen2017g.shp"
) %>% 
  sf::st_centroid() %>%
  sf::st_transform(4326) %>%
  cbind(
    ., sf::st_coordinates(.$geometry)
  )

# get node point coordinates as the mean coordinates of the region outline
nodes <- regions %>%
  tibble::as.tibble() %>%
  dplyr::transmute(
    "id" = ID,
    "x" = X, 
    "y" = Y,
    "region_name" = as.character(NAME),
    "name" = ID
  )

# plot(nodes$x, nodes$y)
# text(nodes$x, nodes$y, nodes$name)

save(nodes, file = "../neomod_datapool/bronze_age/space_and_network/region_graph_nodes.RData")

# prepare edges
from = c()
for(i in 1:nrow(nodes)) {
  from <- append(from, rep(nodes$id[i], nrow(nodes)))
}
to <- rep(nodes$id, nrow(nodes))

edges <- tibble::tibble(
  from = from,
  to = to,
  distance = 0
)

save(edges, file = "../neomod_datapool/bronze_age/space_and_network/region_graph_egdes.RData")
