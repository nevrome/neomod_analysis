crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

# load regions
regions_df <- rgdal::readOGR(
  dsn = "manually_changed_data/regionen2017g.shp"
) %>%
  sp::spTransform(sp::CRS(crs_wgs)) %>%
  ggplot2::fortify()

# get node point coordinates as the mean coordinates of the region outline
nodes <- regions_df %>%
  dplyr::group_by_("id") %>%
  dplyr::summarize_("x" = ~mean(long), "y" = ~mean(lat)) %>%
  dplyr::mutate_(
    "name" = ~seq(from = 0, to = length(id) - 1, by = 1)
  ) %>%
  dplyr::select(name, x, y, id)

save(nodes, file = "../neomod_datapool/bronze_age/space_and_network/region_graph_nodes_no_values.RData")

from = c()
for(i in 1:nrow(nodes)) {
  from <- append(from, rep(nodes$id[i], nrow(nodes)))
}
to <- rep(nodes$id, nrow(nodes))

edges <- tibble::tibble(
  from = from,
  to = to,
  distance = runif(nrow(nodes)^2)
)

save(edges, file = "../neomod_datapool/bronze_age/space_and_network/region_graph_egdes_random_distance.RData")
