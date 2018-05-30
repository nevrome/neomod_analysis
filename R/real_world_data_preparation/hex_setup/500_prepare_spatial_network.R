load("../neomod_datapool/bronze_age/research_area/research_area_hex_df.RData")
load("../neomod_datapool/bronze_age/research_area/research_area_land.RData")
crs_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

# get node point coordinates as the mean coordinates of the hex cell corners
nodes <- research_area_hex_df %>%
  dplyr::group_by_("id") %>%
  dplyr::summarize_("x" = ~mean(long), "y" = ~mean(lat)) %>%
  dplyr::mutate_(
    "name" = ~seq(from = 0, to = length(id) - 1, by = 1)
  ) %>%
  dplyr::select(name, x, y, id)

save(nodes, file = "../neomod_datapool/bronze_age/research_area/hex_graph_nodes_no_ioi.RData")

# transform from wgs84 to mollweide and create SpatialPointsDataFrame
nodes_spdf <- sp::SpatialPointsDataFrame(
  coords = dplyr::select_(nodes, "x", "y"), data = nodes,
  proj4string = sp::CRS(crs_wgs)
) %>% 
  sp::spTransform(
    sp::CRS(crs_moll)
  )

# plot(nodes_spdf@coords)
# text(nodes_spdf, cex = 0.4)

# node distance point matrix
distmat <- rgeos::gDistance(nodes_spdf, byid = TRUE)

# approach1: select 6 closest nodes and connect them: distance value is random.
# edges <- apply(distmat, 1, function(x) {
#     order(x, decreasing = F)[c(2:7)] - 1
#   }) %>% t %>%
#   as.data.frame() %>%
#   set_names(paste0("n_", 1:6)) %>%
#   dplyr::mutate(nodes = as.numeric(row.names(.)) - 1) %>%
#   tidyr::gather(
#     ., key = schnupp, value = neighs,
#     n_1, n_2, n_3, n_4, n_5, n_6
#   ) %>%
#   dplyr::select(nodes, neighs) %>%
#   dplyr::rename(from = nodes, to = neighs) %>%
#   dplyr::mutate(distance = runif(nrow(.), 0, 100) %>% round(0))

# aproach2:  

# prepare distance table
edges_complete <- distmat %>% 
  # convert distmat to a tall, tidy format
  reshape2::melt() %>%
  tibble::as.tibble() %>%
  dplyr::rename(
    "from" = "Var1",
    "to" = "Var2",
    "distance" = "value"
  ) %>%
  dplyr::mutate(
    # index != name
    from = from - 1,
    to = to - 1,
    # transform distance from meter to kilometer
    distance = distance/1000
  ) %>%
  dplyr::arrange(from) %>%
  # remove autocorrelation
  dplyr::filter(
    distance != 0
  ) %>%
  # group by "from"
  dplyr::group_by(from) %>%
  # count number of connections per "from" node
  dplyr::mutate(
    n_con = sum(distance <= 150)
  ) %>%
  # ungroup
  dplyr::ungroup() %>%
  # join with node coordinate information
  dplyr::left_join(
    nodes, by = c("from" = "name")
  ) %>% dplyr::rename(
    "x.from" = "x",
    "y.from" = "y"
  ) %>% dplyr::left_join(
    nodes, by = c("to" = "name")
  ) %>% dplyr::rename(
    "x.to" = "x",
    "y.to" = "y"
  ) %>% dplyr::select(
    -id.x, -id.y
  )

# filter to get potential over-water connections
edges_over_water <- edges_complete %>%
  # remove connections with "from" nodes with the usual 6 or 
  # more connections
  dplyr::filter(
    n_con < 6
  ) %>%
  # remove connections with "to" partnernodes respectively
  dplyr::filter(
    to %in% unique(.$from)
  ) %>%
  # remove every edge with a distance value > 300km 
  dplyr::filter(
    distance <= 300
  )

research_area_lines <- as(research_area, "SpatialLines")
# plot(research_area_lines)

# determine number of intersections (Edges & Land)
intersections <- edges_over_water %>%
  apply(1, function(x) {sp::SpatialLines(
    cbind(c(x["x.from"], x["x.to"]), c(x["y.from"], x["y.to"])) %>%
      sp::Line() %>%
      sp::Lines(ID = "a") %>%
      list,
    proj4string = sp::CRS(crs_wgs)
  )}) %>%
  # (!) most time consuming
  pbapply::pblapply(function(x){
    rgeos::gIntersection(
      x, research_area_lines
    )
  }
  )

# get vector with number of intersections
number_of_intersections <- intersections %>%
  lapply(function(x){
    if(class(x) == "SpatialPoints") {
      x %>% sp::coordinates() %>% nrow
    } else {
      NA
    }
  }) %>% unlist

# add intersections information to connection information
edges_over_water %<>%
  dplyr::select(from, to) %>%
  dplyr::mutate(
    inter_raw = intersections,
    inter = number_of_intersections
  )

# join and filter
edges_complete_sel <- edges_complete %>%
  # join to have interaction information in complete distance table
  dplyr::left_join(
    edges_over_water, by = c("from", "to")
  ) %>%
  # filter edges_complete to fit every criteria
  dplyr::filter(
    distance <= 150 |
      (distance <= 300 & inter == 2)
  )

# check, if over sea connections are actually over land connections 
# and remove the bad ones
edges <- edges_complete_sel %>%
  dplyr::mutate(
    inter_too_much_land =
      apply(., 1, function(x){
        if (!is.na(x$inter) && x$inter == 2) {
          
          # define start-, intersection- and endpoints 
          startpoint <- data.frame(
            x = x$x.from,
            y = x$y.from
          )
          midpoints <- sp::coordinates(x$inter_raw)
          endpoint <- data.frame(
            x = x$x.to,
            y = x$y.to
          )
          
          # combine points to data.frame  
          disi <- rbind(startpoint, midpoints, endpoint) %>% 
            # create Spatial.Points Object
            sp::SpatialPoints(proj4string = sp::CRS(crs_wgs)) %>% 
            # transform coordinates
            sp::spTransform(sp::CRS(crs_moll)) %>%
            # calculate 
            rgeos::gDistance(byid = TRUE)
          
          # check if distance over land is bigger than 
          # distance over sea
          return(disi[1,2] + disi[3,4] > disi[2,3])
        } else {
          return(FALSE)
        }
      }
      ) %>% unlist
  ) %>% 
  # remove the bad ones (where inter_too_much_land == TRUE) 
  dplyr::filter(
    !inter_too_much_land
  )

save(edges, file = "../neomod_datapool/bronze_age/research_area/hex_graph_egdes.RData")
