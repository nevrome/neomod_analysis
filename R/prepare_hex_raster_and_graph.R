library(ggplot2)
library(magrittr)
library(igraph)
library(gluesless)
library(pbapply)
library(sf)
library(readr)
library(dplyr)
library(purrr)
library(sp)
library(rgeos)



#### research area ####

# load data
area <- rgdal::readOGR(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
)

#plot(area)

research_area_border <- rgdal::readOGR(
  dsn = "../neomod_datapool/geodata/research_areas/extent.shp"
)

# clip map to research area 
research_area_highres <- rgeos::gIntersection(
  area, research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

#plot(research_area_highres)

# adjust map with buffering and simplification
crs_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

rahm <- research_area_highres %>% 
  sp::spTransform(CRS(crs_moll)) %>%
  rgeos::gBuffer(width = 15000) %>%
  rgeos::gSimplify(tol = 15000, topologyPreserve = TRUE) %>%
  sp::spTransform(CRS(crs_wgs))

#plot(rahm)

research_area <- rahm

research_area_df <- ggplot2::fortify(research_area)

save(research_area_df, file = "../neomod_datapool/model_data/research_area_df.RData")



#### research area hex ####

# create hex raster
research_area_hex <- gluesless::hexify(
  area = research_area,
  hexproj  = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
  bufferwidth = 2000,
  #hexcellsize = 75000
  hexcellsize = 70000
  )

save(research_area_hex, file = "../neomod_datapool/model_data/research_area_hex.RData")

rgdal::writeOGR(
  obj = as(research_area_hex, "SpatialPolygonsDataFrame"), 
  dsn = "../neomod_datapool/model_data/shapes",
  layer = "research_area_hex",
  driver = "ESRI Shapefile",
  overwrite_layer = TRUE
)
  
research_area_hex_df <- research_area_hex %>%
  ggplot2::fortify(region = "id")

save(research_area_hex_df, file = "../neomod_datapool/model_data/research_area_hex_df.RData")



#### construct nodes and edges ####

# get node point coordinates as the mean coordinates of the hex cell corners
nodes <- research_area_hex_df %>%
  dplyr::group_by_("id") %>%
  dplyr::summarize_("x" = ~mean(long), "y" = ~mean(lat)) %>%
  dplyr::mutate_(
    "name" = ~seq(from = 0, to = length(id) - 1, by = 1)
  ) %>%
  dplyr::select(name, x, y, id)

save(nodes, file = "../neomod_datapool/model_data/hex_graph_nodes.RData")

# transform from wgs84 to mollweide and create SpatialPointsDataFrame
nodes_spdf <- sp::SpatialPointsDataFrame(
    coords = dplyr::select_(nodes, "x", "y"), data = nodes,
    proj4string = sp::CRS(crs_wgs)
  ) %>% 
  sp::spTransform(
    sp::CRS(crs_moll)
  )

# plot(nodes_spdf@coords)
# plot(nodes_spdf, cex = 0.01)
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
    n_con = sum(distance <= 100)
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
  # remove every edge with a distance value > 250km 
  dplyr::filter(
    distance <= 250
  )

research_area_lines <- as(research_area, "SpatialLines")
# plot(research_area_lines)

# determine number of intersections (Edges & Land)
intersections <- edges_over_water %>%
  apply(1, function(x) {SpatialLines(
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
      x %>% coordinates %>% nrow
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
    distance <= 100 |
      (distance <= 250 & inter == 2)
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
          midpoints <- coordinates(x$inter_raw)
          endpoint <- data.frame(
            x = x$x.to,
            y = x$y.to
          )
          
          # combine points to data.frame  
          disi <- rbind(startpoint, midpoints, endpoint) %>% 
            # create Spatial.Points Object
            SpatialPoints(proj4string = sp::CRS(crs_wgs)) %>% 
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

save(edges, file = "../neomod_datapool/model_data/hex_graph_egdes.RData")



#### determine distance values for graph edges ####

# get research area border
research_area_border <- rgdal::readOGR(
  dsn = "../neomod_datapool/geodata/research_areas/extent.shp"
)

# load already present data
load("../neomod_datapool/model_data/research_area_hex.RData")
load("../neomod_datapool/model_data/hex_graph_egdes.RData")
load("../neomod_datapool/model_data/hex_graph_nodes.RData")

wgs <- 4326

# load and clip rivers and lake shapes
rivers <- raster::shapefile(
  "~/neomod/neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_rivers_lake_centerlines_scale_rank"
  ) %>% rgeos::gIntersection(
    ., research_area_border, byid = TRUE,
    drop_lower_td = TRUE
  )

lakes <- raster::shapefile(
  "~/neomod/neomod_datapool/geodata/rivers_lakes_shapes/ne_50m_lakes"
  ) %>% rgeos::gIntersection(
  ., research_area_border, byid = TRUE,
  drop_lower_td = TRUE
)

# intersect rivers and lakes with hexagons

rivers_hex <- rivers %>% raster::intersect(
  research_area_hex, .
) %>%
  as(., "SpatialPolygonsDataFrame")

# plot(research_area_hex)
# plot(rivers_hex, col = "red", add = T)

lakes_hex <- as(lakes, 'SpatialLines') %>% raster::intersect(
  research_area_hex, .
) %>%
  as(., "SpatialPolygonsDataFrame")

# plot(research_area_hex)
# plot(lakes_hex, col = "red", add = T)

# combine river and lake hex fields 

rivers_lakes_hex <- rbind(rivers_hex, lakes_hex)

# get hex ideas with rivers and nodes

river_lake_nodes_ids <- rivers_lakes_hex %>%
  as(., "SpatialPolygons") %>%
  ggplot2::fortify(region = "id") %$%
  unique(id)

# add info if they are part of the river-lake network to nodes

nodes_resistance <- nodes %>% 
  # calculate resistance value from start
  mutate(
    river_lakes = ifelse(.$id %in% river_lake_nodes_ids, TRUE, FALSE)
  )

# load and crop elevation raster
# from: https://topotools.cr.usgs.gov/gmted_viewer/
elevation_world <- raster::raster(
  "~/neomod/neomod_datapool/geodata/world_elevation_raster/mn30_grd/hdr.adf"
)

elevation_research_area <- raster::crop(
  elevation_world,
  research_area_border
)

elevation_research_area_small <- raster::aggregate(
  elevation_research_area, fact = 50
)

mean_elevation_per_hex <- raster::extract(
  x = elevation_research_area_small,
  y = research_area_hex,
  fun = mean,
  df = TRUE
)

diffy <- function(x, ...) {
  diff(range(x))
}

range_elevation_per_hex <- raster::extract(
  x = elevation_research_area_small,
  y = research_area_hex,
  fun = diffy,
  df = TRUE
)

elevation_per_hex <- dplyr::full_join(
  mean_elevation_per_hex, range_elevation_per_hex, by = "ID"
) %>% dplyr::rename(
  "elevation" = "hdr.x",
  "range" = "hdr.y"
)

high_hex_selection <- elevation_per_hex %>%
  dplyr::filter(
    elevation >= 1000 | range > 700
  ) %$%
  ID
  
# plot(research_area_hex)
# plot(research_area_hex[high_hex], col = "red", add = T)

high_hex <- research_area_hex[high_hex_selection]

# get hex ids with mountains

high_hex_nodes_ids <- high_hex %>%
  ggplot2::fortify(region = "id") %$%
  unique(id)

# add info if they are part of the river-lake network to nodes

nodes_resistance <- nodes_resistance %>% 
  # calculate resistance value from start
  mutate(
    mountains = ifelse(.$id %in% high_hex_nodes_ids, TRUE, FALSE)
  )


###

# add resistance to edges 
edges %<>% dplyr::mutate(
  spatial_dist = distance,
  distance = map2_dbl(from, to, function(x, y){
      rl1 <- nodes_resistance[which(x == nodes_resistance$name), ]$river_lakes
      rl2 <- nodes_resistance[which(y == nodes_resistance$name), ]$river_lakes
      m1 <- nodes_resistance[which(x == nodes_resistance$name), ]$mountains
      m2 <- nodes_resistance[which(y == nodes_resistance$name), ]$mountains
      if (rl1 && rl2) {
        res <- 1
      } else if (m1 | m2) {
        res <- 3
      } else (
        res <- 2
      )
      return(res)
    }
  )
)

save(edges, file = "../neomod_datapool/model_data/hex_graph_egdes.RData")



#### define node attributes with different proxies #### 

## C14 dates: ##

load(file = "~/neomod/neomod_datapool/C14_dates/neol_c14_dates.Rdata")

# load already present data
load("../neomod_datapool/model_data/research_area_hex.RData")
load("../neomod_datapool/model_data/hex_graph_egdes.RData")
load("../neomod_datapool/model_data/hex_graph_nodes.RData")

c14_neol_sp <- c14_neol

# make dates to geo object
sp::coordinates(c14_neol_sp) <- ~lon+lat
sp::proj4string(c14_neol_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

# plot(research_area_hex)
# plot(c14_neol_sp, add = T, col = "red")

# get dates per hex
dates_per_hex <- c14_neol_sp %>% sp::over(
  research_area_hex, ., returnList = T
)

# function: gives back oldest age that fits into defined frame
neol_dater <- function(x, start, stop) {
  dplyr::arrange(
    x, calage
  ) %>%
    filter(
      calage < start & calage > stop
    ) %>%
    magrittr::extract(nrow(.),) %$%
    calage
}

# find specific date per hex
date_per_hex <- lapply(
  dates_per_hex,
  function(x) {
    if(nrow(x) > 0) {
      neol_dater(x, 14000, 5000)
    } else {
      -1
    }
  }
  ) %>% plyr::ldply(
    ., data.frame
  ) %>% 
  dplyr::rename(
    "id" = ".id",
    "ioi" = "X..i.."
  )

# merge nodes with date_per_hex
nodes <- nodes %>% dplyr::left_join(
  ., date_per_hex, by = "id"
  ) %>% dplyr::select(
    -id
  )

save(nodes, file = "../neomod_datapool/model_data/hex_graph_nodes.RData")

# load("../neomod_datapool/model_data/hex_graph_nodes.RData")
# ggplot() + geom_point(data = nodes, aes(x, y, color = ioi))


#### create graph from nodes and edges with distance info ####  
load("../neomod_datapool/model_data/hex_graph_nodes.RData")

g <- igraph::graph_from_data_frame(
    edges,
    directed = FALSE,
    vertices = nodes
  ) %>% igraph::simplify(
    edge.attr.comb = "mean"
  )

hex_graph <- set.graph.attribute(g, "graph_name", "testgraph")

#gluesless::plot_world(hex_graph, world = research_area_df, plotedges = TRUE)

save(hex_graph, file = "../neomod_datapool/model_data/hex_graph.RData")

#####

load("../neomod_datapool/model_data/research_area_hex_df.RData")
load("../neomod_datapool/model_data/research_area_df.RData")
load("../neomod_datapool/model_data/hex_graph.RData")
gluesless::plot_world(
  hex_graph, 
  world = research_area_df, 
  #hex = research_area_hex_df,
  plotedges = T
)
