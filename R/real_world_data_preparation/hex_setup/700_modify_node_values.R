## C14 dates: ##
load(file = "~/neomod/neomod_datapool/C14_dates/neol_c14_dates.Rdata")

# load already present data
load("../neomod_datapool/bronze_age/research_area/research_area_hex.RData")
load("../neomod_datapool/bronze_age/research_area/hex_graph_egdes.RData")
load("../neomod_datapool/bronze_age/research_area/hex_graph_nodes_no_ioi.RData")

c14_neol_sp <- c14_neol %>% as.data.frame()

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
    dplyr::filter(
      calage < start & calage > stop
    ) %>%
    magrittr::extract(nrow(.),) %$%
    calage
}

# find specific date per hex
date_per_hex <- dates_per_hex %>% lapply(
  function(x) {
    if(nrow(x) > 0) {
      neol_dater(x, 2500 + 1950, 500 + 1950)
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

save(nodes, file = "../neomod_datapool/bronze_age/research_area/hex_graph_nodes.RData")

# load("../neomod_datapool/model_data/hex_graph_nodes.RData")
# library(ggplot2)
# ggplot() + geom_point(data = nodes, aes(x, y, color = ioi))
