load("../neomod_datapool/bronze_age/research_area/research_area_hex.RData")

crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

load("../neomod_datapool/bronze_age/prop_nodes.RData")
bronze_sp <- prop_nodes %>% as.data.frame() %>%
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = sp::CRS(crs_wgs)
  )

dates_per_hex <- bronze_sp %>% sp::over(
  research_area_hex, ., returnList = T
)

fncols <- function(data, cname) {
  add <- cname[!cname %in% names(data)]
  if (length(add) != 0) {data[add] <- NA_real_}
  return(data)
}

proportion_per_hex <- dates_per_hex %>%
  pbapply::pblapply(function(x) {
    if (nrow(x) == 0 | 
        (all(x$burial_type == "unknown") & 
         all(x$burial_construction == "unknown"))) {
      res <- NULL
    } else {
      bt_basic <- x %>% 
        dplyr::filter(
          burial_type != "unknown"
        ) 
      if (nrow(bt_basic) == 0) {
        bt <- tibble::tibble(
          age = double(),
          cremation = double(), 
          inhumation = double()
        )
      } else {
        bt <- bt_basic %>%
          dplyr::group_by(age, burial_type) %>%
          dplyr::summarise(
            count = n()
          ) %>% 
          tidyr::spread(
            key = burial_type, value = count
          ) %>%
          fncols(c("cremation", "inhumation")) %>%
          dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0))) %>%
          dplyr::mutate(
            cremation = cremation/(cremation + inhumation)
          ) %>%
          dplyr::mutate(
            inhumation = 1 - cremation
          ) %>%
          dplyr::ungroup()
      }
      
        bc_basic <- x %>% 
          dplyr::filter(
            burial_construction != "unknown"
          ) 
        if (nrow(bc_basic) == 0) {
          bc <- tibble::tibble(
            age = double(),
            mound = double(), 
            flat = double()
          )
        } else {
          bc <- bc_basic %>%
            dplyr::group_by(age, burial_construction) %>%
            dplyr::summarise(
              count = n()
            ) %>% 
            tidyr::spread(
              key = burial_construction, value = count
            ) %>%
            fncols(c("mound", "flat")) %>%
            dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0))) %>%
            dplyr::mutate(
              mound = mound/(mound + flat)
            ) %>%
            dplyr::mutate(
              flat = 1 - mound
            ) %>%
            dplyr::ungroup()
        }
        
      res <- dplyr::full_join(
        bt, bc, by = "age"
      ) %>%
        dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0)))
    }
    return(res)
  })

proportion_per_hex_df <- proportion_per_hex %>% 
  purrr::map_dfr(as.data.frame, .id = "id")

load("../neomod_datapool/bronze_age/research_area/hex_graph_nodes_no_ioi.RData")

prop_nodes <- proportion_per_hex_df %>%
  dplyr::left_join(nodes, by = "id")

#### shitty plot ####

library(ggplot2)
area <- rgdal::readOGR(
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_110m_land.shp"
  dsn = "../neomod_datapool/geodata/land_shapes/ne_50m_land.shp"
  #dsn = "../neomod_datapool/geodata/land_shapes/ne_10m_land.shp"
) %>% ggplot2::fortify()

prop_nodes_slices <- prop_nodes %>%
  dplyr::filter(
    age %in% seq(2500, 500, by = -250)
  ) %>%
  dplyr::mutate(
    age_slice = factor(age, levels = seq(2500, 500, by = -250))
  )

hu <- ggplot() +
  geom_polygon(
    data = area,
    aes_string(
      x = "long", y = "lat",
      group = "group"
    ),
    fill = NA, colour = "black",
    size = 0.1
  ) +
  geom_point(
    data = prop_nodes_slices, 
    aes(
      x = x, y = y, 
      color = mound, 
      size = inhumation
    )
  ) +
  theme_bw() +
  coord_map(
    "ortho", orientation = c(48, 13, 0),
    xlim = c(-10, 30), ylim = c(35, 65)
  ) + 
  facet_wrap(
    nrow = 2,
    ~age_slice
  ) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_line(colour = "lightgrey", size = 0.1),
    # legend.position = c(1, 0), legend.justification = c(1, 0)
    legend.position="bottom"
  )
