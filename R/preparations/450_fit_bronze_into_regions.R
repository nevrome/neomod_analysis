# load regions
regions <- sf::st_read(
  "manually_changed_data/regionen2017g.shp"
) %>% 
  sf::st_transform(4326)

load("../neomod_datapool/bronze_age/bronze2.RData")
bronze_sf <- bronze2 %>%
  sf::st_as_sf(
    coords = c("lon", "lat"), crs = 4326
  )

#bronze_sf2 <- bronze_sf[1:200,]

region_index_of_date <- bronze_sf %>% sf::st_intersects(regions) %>%
  sapply(function(z) if (length(z)==0) NA_integer_ else z[1])

region_of_date <- regions$ID[region_index_of_date]
region_of_date_name <- as.character(regions$NAME[region_index_of_date])

dates_per_region <- bronze2 %>%
  dplyr::mutate(
    region_id = region_of_date,
    region_name = region_of_date_name
  ) %>% 
  dplyr::filter(
    !is.na(region_id)
  ) %>%
  plyr::dlply("region_id")

dates_probability_per_year_and_region_df <- dates_per_region %>% 
  as.list() %>%
  dplyr::bind_rows()

save(
  dates_probability_per_year_and_region_df, 
  file = "../neomod_datapool/bronze_age/dates_probability_per_year_and_region_df.RData"
)
  
fncols <- function(data, cname) {
  add <- cname[!cname %in% names(data)]
  if (length(add) != 0) {data[add] <- NA_real_}
  return(data)
}

proportion_per_region <- dates_per_region %>%
  pbapply::pblapply(function(x) {
    # remove empty regions
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
          region_name = character(),
          age = integer(),
          cremation = double(), 
          inhumation = double()
        )
      } else {
        bt <- bt_basic %>%
          dplyr::group_by(age, burial_type) %>%
          dplyr::summarise(
            count = n(), region_name = .$region_name[1]
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
          region_name = character(),
          age = integer(),
          mound = double(), 
          flat = double()
        )
      } else {
        bc <- bc_basic %>%
          dplyr::group_by(age, burial_construction) %>%
          dplyr::summarise(
            count = n(), region_name = .$region_name[1]
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
        bt, bc, by = c("age", "region_name")
      ) %>%
        dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0)))
      
    }
    
    # completion with empty information
    if (nrow(res) < 2001) {
      missing_ages <- c(500:2500)[!(c(500:2500) %in% res$age)]
      res <- rbind(
        res, 
        tibble::tibble(
          region_name = res$region_name[1],
          age = missing_ages,
          cremation = 0, 
          inhumation = 0,
          mound = 0,
          flat = 0
        )
      )
    }
    
    return(res)
  })

proportion_per_region_df <- proportion_per_region %>% 
  purrr::map_dfr(as.data.frame, .id = "node") %>%
  dplyr::mutate_("node" = ~as.numeric(node)) %>%
  dplyr::rename(
    "timestep" = "age"
  ) %>%
  tidyr::gather(
    idea, proportion, -timestep, -node, -region_name
  )

save(proportion_per_region_df, file = "../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")
