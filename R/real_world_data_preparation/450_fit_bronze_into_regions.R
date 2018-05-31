#### load data ####

load("../neomod_datapool/bronze_age/regions.RData")
load("../neomod_datapool/bronze_age/bronze2.RData")

# transform to sf
bronze_sf <- bronze2 %>%
  sf::st_as_sf(
    coords = c("lon", "lat"), crs = 4326
  ) %>% sf::st_transform(102013)



#### dates per region ####

# intersect and get region id per entry
region_index_of_date <- bronze_sf %>% sf::st_intersects(regions) %>%
  sapply(function(z) if (length(z)==0) NA_integer_ else z[1])

dates_per_region <- bronze2 %>%
  # add region information to bronze2
  dplyr::mutate(
    region_id = regions$ID[region_index_of_date],
    region_name = as.character(regions$NAME[region_index_of_date])
  ) %>% 
  # remove entries without (outside of) regions
  dplyr::filter(
    !is.na(region_id)
  ) %>%
  # split datasets by region id
  split(.$region_id)

# merge per-region data.frame list again to one dataframe
dates_probability_per_year_and_region_df <- dates_per_region %>% 
  dplyr::bind_rows()

save(
  dates_probability_per_year_and_region_df, 
  file = "../neomod_datapool/bronze_age/dates_probability_per_year_and_region_df.RData"
)



#### calculate per year, per region distribution of ideas ####

# helper function  
fncols <- function(data, cname) {
  add <- cname[!cname %in% names(data)]
  if (length(add) != 0) {data[add] <- NA_real_}
  return(data)
}

# main loop
proportion_per_region <- dates_per_region %>%
  # apply per region data.frame
  pbapply::pblapply(function(x) {
    
    # in case of empty regions or regions with only unknown graves: NULL
    if (nrow(x) == 0 |
        (all(x$burial_type == "unknown") &
         all(x$burial_construction == "unknown"))) 
    {
      
      res <- NULL
    
    # in case of unempty regions     
    } else {
      
      #### burial_type: cremation vs. inhumation ####
      
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
      
      #### burial_type: mound vs. flat ####
      
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
      
      # combine final result 
        
      res <- dplyr::full_join(
        bt, bc, by = c("age", "region_name")
      ) %>%
        dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0)))
      
    }
    
    # complete result with 0 for years without information
    if (nrow(res) < 1401) {
      missing_ages <- c(800:2200)[!(c(800:2200) %in% res$age)]
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

# merge per region information and transform to tall data.frame
proportion_per_region_df <- proportion_per_region %>% 
  purrr::map_dfr(as.data.frame, .id = "region_id") %>%
  dplyr::mutate_("region_id" = ~as.numeric(region_id)) %>%
  dplyr::rename(
    "timestep" = "age"
  ) %>%
  tidyr::gather(
    idea, proportion, -timestep, -region_id, -region_name
  )

save(proportion_per_region_df, file = "../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")
