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

dates_probability_per_year_and_region_list <- bronze2 %>%
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

save(
  dates_probability_per_year_and_region_list, 
  file = "../neomod_datapool/bronze_age/dates_probability_per_year_and_region_list.RData"
)

# merge per-region data.frame list again to one dataframe
dates_probability_per_year_and_region_df <- dates_probability_per_year_and_region_list %>% 
  dplyr::bind_rows()

save(
  dates_probability_per_year_and_region_df, 
  file = "../neomod_datapool/bronze_age/dates_probability_per_year_and_region_df.RData"
)
