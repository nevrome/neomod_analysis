#### load data ####

load("data_analysis/regions.RData")
load("data_analysis/bronze2.RData")
load("data_analysis/region_order.RData")

# transform to sf
bronze_sf <- bronze2 %>%
  sf::st_as_sf(
    coords = c("lon", "lat"), crs = 4326
  ) %>% sf::st_transform("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")



#### dates per region ####

# intersect and get region id per entry
region_index_of_date <- bronze_sf %>% sf::st_intersects(regions) %>%
  sapply(function(z) if (length(z)==0) NA_integer_ else z[1])

dates_probability_per_year_and_region_list <- bronze2 %>%
  # add region information to bronze2
  dplyr::mutate(
    region_name = factor(regions$NAME[region_index_of_date], levels = region_order)
  ) %>% 
  # remove entries without (outside of) regions
  dplyr::filter(
    !is.na(region_name)
  ) %>%
  # split datasets by region name
  split(.$region_name)

save(
  dates_probability_per_year_and_region_list, 
  file = "data_analysis/dates_probability_per_year_and_region_list.RData"
)

# merge per-region data.frame list again to one dataframe
dates_probability_per_year_and_region_df <- dates_probability_per_year_and_region_list %>% 
  dplyr::bind_rows()

save(
  dates_probability_per_year_and_region_df, 
  file = "data_analysis/dates_probability_per_year_and_region_df.RData"
)
