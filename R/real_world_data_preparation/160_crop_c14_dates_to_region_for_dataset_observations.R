#### load spatial data ####

load("data_analysis/regions.RData")
load("data_analysis/bronze16.RData")

#### prepare data ####

load("data_analysis/bronze16.RData")
bronze16 %<>% sf::st_as_sf(coords = c("lon", "lat"))
sf::st_crs(bronze16) <- 4326
bronze16 %<>% sf::st_transform("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

#### intersect ####

schnu <- sf::st_intersection(bronze16, regions)

#### make tibble ####

dates_per_region <- schnu %>% sf::st_set_geometry(NULL) %>%
  dplyr::mutate(
    region = NAME  
  ) %>%
  dplyr::select(
    -id.1, -ID, -NAME
  )

save(
  dates_per_region,
  file = "data_analysis/dates_per_region.RData"
)
