storage_path <- "data_analysis/desc_storage.txt"

# bronze
load("data_analysis/bronze.RData")
txtstorage::store("size bronze", nrow(bronze), storage_path)
rm(bronze)

# bronze 0
load("data_analysis/bronze0.RData")
txtstorage::store("size bronze0", nrow(bronze0), storage_path)
rm(bronze0)

###

load("data_analysis/dates_per_region.RData")

# how many dates
dates_per_region %>% nrow()

# how many labnrs
dates_per_region$labnr %>% unique %>% length

# how many labnrs are doubles
dates_per_region %>% 
  dplyr::filter(
    !grepl('n/a', labnr)
  ) %>%
  dplyr::group_by(labnr) %>% 
  dplyr::filter(n()>1)

# how many graves with multiple dates
dates_per_region %>%
  dplyr::group_by(site, feature) %>% 
  dplyr::filter(n()>1) 

# distribution of these by burial_type and burial_construction
dates_per_region %>%
  dplyr::group_by(site, feature) %>% 
  dplyr::filter(n()>1) %$%
  #  burial_type %>% table()
  burial_construction %>% table()

# get some counts
bronze1 %>%
  dplyr::group_by(
    burial_type
  ) %>%
  dplyr::summarise(
    n()
  )

bronze1 %>%
  dplyr::group_by(
    burial_construction
  ) %>%
  dplyr::summarise(
    n()
  )

bronze1 %>%
  dplyr::filter(
    burial_type != "unknown",
    burial_construction != "unknown"
  ) %>%
  dplyr::group_by(
    burial_type, burial_construction
  ) %>%
  dplyr::summarise(
    n = n()
  ) %$%
  sum(n)
