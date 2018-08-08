storage_file <- "data_text_elements/sf_prep.txt"

#### bronze ####
load("data_analysis/bronze.RData")
txtstorage::store("size bronze", nrow(bronze), storage_file)
rm(bronze)

#### bronze0 ####
load("data_analysis/bronze0.RData")
txtstorage::store("size bronze0", nrow(bronze0), storage_file)
rm(bronze0)

#### bronze05 ####
load("data_analysis/bronze05.RData")
txtstorage::store("size bronze05", nrow(bronze05), storage_file)
txtstorage::store("bronze1 variable amount", ncol(bronze05), storage_file)
rm(bronze05)

#### bronze1 ####
load("data_analysis/bronze1.RData")
txtstorage::store("size bronze1", nrow(bronze1), storage_file)
txtstorage::store("bronze1 variable amount", ncol(bronze1), storage_file)
rm(bronze1)
 
#### bronze15 ####
load("data_analysis/bronze15.RData")

# size
txtstorage::store("size bronze15", nrow(bronze15), storage_file)

# count indiviual labnrs
labnrs_amount <- bronze15$labnr %>% unique() %>% length()
txtstorage::store("bronze15 labnrs amount", labnrs_amount, storage_file)

# count labnr duplicates without n/a labnrs 
labnr_doubles <- bronze15[!grepl('n/a', bronze15$labnr), ] %>% 
  dplyr::group_by(labnr) %>% 
  dplyr::filter(n() > 1) %>%
  nrow()
txtstorage::store("bronze15 labnr doubles", labnr_doubles, storage_file)

# count graves represented by multiple c14 dates 
multi_dates_one_grave <- bronze15 %>% 
  dplyr::group_by(site, feature) %>% 
  dplyr::filter(n() > 1) %>%
  nrow()
txtstorage::store("bronze15 multi dates one grave", multi_dates_one_grave, storage_file)

bronze15_burial_type_doubles <- bronze15 %>% 
  dplyr::group_by(site, feature) %>% 
  dplyr::filter(n() > 1) %>%
  dplyr::ungroup() %$%
  burial_type %>% table %>%
  unclass %>%
  paste(names(.), ., collapse = ", ", sep = ": ")
txtstorage::store("bronze15 burial_type doubles", bronze15_burial_type_doubles, storage_file)

bronze15_burial_construction_doubles <- bronze15 %>% 
  dplyr::group_by(site, feature) %>% 
  dplyr::filter(n() > 1) %>%
  dplyr::ungroup() %$%
  burial_construction %>% table %>% 
  unclass %>%
  paste(names(.), ., collapse = ", ", sep = ": ")
txtstorage::store("bronze15 burial_construction doubles", bronze15_burial_construction_doubles, storage_file)

rm(bronze15)

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
