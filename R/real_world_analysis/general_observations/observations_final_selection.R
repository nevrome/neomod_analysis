load("data_analysis/dates_per_region.RData")

dpr <- dates_per_region 

# number of sites
dpr$site %>% unique %>% length()
dpr$period %>% unique %>% length()
dpr$culture %>% unique %>% length()

# distributions
dpr$material %>% table(useNA = "always")
dpr$species %>% table(useNA = "always")
dpr$burial_type %>% table()
dpr$burial_construction %>% table()
