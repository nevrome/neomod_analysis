load("data_analysis/dates_per_region.RData")

dpr <- dates_per_region 

# number of sites
dpr$site %>% unique %>% length()

# distribution of materiels
dpr$material %>% table(useNA = "always")
