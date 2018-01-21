load("../neomod_datapool/bronze_age/dates_probability_per_year_and_region_df.RData")

amount_development_burial_type <- dates_probability_per_year_and_region_df %>%
  dplyr::group_by(region_name, age, burial_type) %>%
  dplyr::rename(
    timestep = age, 
    idea = burial_type
  ) %>%
  dplyr::tally()

save(
  amount_development_burial_type, 
  file = "../neomod_datapool/bronze_age/amount_development_burial_type.RData"
)

amount_development_burial_construction <- dates_probability_per_year_and_region_df %>%
  dplyr::group_by(region_name, age, burial_construction) %>%
  dplyr::rename(
    timestep = age, 
    idea = burial_construction
  ) %>%
  dplyr::tally()

save(
  amount_development_burial_type, 
  file = "../neomod_datapool/bronze_age/amount_development_burial_construction.RData"
)
