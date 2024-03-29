library(magrittr)
load("data_analysis/region_order.RData")

#### prepare data ####

# starting positions
load("data_analysis/start_proportion_burial_type.RData")
load("data_analysis/start_proportion_burial_construction.RData")
start_proportion_5050 <- structure(
  list(
    idea_1 = c(.5, .5, .5, .5, .5, .5, .5, .5), 
    idea_2 = c(.5, .5, .5, .5, .5, .5, .5, .5)), 
  class = "data.frame", 
  row.names = region_order)

# distance matrizes
load("data_analysis/distance_matrix_spatial.RData")
load("data_analysis/distance_matrix_burial_type.RData")
load("data_analysis/distance_matrix_burial_construction.RData")
distance_matrix_equal <- distance_matrix_spatial %>% `[<-`(1) %>% `diag<-`(0)

#### setup settings grid ####

models_grid <- expand.grid(
  # general settings
  timeframe = list(
    -2200:-800
  ),
  # population settings  
  unit_amount = c(
    8
  ),
  unit_names = list(
    list(
      factor("Southeastern Central Europe", levels = region_order),
      factor("Poland", levels = region_order),
      factor("Southern Germany", levels = region_order),
      factor("Northeastern France", levels = region_order),
      factor("Northern Germany", levels = region_order),
      factor("Southern Scandinavia", levels = region_order),
      factor("Benelux", levels = region_order),
      factor("England", levels = region_order)
    )
  ),
  unit_size_functions = list(
    list(
      "1" = function(t) {50},
      "2" = function(t) {50},
      "3" = function(t) {50},
      "4" = function(t) {50},
      "5" = function(t) {50},
      "6" = function(t) {50},
      "7" = function(t) {50},
      "8" = function(t) {50}
    )
  ),
  age_distribution_functions = c(
    function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}
  ),
  age_ranges = list(
    1:90
  ),
  # relations settings
  amounts_friends = c(
    10
  ),
  unit_interaction_matrix = list(
    distance_matrix_equal,
    distance_matrix_spatial
  ),
  cross_unit_proportion_child_of = c(
    0.002, 0.02
  ),
  cross_unit_proportion_friend = c(
    0.01, 0.1
  ),
  weight_child_of = list(
    50
  ),
  weight_friend = list(
    10
  ),
  # ideas settings
  names = list(
    c("idea_1", "idea_2")
  ),
  start_distribution = list(
    start_proportion_5050
  ), 
  strength = list(
    c(1, 1) 
  ),
  stringsAsFactors = FALSE
) %>% tibble::as.tibble() %>%
  # remove unnecessary repetition
  dplyr::filter(
    5 * cross_unit_proportion_child_of == cross_unit_proportion_friend
  ) %>%
  # add relevant model ids
  dplyr::mutate(
    model_group = c(
      "low equal interaction",
      "low spatial interaction",
      "high equal interaction",
      "high spatial interaction"
    )
  ) %>%
  tidyr::uncount(100) %>%
  dplyr::mutate(
    model_id = 1:nrow(.)
  )

save(models_grid, file = "data_simulation/sed_simulation_model_grid.RData")

#### run simulation ####

#data_path <- "data_simulation/sed_simulation"
data_path <- "../simulationdata/sed_simulation"
popgenerator::run_simulation(models_grid, data_path)

