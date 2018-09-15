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
    1:1400
  ),
  # population settings  
  unit_amount = c(
    8
  ),
  unit_names = list(
    list(
      factor("1", as.character(c(1,2,3,4,5,6,7,8))),
      factor("2", as.character(c(1,2,3,4,5,6,7,8))),
      factor("3", as.character(c(1,2,3,4,5,6,7,8))),
      factor("4", as.character(c(1,2,3,4,5,6,7,8))),
      factor("5", as.character(c(1,2,3,4,5,6,7,8))),
      factor("6", as.character(c(1,2,3,4,5,6,7,8))),
      factor("7", as.character(c(1,2,3,4,5,6,7,8))),
      factor("8", as.character(c(1,2,3,4,5,6,7,8)))
    )
  ),
  unit_size_functions = list(
    list(
      "1" = function(t) {10},
      "2" = function(t) {10},
      "3" = function(t) {10},
      "4" = function(t) {10},
      "5" = function(t) {10},
      "6" = function(t) {10},
      "7" = function(t) {10},
      "8" = function(t) {10}
    ),
    list(
      "1" = function(t) {50},
      "2" = function(t) {50},
      "3" = function(t) {50},
      "4" = function(t) {50},
      "5" = function(t) {50},
      "6" = function(t) {50},
      "7" = function(t) {50},
      "8" = function(t) {50}
    ),
    list(
      "1" = function(t) {200},
      "2" = function(t) {200},
      "3" = function(t) {200},
      "4" = function(t) {200},
      "5" = function(t) {200},
      "6" = function(t) {200},
      "7" = function(t) {200},
      "8" = function(t) {200}
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
    distance_matrix_equal
  ),
  cross_unit_proportion_child_of = c(
    0, 0.01, 0.1, 0.5, 1
  ),
  cross_unit_proportion_friend = c(
    0, 0.01, 0.1, 0.5, 1
  ),
  weight_child_of = list(
    50
  ),
  weight_friend = list(
    5
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
    cross_unit_proportion_child_of == cross_unit_proportion_friend
  ) %>%
  # add relevant model ids
  dplyr::mutate(
    model_group = 1:nrow(.)
  ) %>%
  tidyr::uncount(8) %>%
  dplyr::mutate(
    model_id = 1:nrow(.)
  )

save(models_grid, file = "data_simulation/pe_popsize_crossregions.RData")

#### run simulation ####

data_path <- "../simulationdata/pe_popsize_crossregions"
popgenerator::run_simulation(models_grid, data_path)

