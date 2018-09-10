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
      "1" = function(t) {1},
      "2" = function(t) {1},
      "3" = function(t) {1},
      "4" = function(t) {1},
      "5" = function(t) {1},
      "6" = function(t) {1},
      "7" = function(t) {1},
      "8" = function(t) {1}
    ),
    list(
      "1" = function(t) {5},
      "2" = function(t) {5},
      "3" = function(t) {5},
      "4" = function(t) {5},
      "5" = function(t) {5},
      "6" = function(t) {5},
      "7" = function(t) {5},
      "8" = function(t) {5}
    ),
    list(
      "1" = function(t) {30},
      "2" = function(t) {30},
      "3" = function(t) {30},
      "4" = function(t) {30},
      "5" = function(t) {30},
      "6" = function(t) {30},
      "7" = function(t) {30},
      "8" = function(t) {30}
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
    30
  ),
  unit_interaction_matrix = list(
    distance_matrix_equal
  ),
  cross_unit_proportion_child_of = c(
    0.0001, 0.001, 0.01
  ),
  cross_unit_proportion_friend = c(
    0.01, 0.01, 0.1 
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
    10 * cross_unit_proportion_child_of == cross_unit_proportion_friend
  ) %>%
  # add relevant model ids
  dplyr::mutate(
    model_group = 1:nrow(.)
  ) %>%
  tidyr::uncount(5) %>%
  dplyr::mutate(
    model_id = 1:nrow(.)
  )

#### run simulation ####

data_path <- "data_simulation/gluesless_input_output"
popgenerator::run_simulation(models_grid, data_path)

