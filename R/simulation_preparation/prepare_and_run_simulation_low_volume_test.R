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
      "Southeastern Central Europe" = function(t) {10},
      "Poland" =                      function(t) {10},
      "Southern Germany" =            function(t) {10},
      "Northeastern France" =         function(t) {10},
      "Northern Germany" =            function(t) {10},
      "Southern Scandinavia" =        function(t) {10},
      "Benelux" =                     function(t) {10},
      "England" =                     function(t) {10}
    ),
    list(
      "Southeastern Central Europe" = function(t) {20},
      "Poland" =                      function(t) {20},
      "Southern Germany" =            function(t) {20},
      "Northeastern France" =         function(t) {20},
      "Northern Germany" =            function(t) {20},
      "Southern Scandinavia" =        function(t) {20},
      "Benelux" =                     function(t) {20},
      "England" =                     function(t) {20}
    ),
    list(
      "Southeastern Central Europe" = function(t) {30},
      "Poland" =                      function(t) {30},
      "Southern Germany" =            function(t) {30},
      "Northeastern France" =         function(t) {30},
      "Northern Germany" =            function(t) {30},
      "Southern Scandinavia" =        function(t) {30},
      "Benelux" =                     function(t) {30},
      "England" =                     function(t) {30}
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
    distance_matrix_equal,
    distance_matrix_spatial,
    distance_matrix_burial_type,
    distance_matrix_burial_construction
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
    start_proportion_5050,
    start_proportion_burial_type,
    start_proportion_burial_construction
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
  tidyr::uncount(10) %>%
  dplyr::mutate(
    model_id = 1:nrow(.)
  )

#### run simulation ####

data_path <- "data_simulation/gluesless_input_output"
popgenerator::run_simulation(models_grid[1:8,], data_path)

