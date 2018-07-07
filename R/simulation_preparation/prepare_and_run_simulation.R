#### setup settings grid ####

load("../neomod_datapool/R_data/start_proportion_burial_type.RData")
load("../neomod_datapool/R_data/distance_matrix_spatial.RData")

start_proportion_5050 <- structure(
  list(
    idea_1 = c(.5, .5, .5, .5, .5, .5, .5, .5), 
    idea_2 = c(.5, .5, .5, .5, .5, .5, .5, .5)), 
  class = "data.frame", 
  row.names = c(
    "Austria and Czechia", "Poland", "Southern Germany", "Northeast France", 
    "Northern Germany", "Southern Skandinavia", "Benelux", "England"))

region_factor_levels <- c(
  "Austria and Czechia",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
)

# create models_grid data.frame
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
      factor("Austria and Czechia", levels = region_factor_levels),
      factor("Poland", levels = region_factor_levels),
      factor("Southern Germany", levels = region_factor_levels),
      factor("Northeast France", levels = region_factor_levels),
      factor("Northern Germany", levels = region_factor_levels),
      factor("Southern Skandinavia", levels = region_factor_levels),
      factor("Benelux", levels = region_factor_levels),
      factor("England", levels = region_factor_levels)
    )
  ),
  unit_size_functions = list(
    list(
      "Austria and Czechia" =  function(t) {25},
      "Poland" =               function(t) {25},
      "Southern Germany" =     function(t) {25},
      "Northeast France" =     function(t) {25},
      "Northern Germany" =     function(t) {25},
      "Southern Skandinavia" = function(t) {25},
      "Benelux" =              function(t) {25},
      "England" =              function(t) {25}
    )
  ),
  age_distribution_functions = c(
    function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}
  ),
  age_ranges = list(
    1:90
  ),
  # relations settings
  amounts_friends = list(
    5
  ),
  unit_interaction_matrix = list(
    distance_matrix_spatial
  ),
  cross_unit_proportion_child_of = list(
    0.02
  ),
  cross_unit_proportion_friend = list(
    0.20
  ),
  weight_child_of = list(
    45
  ),
  weight_friend = list(
    5
  ),
  # ideas settings
  names = list(
    c("idea_1", "idea_2")
  ),
  start_distribution = list(
    #start_proportion_burial_type.
    start_proportion_5050
  ), 
  strength = list(
    c(1, 1) 
  ),
  stringsAsFactors = FALSE
) %>% tibble::as.tibble() %>%
  dplyr::mutate(
    model_group = 1:nrow(.)
  ) %>%
  tidyr::uncount(48) %>%
  dplyr::mutate(
    model_id = 1:nrow(.)
  )

models_grid %<>% popgenerator::prepare_pops_rels_ideas()

# models_grid$population_settings[[1]] -> settings 
# models_grid$relations_settings[[1]] -> settings
# models_grid$ideas_settings[[1]] -> settings

# models_grid$populations[[1]] -> pop
# save(pop, file = "testresults/pop.RData")
# models_grid$relations[[1]] -> rel
# save(rel, file = "testresults/rel.RData")

data_path <- "../neomod_datapool/simulation_data/gluesless_input_output"

models_grid %>% popgenerator::write_all_models_to_files(dir_path = data_path)

#### test working with gluesless ####

models_grid$simulation_results <- popgenerator::run_gluesless(
  app_path = "../gluesless/build/gluesless",
  input_file_dir = data_path,
  output_file_dir = data_path,
  models_to_run = models_grid$model_id
)

models_grid %<>% popgenerator::calculate_all_idea_proportions_over_time(by_unit = TRUE)

pryr::object_size(models_grid)

save(models_grid, file = "../neomod_datapool/simulation_data/sim1.RData")

# models_grid %<>% popgenerator::calculate_all_idea_proportions_over_time(by_unit = FALSE)
# 
# save(models_grid, file = "../neomod_datapool/simulation_data/sim_general.RData")

