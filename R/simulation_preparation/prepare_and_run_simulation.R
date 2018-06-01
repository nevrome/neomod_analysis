#### setup settings grid ####

# create populations_grid data.frame
models_grid <- expand.grid(
  # general settings
  timeframe = list(
    0:1400
  ),
  # population settings  
  population_size_functions = c(
    function(t) {200}
    #function(t) {round(0.0002 * (t - 700)^2 + 10, 0)}
    # function(t) {round(0.0005 * (t - 1000)^2 + 100, 0)}
    # function(t) {round(0.0005 * (t - 1000)^2 + 100, 0)}
    # function(t) {round(2000 - 0.95 * t, 0)}
  ),
  units_amount = c(
    8
  ),
  age_distribution_functions = c(
    function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}
  ),
  age_ranges = list(
    1:100
  ),
  # relations settings
  amounts_friends = list(
    20
  ),
  unit_interaction_matrix = list(
    t(matrix(
      c(
        0,1,1,1,1,1,1,1,
        1,0,1,1,1,1,1,1,
        1,1,0,1,1,1,1,1,
        1,1,1,0,1,1,1,1,
        1,1,1,1,0,1,1,1,
        1,1,1,1,1,0,1,1,
        1,1,1,1,1,1,0,1,
        1,1,1,1,1,1,1,0
      ),
      8, 8
    ))
  ),
  cross_unit_proportion_child_of = list(
    0
  ),
  cross_unit_proportion_friend = list(
    0.2,
    0
  ),
  weight_child_of = list(
    3
  ),
  weight_friend = list(
    2
  ),
  # ideas settings
  names = list(
    c("idea_1", "idea_2")
  ),
  start_distribution = list(
    c(0.5, 0.5)
  ), 
  strength = list(
    c(1, 1) 
  )
) %>% tibble::as.tibble() %>%
  dplyr::mutate(
    multiplier = 1:nrow(.)
  ) %>%
  tidyr::uncount(1) %>%
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

models_grid %>% popgenerator::write_all_models_to_files(dir_path = "../gluesless/test_data/model_grid")

#### test working with gluesless ####

models_grid$simulation_results <- popgenerator::run_gluesless(
  app_path = "/home/clemens/neomod/gluesless/build/gluesless",
  input_file_dir = "/home/clemens/neomod/gluesless/test_data/model_grid",
  output_file_dir = "/home/clemens/neomod/gluesless/test_data/model_grid",
  models_to_run = models_grid$model_id
)

models_grid %<>% popgenerator::calculate_all_idea_proportions_over_time(by_unit = TRUE)

pryr::object_size(models_grid)

save(models_grid, file = "R/simulation_results/sim1.RData")
