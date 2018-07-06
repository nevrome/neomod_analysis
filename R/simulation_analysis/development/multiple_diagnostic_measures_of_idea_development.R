load("R/simulation_results/sim1_general.RData")

idea_proportions <- dplyr::bind_rows(models_grid$idea_proportions)

moving_average <- function(x, n = 5, sides = 1) {
  as.vector(stats::filter(x, rep(1/n, n), sides = sides))
}

huup <- idea_proportions %>%
  dplyr::filter(idea == "idea_1") %>%
  dplyr::group_by(model_id) %>%
  dplyr::mutate(
    proportion = moving_average(proportion, n = 50, sides = 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(timestep) %>%
  dplyr::mutate(
    general_sd = sd(proportion)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(timestep, model_group) %>%
  dplyr::summarise(
    min = min(proportion),
    max = max(proportion),
    mean = mean(proportion),
    standard_deviation = sd(proportion),
    range = abs(min - max),
    lower_quart = quantile(proportion, na.rm = TRUE)[2],
    upper_quart = quantile(proportion, na.rm = TRUE)[4],
    inter_quart_dist = abs(upper_quart - lower_quart),
    general_sd = mean(general_sd)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    diff_standard_deviation = standard_deviation - general_sd
  )

library(ggplot2)

huup %>%
  ggplot() +
  geom_ribbon(aes(x = timestep, ymin = min, ymax = max)) +
  geom_line(aes(x = timestep, y = mean)) +
  geom_line(aes(x = timestep, y = mean + standard_deviation), color = "red") +
  geom_line(aes(x = timestep, y = mean - standard_deviation), color = "red") +
  geom_line(aes(x = timestep, y = lower_quart), color = "darkgreen") +
  geom_line(aes(x = timestep, y = upper_quart), color = "darkgreen") +
  facet_wrap(~model_group)

huup %>%
  ggplot() +
  geom_line(aes(x = timestep, y = range, color = as.factor(model_group), group = as.factor(model_group)))

huup %>%
  ggplot() +
  geom_line(aes(x = timestep, y = inter_quart_dist, color = as.factor(model_group), group = as.factor(model_group)))

huup %>%
  ggplot() +
  geom_line(aes(x = timestep, y = standard_deviation, color = as.factor(model_group), group = as.factor(model_group)))

huup %>%
  ggplot() +
  geom_line(aes(x = timestep, y = diff_standard_deviation, color = as.factor(model_group), group = as.factor(model_group)))

#####

load("R/simulation_results/sim1.RData")

idea_proportions <- dplyr::bind_rows(models_grid$idea_proportions)

huup <- idea_proportions %>%
  dplyr::filter(idea == "idea_1") %>%
  # dplyr::group_by(model_id) %>%
  # dplyr::mutate(
  #   proportion = moving_average(proportion, n = 50, sides = 2)
  # ) %>%
  # dplyr::ungroup() %>%
  dplyr::group_by(model_group, region, timestep) %>%
  dplyr::mutate(
    general_sd = sd(proportion)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(model_group, timestep) %>%
  dplyr::summarise(
    standard_deviation = mean(general_sd)
  ) %>%
  dplyr::ungroup()

huup %>%
  ggplot() +
  geom_line(
    aes(
      x = timestep, 
      y = standard_deviation, 
      color = as.factor(model_group)
    )
  )







