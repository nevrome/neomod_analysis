load("R/simulation_results/sim1.RData")

idea_proportions <- dplyr::bind_rows(models_grid$idea_proportions)

huup <- idea_proportions %>%
  dplyr::filter(variant == "idea_1") %>%
  dplyr::group_by(model_id) %>%
  dplyr::mutate(
    individuals_with_variant = moving_average(individuals_with_variant, n = 50, sides = 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(timesteps) %>%
  dplyr::mutate(
    general_sd = sd(individuals_with_variant)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(timesteps, multiplier) %>%
  dplyr::summarise(
    min = min(individuals_with_variant),
    max = max(individuals_with_variant),
    mean = mean(individuals_with_variant),
    standard_deviation = sd(individuals_with_variant),
    range = abs(min - max),
    lower_quart = quantile(individuals_with_variant, na.rm = TRUE)[2],
    upper_quart = quantile(individuals_with_variant, na.rm = TRUE)[4],
    inter_quart_dist = abs(upper_quart - lower_quart),
    general_sd = mean(general_sd)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    diff_standard_deviation = standard_deviation - general_sd
  )

huup %>%
  ggplot() +
  geom_ribbon(aes(x = timesteps, ymin = min, ymax = max)) +
  geom_line(aes(x = timesteps, y = mean)) +
  geom_line(aes(x = timesteps, y = mean + standard_deviation), color = "red") +
  geom_line(aes(x = timesteps, y = mean - standard_deviation), color = "red") +
  geom_line(aes(x = timesteps, y = lower_quart), color = "darkgreen") +
  geom_line(aes(x = timesteps, y = upper_quart), color = "darkgreen") +
  facet_wrap(~multiplier)

huup %>%
  ggplot() +
  geom_line(aes(x = timesteps, y = range, color = as.factor(multiplier), group = as.factor(multiplier)))

huup %>%
  ggplot() +
  geom_line(aes(x = timesteps, y = inter_quart_dist, color = as.factor(multiplier), group = as.factor(multiplier)))

huup %>%
  ggplot() +
  geom_line(aes(x = timesteps, y = standard_deviation, color = as.factor(multiplier), group = as.factor(multiplier)))

huup %>%
  ggplot() +
  geom_line(aes(x = timesteps, y = diff_standard_deviation, color = as.factor(multiplier), group = as.factor(multiplier)))
