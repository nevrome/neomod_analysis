####



library(ggplot2)

idea_proportions <- dplyr::bind_rows(models_grid$idea_proportions)

idea_proportions %>% 
  ggplot(aes(x = timesteps, y = individuals_with_variant, color = as.factor(multiplier), group = model_id)) +
  geom_line(alpha = 0.4) +
  theme_bw() +
  #facet_wrap(~variant) +
  facet_wrap(as.factor(multiplier)~variant) +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.2) +
  xlab(expression(paste("t"))) 

idea_proportions %>% 
  ggplot(aes(x = timesteps, y = individuals_with_variant, fill = variant, group = variant)) +
  geom_area() +
  geom_line(alpha = 0.4, position="stack",  color = "black") +
  theme_bw() +
  #facet_wrap(~variant) +
  facet_wrap(~model_id) +
  xlab(expression(paste("t"))) 

idea_proportions %>% 
  ggplot(aes(x = timesteps, y = individuals_with_variant, color = variant, group = variant)) +
  geom_line(alpha = 0.4) +
  theme_bw() +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.2) +
  facet_wrap(~model_id) +
  xlab(expression(paste("t"))) 

idea_proportions %>% 
  tidyr::spread(variant, individuals_with_variant) %>%
  ggplot(aes(x = idea_1, y = idea_2, z = not_involved)) +
  geom_point(alpha = 0.4) +
  theme_bw() +
  #stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.2) +
  facet_wrap(~model_id) +
  ggtern::coord_tern()

#### 

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

#### analyse result ####

population_development_plot <- plot_population_development(pop)
relations_development_plot <- plot_relations_development(pop, rel)

library(cowplot)
cowplot::plot_grid(
  population_development_plot,
  relations_development_plot,
  align = "v",
  nrow = 2,
  labels = "AUTO"
)

# hu %>%
#   ggplot() +
#     geom_area(aes(x = time, y = n, fill = unit, group = unit)) +
#     geom_line(aes(x = time, y = n, group = unit), position = "stack") +
#     theme_bw()
