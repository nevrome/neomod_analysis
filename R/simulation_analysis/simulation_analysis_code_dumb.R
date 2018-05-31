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
