####

load("R/simulation_results/sim1.RData")

library(ggplot2)

idea_proportions <- dplyr::bind_rows(models_grid$idea_proportions)

idea_proportions %>% 
  dplyr::filter(idea == "idea_1") %>%
  ggplot(aes(x = timestep, y = proportion, color = as.factor(model_group), group = model_id)) +
  geom_line(alpha = 0.4, size = 0.3) +
  theme_bw() +
  #facet_wrap(~idea) +
  facet_grid(region~as.factor(model_group)) +
  stat_smooth(method = "loess", formula = y ~ x, size = 0.5, span = 0.2) +
  xlab(expression(paste("t"))) 

####

idea_proportions %>% 
  ggplot(aes(x = timestep, y = proportion, fill = idea, group = idea)) +
  geom_area() +
  geom_line(alpha = 0.4, position="stack",  color = "black") +
  theme_bw() +
  #facet_wrap(~idea) +
  facet_wrap(~model_id) +
  xlab(expression(paste("t"))) 

idea_proportions %>% 
  ggplot(aes(x = timestep, y = proportion, color = idea, group = idea)) +
  geom_line(alpha = 0.4) +
  theme_bw() +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.2) +
  facet_wrap(~model_id) +
  xlab(expression(paste("t"))) 

idea_proportions %>% 
  tidyr::spread(idea, proportion) %>%
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
