load("R/simulation_results/sim1.RData")

# select individual proportions development
models_grid$idea_proportions[[8]] -> test_prop

moving_average <- function(x, n = 5, sides = 1) {
  as.vector(stats::filter(x, rep(1/n, n), sides = sides))
}

test_prop_smooth <- test_prop %>% 
  dplyr::group_by(variant) %>%
  dplyr::mutate(
    individuals_with_variant = moving_average(individuals_with_variant, n = 50, sides = 2)
  )

ggplot() +
  geom_line(
    data = test_prop,
    mapping = aes(x = timesteps, y = individuals_with_variant, color = variant, group = variant), 
    alpha = 0.4
  ) +
  geom_line(
    data = test_prop_smooth,
    mapping = aes(x = timesteps, y = individuals_with_variant, color = variant, group = variant), 
    size = 1
  ) +
  theme_bw() +
  facet_wrap(~variant)
  xlab(expression(paste("t"))) 
