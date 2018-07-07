load("../neomod_datapool/simulation_data/mantel_sed_spatial_simulation.RData")

library(ggplot2)
ggplot(mantel_test_results) +
  geom_point(
    aes(
      x = time,
      y = statistic,
      colour = model_id
    )
  ) +
  geom_boxplot(
    aes(
      x = time,
      y = statistic
    )
  )
