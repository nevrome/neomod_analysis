load("../neomod_datapool/simulation_data/mantel_sed_spatial_simulation.RData")

library(ggplot2)
ggplot(mantel_test_results) +
  geom_hline(
    yintercept = 0,
    colour = "red",
    size = 2
  ) + 
  geom_boxplot(
    aes(
      x = time,
      y = statistic
    ),
    width = 0.4
  ) +
  geom_dotplot(
    aes(
      x = time,
      y = statistic,
      fill = base::cut(signif, breaks = c(0, 0.05, 0.1, seq(0.2, 1, 0.1)), labels = c("1", "2", rep("3", 9)))
    ),
    binaxis = "y",
    stackdir = "down",
    position = position_nudge(x = -0.3),
    dotsize = 0.3,
    binpositions = "all"
  ) +
  ylim(-0.7, +0.7) +
  scale_fill_manual(
    values = c(
      "1" = "red",
      "2" = "green",
      "3" = "black"
    )
  )
