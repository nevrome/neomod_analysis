load("../neomod_datapool/simulation_data/mantel_sed_spatial_simulation.RData")

library(ggplot2)
ju <- ggplot(mantel_test_results) +
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
      fill = base::cut(
        signif, 
        breaks = c(0, 0.01, 0.05, 0.1, seq(0.2, 1, 0.1)), 
        labels = c("< 0.01", "< 0.05", "< 0.1", rep("> 0.1", 9))
      )
    ),
    binaxis = "y",
    stackdir = "down",
    position = position_nudge(x = -0.3),
    dotsize = 0.4,
    binpositions = "all"
  ) +
  scale_fill_manual(
    name = "Mantel test significance level",
    values = c(
      "< 0.01" = "#800026",
      "< 0.05" = "#e31a1c",
      "< 0.1" = "#fd8d3c",
      "> 0.1" = "white"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 15, angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15)
  ) +
  ylab("Spearman's rank correlation coefficient") +
  xlab("time blocks calBC")

ju %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/plots/sed_simulation/mantel_test_many_simulations.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 300, height = 300, units = "mm",
    limitsize = F
  )
