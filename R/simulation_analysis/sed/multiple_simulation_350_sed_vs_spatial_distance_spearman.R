load("../neomod_datapool/simulation_data/mantel_sed_spatial_simulation.RData")
mantel_simulations <- mantel_test_results
load("../neomod_datapool/R_data/mantel_sed_spatial_burial_type.RData")
mantel_burial_type <- mantel_test_results
load("../neomod_datapool/R_data/mantel_sed_spatial_burial_construction.RData")
mantel_burial_construction <- mantel_test_results

mantel_burial_type %<>%
  dplyr::mutate(
    context = "burial_type"
  )

mantel_burial_construction %<>%
  dplyr::mutate(
    context = "burial_construction"
  )

mantel_real_world <- rbind(mantel_burial_type, mantel_burial_construction)

library(ggplot2)
ju <- ggplot() +
  geom_hline(
    yintercept = 0,
    colour = "red",
    size = 2
  ) + 
  geom_boxplot(
    data = mantel_simulations,
    mapping = aes(
      x = time,
      y = statistic
    ),
    width = 0.2
  ) +
  geom_dotplot(
    data = mantel_simulations,
    mapping = aes(
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
    position = position_nudge(x = -0.4),
    dotsize = 0.35,
    binpositions = "all"
  ) +
  geom_point(
    data = mantel_real_world,
    mapping = aes(
      x = time,
      y = statistic,
      colour = context
    ),
    position = position_nudge(x = -0.25),
    size = 6
  ) +
  scale_fill_manual(
    name = "Mantel test significance level of simulation runs",
    values = c(
      "< 0.01" = "#800026",
      "< 0.05" = "#e31a1c",
      "< 0.1" = "#fd8d3c",
      "> 0.1" = "white"
    )
  ) +
  scale_colour_manual(
    name = "Real world context",
    values = c(
      "burial_type" = "#0072B2",
      "burial_construction" = "#009E73"
    )
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 15, angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15),
    legend.box = "vertical"
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
