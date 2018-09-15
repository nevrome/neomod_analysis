models <- pbapply::pblapply(
  list.files("../simulationdata/pe_startprop_distancemat", full.names = TRUE),
  function(y) {
    read.csv(y) %>% tibble::as.tibble()
  }
)

models_groups <- do.call(rbind, models) %>%
  base::split(.$model_group)

load("data_simulation/pe_startprop_distancemat.RData")

library(ggplot2)
plots <- cowplot::plot_grid(
  plotlist = lapply(models_groups, plot_by_group),
  labels = paste0(LETTERS[1:length(models_groups)], " - ", names(models_groups)),
  label_x = 0,
  hjust = 0,
  label_size = 10,
  ncol = 4,
  nrow = 3,
  align = "v"
)

plots %>%
  ggsave(
    "figures_plots/simulation_parameter_exploration/startprop_distancemat.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )
