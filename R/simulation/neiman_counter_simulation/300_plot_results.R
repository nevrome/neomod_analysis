models <- pbapply::pblapply(
  list.files("data_simulation/neiman_counter_simulation", full.names = TRUE),
  function(y) {
    read.csv(y) %>% tibble::as.tibble()
  }
)

models_groups <- do.call(rbind, models) %>%
  base::split(.$model_group)

library(ggplot2)
plots <- cowplot::plot_grid(
  plotlist = lapply(models_groups, plot_by_group),
  labels = "AUTO", 
  ncol = 3,
  nrow = 3,
  align = "v"
)

plots %>%
  ggsave(
    "figures_plots/simulation_counter_neiman/counter_neiman_general.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )
