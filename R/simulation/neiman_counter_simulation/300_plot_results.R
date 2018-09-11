models <- pbapply::pblapply(
  list.files("data_simulation/neiman_counter_simulation", full.names = TRUE),
  function(y) {
    read.csv(y) %>% tibble::as.tibble()
  }
)

models_groups <- do.call(rbind, models) %>%
  base::split(.$model_group)

load("data_simulation/neiman_counter_simulation.RData")

library(ggplot2)
plots <- cowplot::plot_grid(
  plotlist = lapply(models_groups, plot_by_group),
  labels = models_grid %>% base::split(.$model_group) %>%
    sapply(function(x){
      rps <- x$unit_size_functions[[1]][[1]](0)
      cuiv <- x$cross_unit_proportion_child_of[1]
      cuih <- x$cross_unit_proportion_friend[1]
      paste0(
        LETTERS[x$model_group[1]], " - ", rps, ", v:", format(cuiv, scientific = FALSE), ", h:", cuih)
    }),
  label_x = 0,
  hjust = 0,
  label_size = 10,
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
