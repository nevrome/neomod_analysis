load("data_simulation/sim1.RData")
load("data_analysis/development_proportions_burial_type.RData")
load("data_analysis/development_proportions_burial_construction.RData")
load("data_analysis/region_colors.RData")

burial_type_long_prop <- proportion_development_burial_type %>%
  dplyr::rename(
    region = region_name
  ) %>%
  dplyr::mutate(
    model_id = -2,
    model_group = 0,
    idea = dplyr::case_when(
      idea == "cremation" ~ "idea_1",
      idea == "inhumation" ~ "idea_2"
    )
  ) %>%
  tibble::as.tibble()
 
burial_construction_long_prop <- proportion_development_burial_construction %>%
  dplyr::rename(
    region = region_name
  ) %>%
  dplyr::mutate(
    model_id = -1,
    model_group = 0,
    idea = dplyr::case_when(
      idea == "flat" ~ "idea_1",
      idea == "mound" ~ "idea_2"
    )
  ) %>%
  tibble::as.tibble()

prop <- c(
    models_grid$idea_proportions[1:47],
    list(burial_type_long_prop, burial_construction_long_prop)
  ) %>%
  do.call(rbind, .)

long_prop <- prop %>%
  tidyr::spread(
    idea, proportion
  )

library(ggplot2)
fu <- ggplot(long_prop) +
  # geom_line(
  #   aes(
  #     x = timestep,
  #     y = idea_1,
  #     colour = region
  #   )
  # ) +
  geom_smooth(
    aes(
      x = timestep,
      y = idea_1,
      colour = region
    ),
    method = "loess",
    span = 0.1
  ) +
  facet_wrap(
    ~model_id,
    nrow = 7,
    ncol = 7
  ) +
  theme_bw() +
  scale_color_manual(
    values = region_colors
  ) +
  xlab("Time in years calBC") +
  ylab("Proportions") +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title=element_blank()
  ) +
  scale_x_continuous(
    breaks = c(-2000, -1500, -1000)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%")
  )

fu %>%
  ggsave(
    "../neomod_datapool/plots/development_simulation/development_proportions_regions_simulation_improved_vis.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 350, height = 360, units = "mm",
    limitsize = F
  )
