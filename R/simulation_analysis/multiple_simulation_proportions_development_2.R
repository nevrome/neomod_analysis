load("R/simulation_results/sim1.RData")
load("../neomod_datapool/bronze_age/development_proportions_burial_type.RData")
#load("../neomod_datapool/bronze_age/development_proportions_burial_construction.RData")

burial_type_long_prop <- proportion_development_burial_type %>%
  dplyr::rename(
    region = region_name
  ) %>%
  dplyr::mutate(
    timestep = -timestep,
    model_id = 0,
    model_group = 0,
    idea = dplyr::case_when(
      idea == "cremation" ~ "idea_1",
      idea == "inhumation" ~ "idea_2"
    )
  ) %>%
  tibble::as.tibble()
 
# burial_construction_long_prop <- proportion_development_burial_construction %>%
#   dplyr::rename(
#     region = region_name
#   ) %>%
#   dplyr::mutate(
#     timestep = -timestep,
#     model_id = -1,
#     model_group = 0,
#     idea = dplyr::case_when(
#       idea == "flat" ~ "idea_1",
#       idea == "mound" ~ "idea_2"
#     )
#   ) %>%
#   tibble::as.tibble()

prop <- c(
    models_grid$idea_proportions,
    list(burial_type_long_prop)
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
    nrow = 13,
    ncol = 4
  ) +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Austria and Czechia" = "#999999", 
      "Poland" = "#E69F00", 
      "Southern Germany" = "#56B4E9", 
      "Northeast France" = "#009E73", 
      "Northern Germany" = "#000000", 
      "Southern Skandinavia" = "#0072B2", 
      "Benelux" = "#D55E00", 
      "England" = "#CC79A7"
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    strip.text.x = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

fu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/development_proportions_regions_simulation_2.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )
