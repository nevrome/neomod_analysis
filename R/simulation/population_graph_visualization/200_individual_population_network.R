load("data_simulation/example_sim_pop.RData")
load("data_simulation/example_sim_rel.RData")
load("data_analysis/region_order.RData")

pop_groups <- pop %>% 
  dplyr::group_by(unit) %>%
  dplyr::mutate(
    timeblock = plyr::round_any(birth_time, 100)
  ) %>%
  dplyr::group_by(unit, timeblock) %>%
  dplyr::summarise(
    n = n()
  ) %>%
  dplyr::ungroup()

frommer <- dplyr::left_join(
  rel[, c("from", "type")],
  pop,
  by = c("from" = "id")
) %>%
  dplyr::select(
    from, birth_time, unit, type
  ) %>%
  dplyr::rename(
    "from_birth_time" = "birth_time",
    "from_unit" = "unit"
  )

toer <- dplyr::left_join(
  rel[, c("to")],
  pop,
  by = c("to" = "id")
) %>%
  dplyr::select(
    to, birth_time, unit
  ) %>%
  dplyr::rename(
    "to_birth_time" = "birth_time",
    "to_unit" = "unit"
  )

rel2 <- cbind(frommer, toer)

rel3 <- rel2 %>%
  dplyr::filter(
    from_unit != to_unit
  ) %>%
  dplyr::mutate(
    timeblock_from = plyr::round_any(from_birth_time, 100),
    timeblock_to = plyr::round_any(to_birth_time, 100)
  ) %>%
  dplyr::select(
    -from, -to, -from_birth_time, -to_birth_time
  )

rel4 <- rel3 %>% dplyr::group_by(
  from_unit, to_unit, timeblock_from, timeblock_to
) %>%
  dplyr::summarise(
    n = n(),
    type = type[1]
  ) %>%
  dplyr::ungroup()

regions_factor <- as.factor(pop_groups$unit)
pop_groups$unit <- factor(regions_factor, levels = region_order)

library(ggplot2)
pu <- ggplot() +
  geom_point(
    data = pop_groups, 
    aes(
      x = timeblock, 
      y = unit, 
      size = n
    )
  ) +
  geom_segment(
    data = rel4,#[rel4$type != "friend", ], 
    aes(
      y = from_unit, yend = to_unit, 
      x = timeblock_from, xend = timeblock_to,
      color = type,
      alpha = type
    ),
    size = 0.6
  ) +
  scale_color_manual(
    values = c(
      "child_of" = "red",
      "friend" = "blue"
    ),
    labels = c(
      "child_of" = "vertical",
      "friend" = "horizontal"
    )
  ) +
  scale_alpha_manual(
    values = c(
      "child_of" = 0.4,
      "friend" = 0.1
    ),
    labels = c(
      "child_of" = "vertical",
      "friend" = "horizontal"
    )
  ) +
  theme_bw() +
  scale_y_discrete(limits = rev(levels(pop_groups$unit))) +
  scale_x_continuous(
    breaks = seq(min(pop_groups$timeblock), max(pop_groups$timeblock), 100)
  ) +
  theme(
    panel.grid.major.y = element_line(size = 0.5, colour = "black"),
    axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 10, angle = 45, vjust = 0, hjust = 1)
  ) +
  ylab(NULL) +
  xlab("timeblock in calBC")

pu %>%
  ggsave(
    "figures_plots/simulation_population_graph/population_group_graph.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 297, height = 210, units = "mm",
    limitsize = F
  )
