chronology <- readr::read_csv(
  "data_manually_prepared/bronze_age_chronology.csv"
  )

chronology %<>% dplyr::mutate(
  start_date = replace(start_date, is.na(start_date), -Inf), 
  end_date = replace(end_date, is.na(end_date), Inf), 
  label_pos = (start_date + end_date) / 2, 
  unit_name = factor(as.factor(unit_name), levels = unique(as.factor(unit_name)[order(start_date)])),
  sub_context = as.factor(sub_context)
  # sub_context = factor(as.factor(sub_context), levels = rev(c(
  #  "Britain", "France", "Scandinavia", "Central Europe", "North and central Italy", "Spain"
  # )))
  )

separators <- chronology %>%
  dplyr::select(-reference, -start_date_pre, -end_date_post, -unit_name, -label_pos, -unit_general) %>%
  tidyr::gather(
    unit, dates, -sub_context, -context_general
  )

line_width = 15

library(ggplot2)
hu <- ggplot() +
  geom_linerange(
    data = chronology, 
    mapping = aes(
      x = sub_context,
      ymin = start_date_pre,
      ymax = start_date,
      group = unit_name
    ),
    size = line_width,
    color = "darkgrey"
  ) +
  geom_linerange(
    data = chronology, 
    mapping = aes(
      x = sub_context,
      ymin = end_date,
      ymax = end_date_post,
      group = unit_name
    ),
    size = line_width,
    color = "darkgrey"
  ) +
  geom_linerange(
    data = chronology, 
    mapping = aes(
      x = sub_context,
      ymin = start_date,
      ymax = end_date,
      group = unit_name,
      color = unit_general
    ),
    size = line_width
  ) +
  # geom_segment(
  #   data = separators,
  #   mapping = aes(
  #     x = as.numeric(sub_context) - 0.2,
  #     xend = as.numeric(sub_context) + 0.2,
  #     y = dates,
  #     yend = dates
  #   )
  # ) +
  geom_text(
    data = chronology, 
    mapping = aes(
      x = sub_context,
      y = label_pos,
      label = unit_name,
      group = unit_name
    ),
    size = 3
  ) +
  theme_bw() +
  guides(color = FALSE) +
  xlab("") +
  ylab("Time in years calBC") +
  scale_y_reverse(
    breaks = seq(-800, -2200, -200),
    limits = c(-700, -2500)
  ) +
  facet_grid(cols = vars(context_general), scales = "free_x", space = "free_x") +
  theme(
    panel.spacing = unit(0, "lines"), 
    strip.background = element_blank(),
    strip.placement = "outside"
  )

hu %>%
  ggsave(
    paste0("figures_plots/chronology/bronze_age_europe_chronology.jpeg"),
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 297, height = 210, units = "mm",
    limitsize = F
  )

