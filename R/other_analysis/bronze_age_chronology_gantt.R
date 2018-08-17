chronology <- readr::read_csv(
  "data_manually_prepared/bronze_age_chronology.csv"
  )

chronology %<>% dplyr::mutate(
  start_date = replace(start_date, is.na(start_date), -Inf), 
  end_date = replace(end_date, is.na(end_date), Inf), 
  label_pos = (start_date + end_date) / 2, 
  unit_name = factor(as.factor(unit_name), levels = unique(as.factor(unit_name)[order(start_date)])),
  context_general = factor(as.factor(context_general), levels = c(
    "Central Europe",
    "France",
    "Scandinavia",
    "Netherlands", 
    "Belgium",
    "Britain",
    "Irland"
  )),
  unit_general = factor(as.factor(unit_general), levels = c(
    "Neolithic",
    "Chalcolithic",
    "Early Bronze Age",
    "Middle Bronze Age", 
    "Late Bronze Age",
    "Iron Age"
  )),
  sub_context = stringr::str_wrap(paste0(reference, "\n", ifelse(is.na(sub_context), "", sub_context)), width = 10)
  )

separators <- chronology %>%
  dplyr::select(-reference, -start_date_pre, -end_date_post, -unit_name, -label_pos, -unit_general) %>%
  tidyr::gather(
    unit, dates, -sub_context, -context_general
  )

line_width = 22

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
  scale_color_manual(
    values = c(
      "Neolithic" = "grey",
      "Chalcolithic" = "#a6cee3",
      "Early Bronze Age" = "#1f78b4",
      "Middle Bronze Age" = "#b2df8a",
      "Late Bronze Age" = "#33a02c",
      "Iron Age" = "grey"
    )
  ) +
  geom_point(
    data = separators,
    mapping = aes(
      x = sub_context,
      y = dates
    ),
    shape = 3,
    size = 1,
    position = position_dodge(preserve = "single", width = 0.8)
  ) +
  geom_point(
    data = separators,
    mapping = aes(
      x = sub_context,
      y = dates
    ),
    shape = 3,
    size = 1,
    position = position_dodge(preserve = "single", width = -0.8)
  ) +
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
  xlab(NULL) +
  ylab(NULL) +
  scale_y_reverse(
    breaks = seq(-800, -2200, -200),
    limits = c(-700, -2500),
    sec.axis = dup_axis(name = NULL)
  ) +
  facet_grid(cols = vars(context_general), scales = "free_x", space = "free_x") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    strip.text.x = element_text(size = 8),
    panel.grid.major.y = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 15),
    legend.spacing.x = unit(.3, 'cm')
  ) +
  guides(
    color = guide_legend(nrow = 1, override.aes = list(size = 8))
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

