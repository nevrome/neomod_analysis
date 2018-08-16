chronology <- readr::read_delim(
  "data_manually_prepared/bronze_age_chronology.csv",
  delim = "\t"
  )

chronology %<>% dplyr::mutate(
  start_date = replace(start_date, is.na(start_date), -Inf), 
  end_date = replace(end_date, is.na(end_date), Inf), 
  label_pos = (start_date + end_date) / 2, 
  unit_name = factor(as.factor(unit_name), levels = unique(as.factor(unit_name)[order(start_date)])),
  spatial_context = factor(as.factor(spatial_context), levels = rev(c(
   "Britain", "France", "Scandinavia", "Central Europe", "North and central Italy", "Spain"
  )))
  )

dodge_value <- 1.4
line_width <- 6
  
library(ggplot2)
hu <- chronology %>%
  ggplot() +
  geom_linerange(
    aes(
      x = spatial_context,
      ymin = start_date_pre,
      ymax = start_date,
      group = unit_name
    ),
    size = line_width,
    color = "darkgrey",
    position = position_dodge(dodge_value)
  ) +
  geom_linerange(
    aes(
      x = spatial_context,
      ymin = end_date,
      ymax = end_date_post,
      group = unit_name
    ),
    size = line_width,
    color = "darkgrey",
    position = position_dodge(dodge_value)
  ) +
  geom_linerange(
    aes(
      x = spatial_context,
      ymin = start_date,
      ymax = end_date,
      group = unit_name,
      color = spatial_context
    ),
    size = line_width,
    position = position_dodge(dodge_value)
  ) +
  geom_text(
    aes(
      x = spatial_context,
      y = label_pos,
      label = unit_name,
      group = unit_name
    ),
    position = position_dodge(dodge_value),
    size = 3
  ) +
  coord_flip(ylim = c(-2550, -500)) + 
  theme_bw() +
  guides(color = FALSE) +
  xlab("") +
  ylab("Time in years calBC") +
  scale_y_continuous(
    breaks = seq(-2500, -500, 200),
    limits = c(-2550, -500)
  )

# hu %>%
#   ggsave(
#     paste0("/home/clemens/neomod/neomod_datapool/bronze_age/harding_chronology.jpeg"),
#     plot = .,
#     device = "jpeg",
#     scale = 1,
#     dpi = 300,
#     width = 210, height = 297, units = "mm",
#     limitsize = F
#   )

