load("../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")

library(ggplot2)

prop <- proportion_per_region_df %>% 
  dplyr::filter(
    idea != "flat" & idea != "mound"
  )

prop$idea <- as.factor(prop$idea)
prop$idea <- factor(prop$idea , levels = rev(levels(prop$idea )))

#hu <- proportion_per_region_df %>%
hu <- prop %>%
  ggplot(aes()) +
  geom_area(
    aes(x = timestep, y = proportion, fill = idea),
    position = 'stack',
    alpha = 0.6,
    linetype = "blank"
  ) +
  geom_line(
    data = dplyr::filter(prop, idea == "cremation"),
    mapping = aes(x = timestep, y = proportion),
    color = "black",
    size = 0.2
  ) +
  facet_wrap(~region_name, nrow = 11) +
  scale_x_reverse() +
  xlab("Time in years calBC") +
  labs(fill = "Memes (mutually exclusive)") + 
  theme_bw() +
  theme(
    legend.position="bottom",
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(colour = "black", size = 0.3)
  ) +
  scale_fill_manual(
    values = c(
      "cremation" = "#D55E00",
      "inhumation" = "#0072B2"
    )
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%")
  ) +
  xlim(2800, 500)

#img <- magick::image_read("../neomod_datapool/bronze_age/region_pictograms/test.png")
# hu <- hu + cowplot::draw_image(img, x = -2700, y = -0.5, scale = 100)
# https://github.com/tidyverse/ggplot2/issues/1399

hu %>%
  ggsave(
    "/home/clemens/neomod/neomod_datapool/bronze_age/proportions_development_regions.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )
