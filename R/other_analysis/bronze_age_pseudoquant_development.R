devel_table <- readr::read_csv("data_manually_prepared/burial_traditions_pseudo.csv")

regions_factor <- as.factor(devel_table$region)
devel_table$region<- factor(regions_factor, levels = c(
  "Austria and Czechia",
  "Poland",
  "Southern Germany",
  "Northeastern France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

dt2 <- devel_table %>%
  tidyr::gather(idea, strength, cremation, inhumation, mound, flat) %>%
  dplyr::mutate(
    time = dplyr::case_when(
      period == "Early Bronze Age" ~ -2000,
      period == "Middle Bronze Age" ~ -1500,
      period == "Late Bronze Age" ~ -1000
    )
  ) %>% 
  dplyr::mutate(
    time = time + dplyr::case_when(
      idea == "cremation" ~ -100,
      idea == "inhumation" ~ -100,
      idea == "mound" ~ 100,
      idea == "flat" ~ 100
    )
  )

idea_factor <- as.factor(dt2$idea)
dt2$idea <- factor(idea_factor, levels = rev(levels(idea_factor)))

library(ggplot2)
spu <- ggplot() +
  geom_bar(
    data = dt2,
    aes(x = time, y = strength, fill = idea),
    stat = "identity",
    position = "stack"
  ) +
  facet_wrap(~region, nrow = 8) +
  xlab("Time in years calBC") +
  ylab(" ") +
  labs(fill = " ") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15)
  ) +
  scale_fill_manual(
    values = c(
      "cremation" = "#D55E00",
      "inhumation" = "#0072B2",
      "mound" = "#CC79A7",
      "flat" = "#009E73",
      "unknown" = "grey85"
    )
  ) +
  coord_cartesian(
    ylim = c(0, 4)
  ) +
  scale_x_continuous(
    breaks = c(-2200, -2000, -1500, -1000, -800), 
    limits = c(-2500, -800)
  )



region_file_list <- unique(dt2$region) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("figures_plots/region_pictograms_colour/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.14, height = 1.2
  )
})
dummy <- tibble::tibble(region = unique(dt2$region), grob = gl )

source("R/helper_functions/geom_grob.R")

spu <- spu +
  geom_custom(
    data = dummy, 
    aes(grob = grob), 
    inherit.aes = FALSE,
    x = 0.1, y = 0.5
  )

spu %>%
  ggsave(
    "figures_plots/development/development_pseudoquant.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )


