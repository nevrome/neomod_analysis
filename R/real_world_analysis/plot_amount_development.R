load("../neomod_datapool/bronze_age/development_amount_burial_type.RData")
load("../neomod_datapool/bronze_age/development_amount_burial_construction.RData")

amount_devel <- amount_development_burial_type
#amount_devel <- amount_development_burial_construction

regions_factor <- as.factor(amount_devel$region_name)
amount_devel$region_name <- factor(regions_factor, levels = c(
  "Austria and Czech Republic",
  "Poland",
  "Southern Germany",
  "Northeast France",
  "Northern Germany",
  "Southern Skandinavia",
  "Benelux",
  "England"
))

idea_factor <- as.factor(amount_devel$idea)
amount_devel$idea <- factor(idea_factor, levels = rev(levels(idea_factor)))

library(ggplot2)
spu <- ggplot() +
  geom_area(
    data = amount_devel,
    aes(x = timestep, y = n, fill = idea),
    position = 'stack',
    linetype = "blank"
  ) +
  facet_wrap(~region_name, nrow = 8) +
  xlab("Time in years calBC") +
  ylab("Amount of burials") +
  labs(fill = "Ideas (mutually exclusive)") + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    strip.text.x = element_text(size = 13),
    legend.title = element_text(size = 15),
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
    ylim = c(0, 80)
  ) +
  scale_x_reverse(breaks = c(2200, 2000, 1500, 1000, 800), limits = c(2500, 800))
  
  

region_file_list <- unique(amount_devel$region_name) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("../neomod_datapool/bronze_age/region_pictograms/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.1, height = 0.9
  )
})
dummy <- tibble::tibble(region_name = unique(amount_devel$region_name), grob = gl )

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
    "/home/clemens/neomod/neomod_datapool/bronze_age/development_amount_regions_burial_type.jpeg",
    #"/home/clemens/neomod/neomod_datapool/bronze_age/development_amount_regions_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )


