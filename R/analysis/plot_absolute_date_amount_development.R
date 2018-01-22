#load("../neomod_datapool/bronze_age/amount_development_burial_type.RData")
load("../neomod_datapool/bronze_age/amount_development_burial_construction.RData")

#amount_devel <- amount_development_burial_type
amount_devel <- amount_development_burial_construction

library(ggplot2)
spu <- ggplot() +
  geom_area(
    data = amount_devel,
    aes(x = timestep, y = n, fill = idea),
    position = 'stack',
    linetype = "blank"
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
      "inhumation" = "#0072B2",
      "mound" = "#CC79A7",
      "flat" = "#009E73",
      "unknown" = "grey85"
    )
  ) +
  xlim(2700, 500)

region_file_list <- unique(amount_devel$region_name) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("../neomod_datapool/bronze_age/region_pictograms/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.06, height = 0.8
  )
})
dummy <- tibble(region_name = unique(amount_devel$region_name), grob = gl )

source("R/helper/geom_grob.R")

spu <- spu +
  geom_custom(
    data = dummy, 
    aes(grob = grob), 
    inherit.aes = FALSE,
    x = 0.07, y = 0.5
  )

spu %>%
  ggsave(
    #"/home/clemens/neomod/neomod_datapool/bronze_age/amount_development_regions_cremation_inhumation.jpeg",
    "/home/clemens/neomod/neomod_datapool/bronze_age/amount_development_regions_mound_flat.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 210, height = 297, units = "mm",
    limitsize = F
  )


