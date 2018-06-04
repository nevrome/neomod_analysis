load("../neomod_datapool/bronze_age/development_amount_burial_type.RData")
load("../neomod_datapool/bronze_age/development_amount_burial_construction.RData")

amount_devel <- amount_development_burial_type
#amount_devel <- amount_development_burial_construction

amount_devel <- amount_devel %>%
  dplyr::filter(
    region_name == "Southern Germany"
  )

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
  xlab("Time in years calBC") +
  ylab("Amount of burials") +
  labs(fill = "Ideas (mutually exclusive)") + 
  theme_bw() +
  theme(
    panel.grid.major.x = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 15),
    axis.title = element_blank()
  ) +
  scale_fill_manual(
    guide = FALSE,
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

######

load("../neomod_datapool/bronze_age/development_proportions_burial_type.RData")
load("../neomod_datapool/bronze_age/development_proportions_burial_construction.RData")

library(ggplot2)

prop <- proportion_development_burial_type
# prop <- proportion_development_burial_construction

prop <- prop %>%
  dplyr::filter(
    region_name == "Southern Germany"
  )

prop$idea <- as.factor(prop$idea)
prop$idea <- factor(prop$idea , levels = rev(levels(prop$idea )))

hu <- ggplot() +
  geom_area(
    data = prop,
    mapping = aes(x = timestep, y = proportion, fill = idea),
    position = 'stack',
    linetype = "blank"
  ) +
  geom_line(
    data = dplyr::filter(prop, idea == "cremation"),
    #data = dplyr::filter(prop, idea == "flat"),
    mapping = aes(x = timestep, y = proportion),
    color = "black",
    size = 0.2
  ) +
  scale_alpha_continuous(range = c(0.0, 0.7)) +
  xlab("Time in years calBC") +
  ylab("Proportion of burials") +
  labs(fill = "Ideas (mutually exclusive)") + 
  theme_bw() +
  theme(
    panel.grid.major.x = element_line(colour = "black", size = 0.3),
    axis.text = element_text(size = 15),
    axis.title = element_blank()
  ) +
  scale_fill_manual(
    guide = FALSE,
    values = c(
      "cremation" = "#D55E00",
      "inhumation" = "#0072B2",
      "mound" = "#CC79A7",
      "flat" = "#009E73"
    )
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%")
  ) +
  scale_x_reverse(breaks = c(2200, 2000, 1500, 1000, 800), limits = c(2500, 800))



region_file_list <- unique(prop$region_name) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("../neomod_datapool/bronze_age/region_pictograms/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.1, height = 0.9
  )
})
dummy <- tibble::tibble(region_name = unique(prop$region_name), grob = gl )

source("R/helper_functions/geom_grob.R")

hu <- hu +
  geom_custom(
    data = dummy, 
    aes(grob = grob), 
    inherit.aes = FALSE,
    x = 0.1, y = 0.5
  )

######

library(grid)
grid.newpage()
glu <- grid.arrange(rbind(ggplotGrob(spu), ggplotGrob(hu), size = "last"))

glu %>%
  ggsave(
    "/home/clemens/neomod/neomod_analysis/R/real_world_analysis/Falkenstein/plot_burial_type.jpeg",
    #"/home/clemens/neomod/neomod_analysis/R/real_world_analysis/Falkenstein/plot_burial_construction.jpeg",
    plot = .,
    device = "jpeg",
    scale = 1,
    dpi = 300,
    width = 200, height = 80, units = "mm",
    limitsize = F
  )



