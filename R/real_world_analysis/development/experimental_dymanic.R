load("../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")

proportion_per_region_df$timestep <- proportion_per_region_df$timestep * (-1)

smooth_model <- function(df) {
  smooth.spline(df$timestep, df$proportion, spar = 0.5)
}

predict_spline <- function(df, model, deriv = 1) {
  prediction <- predict(model, rev(df$timestep), deriv = deriv)
  tibble::tibble(
    timestep = prediction$x,
    prediction = prediction$y
  )
}

kuu <- proportion_per_region_df %>%
  dplyr::filter(
    idea != "mound" & idea != "inhumation"
  ) %>%
  dplyr::group_by(idea, region_name) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    spline_model = purrr::map(
      data, .f = smooth_model
    )
  ) %>%
  dplyr::mutate(
    spline_prediction = purrr::map2(
      data, spline_model, .f = predict_spline
    )
  ) %>%
  tidyr::unnest(spline_prediction)
  
kuu2 <- kuu %>% dplyr::filter(
  idea != "flat"
) 

library(ggplot2)
spu <- ggplot() +
  geom_line(
    data = kuu2,
    aes(x = timestep, y = prediction)
  ) +
  facet_wrap(~region_name, nrow = 11) +
  #scale_x_reverse() +
  xlab("Time in years calBC") +
  labs(fill = "Memes (mutually exclusive)") + 
  theme_bw() +
  theme(
    legend.position="bottom",
    axis.title.y = element_blank(),
    panel.grid.major.x = element_line(colour = "black", size = 0.3)
  ) +
  xlim(-2700, -500)

region_file_list <- unique(kuu2$region_name) %>% gsub(" ", "_", ., fixed = TRUE)

gl <- lapply(region_file_list, function(x) {
  img <- png::readPNG(paste0("../neomod_datapool/bronze_age/region_pictograms/", x, ".png"))
  g <- grid::rasterGrob(
    img, interpolate = TRUE,
    width = 0.06, height = 0.8
  )
})
dummy <- tibble::tibble(region_name = unique(kuu2$region_name), grob = gl )

source("R/helper/geom_grob.R")

spu <- spu +
  geom_custom(
    data = dummy, 
    aes(grob = grob), 
    inherit.aes = FALSE,
    x = 0.07, y = 0.5
  )

# spu %>%
#   ggsave(
#     #"/home/clemens/neomod/neomod_datapool/bronze_age/amount_development_regions_cremation_inhumation.jpeg",
#     "/home/clemens/neomod/neomod_datapool/bronze_age/derivative_proportion_cremation_1.jpeg",
#     plot = .,
#     device = "jpeg",
#     scale = 1,
#     dpi = 300,
#     width = 210, height = 297, units = "mm",
#     limitsize = F
#   )
