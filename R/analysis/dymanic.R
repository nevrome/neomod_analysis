load("../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")

cremation <- proportion_per_region_df %>%
  dplyr::filter(
    idea != "mound" & idea != "flat" & idea != "inhumation"
  ) %>%
  dplyr::filter(
    #region_name == "Northeast France and Benelux"
    #region_name == "Northern Germany"
    #region_name == "Great Britain and Ireland"
    region_name == "Jutland"
  )


library(splines)

#kuu <- spline(cremation$timestep, cremation$proportion, method = "fmm")

f_of_x <- splinefun(cremation$timestep, cremation$proportion, method = "fmm")

plot(cremation$timestep, f_of_x(cremation$timestep, deriv = 0), "l")
points(cremation$timestep, f_of_x(cremation$timestep, deriv = 1), "l", add = T)
