load("../neomod_datapool/bronze_age/space_and_network/proportions_per_region_df.RData")

cremation <- proportion_per_region_df %>%
  dplyr::filter(
    idea != "mound" & idea != "flat" & idea != "inhumation"
  ) %>%
  dplyr::filter(
    region_name == "Northeast France and Benelux"
    #region_name == "Northern Germany"
    #region_name == "Great Britain and Ireland"
    #region_name == "Jutland"
  )

# po <- cremation %>%
#   lm(timestep ~ proportion, data = .)
# 
# f_of_x <- splinefun(cremation$timestep, cremation$proportion, method = "fmm")
# 
# plot(cremation$timestep, cremation$proportion, "l")
# plot(cremation$timestep, f_of_x(cremation$timestep, deriv = 0), "l", add = T, col = "red")
# points(cremation$timestep, f_of_x(cremation$timestep, deriv = 1), "l", add = T)

schmooth <- smooth.spline(cremation$timestep, cremation$proportion, spar = 0.5)

plot(cremation$timestep, cremation$proportion, "l")
lines(cremation$timestep, predict(schmooth, cremation$timestep, deriv = 0)$y, "l", col = "red")
plot(cremation$timestep, predict(schmooth, cremation$timestep, deriv = 1)$y, "l", col = "blue")


