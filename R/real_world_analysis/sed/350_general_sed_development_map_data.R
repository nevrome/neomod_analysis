load("data_analysis/regions.RData")
load("data_analysis/squared_euclidian_distance_over_timeblocks_burial_type.RData")
burial_type_distance <- sed_spatial_distance %>% dplyr::mutate(context = "burial_type")
load("data_analysis/squared_euclidian_distance_over_timeblocks_burial_construction.RData")
burial_construction_distance <- sed_spatial_distance %>% dplyr::mutate(context = "burial_construction")

distance <- rbind(burial_type_distance, burial_construction_distance)

save(distance, file = "data_analysis/squared_euclidian_distance_over_timeblocks.RData")

region_centers <- regions %>%
  sf::st_centroid()

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

region_centers %>%
  sfc_as_cols() %>%
  dplyr::select(
    NAME, x, y
  )

distance_lines <- distance %>%
  dplyr::left_join(
    region_centers,
    by = c("regionA" = "NAME")
  ) %>%
  dplyr::left_join(
    region_centers,
    by = c("regionB" = "NAME"),
    suffix = c("_regionA", "_regionB")
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    x_a = sf::st_coordinates(geometry_regionA)[,1],
    y_a = sf::st_coordinates(geometry_regionA)[,2],
    x_b = sf::st_coordinates(geometry_regionB)[,1],
    y_b = sf::st_coordinates(geometry_regionB)[,2]
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    context, time, regionA, regionB, distance, mean_sed, x_a, y_a, x_b, y_b
  ) %>%
  dplyr::filter(
    regionA != regionB
  )

save(distance_lines, file = "data_analysis/sed_time_spatial_network.RData")
