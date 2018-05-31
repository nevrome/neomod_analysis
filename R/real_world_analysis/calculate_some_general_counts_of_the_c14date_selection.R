load("../neomod_datapool/bronze_age/bronze1.RData")

# get some counts
bronze1 %>%
  dplyr::group_by(
    burial_type
  ) %>%
  dplyr::summarise(
    n()
  )

bronze1 %>%
  dplyr::group_by(
    burial_construction
  ) %>%
  dplyr::summarise(
    n()
  )

bronze1 %>%
  dplyr::filter(
    burial_type != "unknown",
    burial_construction != "unknown"
  ) %>%
  dplyr::group_by(
    burial_type, burial_construction
  ) %>%
  dplyr::summarise(
    n = n()
  ) %$%
  sum(n)