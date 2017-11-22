load("../neomod_datapool/bronze_age/research_area/research_area_hex.RData")

crs_wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

load("../neomod_datapool/bronze_age/bronze2.RData")
bronze_sp <- bronze2 %>% as.data.frame() %>%
  SpatialPointsDataFrame(
    coords = .[c("lon", "lat")],
    proj4string = sp::CRS(crs_wgs)
  )

dates_per_hex <- bronze_sp %>% sp::over(
  research_area_hex, ., returnList = T
)

fncols <- function(data, cname) {
  add <- cname[!cname %in% names(data)]
  if (length(add) != 0) {data[add] <- NA_real_}
  return(data)
}

proportion_per_hex <- dates_per_hex %>%
  pbapply::pblapply(function(x) {
    if (nrow(x) == 0 | 
        (all(x$burial_type == "unknown") & 
         all(x$burial_construction == "unknown"))) {
      res <- NULL
    } else {
      bt_basic <- x %>% 
        dplyr::filter(
          burial_type != "unknown"
        ) 
      if (nrow(bt_basic) == 0) {
        bt <- tibble::tibble(
          age = double(),
          cremation = character(), 
          inhumation = character()
        )
      } else {
        bt <- bt_basic %>%
          dplyr::group_by(age, burial_type) %>%
          dplyr::summarise(
            count = n()
          ) %>% 
          tidyr::spread(
            key = burial_type, value = count
          ) %>%
          fncols(c("cremation", "inhumation")) %>%
          dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0))) %>%
          dplyr::mutate(
            cremation = cremation/sum(cremation, inhumation),
            inhumation = inhumation/sum(cremation, inhumation)
          ) %>%
          dplyr::ungroup()
      }
      
        bc_basic <- x %>% 
          dplyr::filter(
            burial_construction != "unknown"
          ) 
        if (nrow(bc_basic) == 0) {
          bc <- tibble::tibble(
            age = double(),
            mound = character(), 
            flat = character()
          )
        } else {
          bc <- bc_basic %>%
            dplyr::group_by(age, burial_construction) %>%
            dplyr::summarise(
              count = n()
            ) %>% 
            tidyr::spread(
              key = burial_construction, value = count
            ) %>%
            fncols(c("mound", "flat")) %>%
            dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0))) %>%
            dplyr::mutate(
              mound = mound/sum(mound, flat),
              flat = flat/sum(mound, flat)
            ) %>%
            dplyr::ungroup()
        }
        
      res <- dplyr::full_join(
        bt, bc, by = "age"
      ) %>%
        dplyr::mutate_all(dplyr::funs(replace(., is.na(.), 0)))
    }
    return(res)
  })



