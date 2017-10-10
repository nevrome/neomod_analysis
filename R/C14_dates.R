library(c14databases)
library(magrittr)

all_dates <- c14databases::get_all_dates()

#save(all_dates_2, file = "~/neomod/neomod_datapool/C14_dates/calibrated_c14_dates.Rdata")

cleaned_data <- all_dates %>%
  c14databases::order_variables() %>%
  c14databases::rm_doubles() %>%
  c14databases::calibrate() %>%
  c14databases::thesaurify() %>%
  c14databases::estimate_spatial_quality() %>%
  c14databases::order_variables()

#save(cleaned_data, file = "~/neomod/neomod_datapool/C14_dates/cleaned_c14_dates.Rdata")

cleaned_data <- cleaned_data %>%
  dplyr::mutate_if(is.character, dplyr::funs(iconv(., "UTF-8", "UTF-8", sub = '')))

cd_neol_2 <- cleaned_data  %>% dplyr::filter(
  spatial_quality == "possibly correct"
) %>%
  dplyr::filter(
    (lat > 25) & (lon < 60)
  ) %>% dplyr::filter(
    grepl("neolithic", period, ignore.case = TRUE)
  )

c14_neol <- cd_neol_2 %>%
  dplyr::filter(
    !is.na(lat) & !is.na(lon)
  )

save(c14_neol, file = "~/neomod/neomod_datapool/C14_dates/neol_c14_dates.Rdata")
