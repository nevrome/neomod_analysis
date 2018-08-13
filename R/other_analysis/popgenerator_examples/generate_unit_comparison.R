devtools::load_all("../popgenerator/R")

time <- 1:1000
testunitA_function <- function(t) {300}
testunitB_function <- function(t) {500 + t/4}
testunitC_function <- function(t) {800 + (0.04 * (t - 500))^2}
#plot(time, testunitC_function(time))

testunitA <- methods::new(
  "unit_settings",
  time =                      time,
  unit_name =                 as.factor("testunitA"),
  unit_size_function =        testunitA_function,
  age_distribution_function = function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))},
  age_range =                 1:90
) %>%
  generate_unit() %>% 
  count_living_humans_over_time(time) %>%
  tibble::tibble(
    time = time,
    count = .
  )

testunitB <- methods::new(
  "unit_settings",
  time =                      time,
  unit_name =                 as.factor("testunitA"),
  unit_size_function =        testunitB_function,
  age_distribution_function = function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))},
  age_range =                 1:90
) %>%
  generate_unit() %>% 
  count_living_humans_over_time(time) %>%
  tibble::tibble(
    time = time,
    count = .
  )

testunitC <- methods::new(
  "unit_settings",
  time =                      time,
  unit_name =                 as.factor("testunitA"),
  unit_size_function =        testunitC_function,
  age_distribution_function = function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))},
  age_range =                 1:90
) %>%
  generate_unit() %>% 
  count_living_humans_over_time(time) %>%
  tibble::tibble(
    time = time,
    count = .
  )

library(ggplot2)
ggplot() +
  geom_line(
    data = testunitA,
    mapping = aes(time, count)
  ) +
  geom_line(
    data = data.frame(time, count = testunitA_function(time)),
    mapping = aes(time, count),
    colour = "red"
  ) +
  geom_line(
    data = testunitB,
    mapping = aes(time, count)
  ) +
  geom_line(
    data = data.frame(time, count = testunitB_function(time)),
    mapping = aes(time, count),
    colour = "red"
  ) +
  geom_line(
    data = testunitC,
    mapping = aes(time, count)
  ) +
  geom_line(
    data = data.frame(time, count = testunitC_function(time)),
    mapping = aes(time, count),
    colour = "red"
  ) 
