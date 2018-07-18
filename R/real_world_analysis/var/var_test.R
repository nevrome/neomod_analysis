library(tseries) # for `adf.test()`
library(dynlm) #for function `dynlm()`
library(vars) # for function `VAR()`
library(nlWaldTest) # for the `nlWaldtest()` function
library(lmtest) #for `coeftest()` and `bptest()`.
library(broom) #for `glance(`) and `tidy()`
library(PoEdata) #for PoE4 datasets
library(car) #for `hccm()` robust standard errors
library(sandwich)
library(forecast) 

load("../neomod_datapool/R_data/development_proportions_burial_type.RData")
load("../neomod_datapool/R_data/development_proportions_burial_construction.RData")

proportion_development_burial_type %<>%
  dplyr::mutate(
    region_name = gsub(" ", ".", region_name)
  )

bt <- proportion_development_burial_type %>%
  dplyr::filter(idea == "cremation") %>%
  dplyr::select(-idea) %>%
  tidyr::spread(region_name, proportion) %>%
  dplyr::select(-timestep) 

bt2_with_unit <- bt %>% 
  dplyr::mutate(
    time_unit = c(lapply(1:30, function(x) { rep(x, 47) }) %>% unlist)[1:1401]
  ) %>% 
  dplyr::group_by(time_unit) %>%
  dplyr::summarize_all(mean) 

library(ggplot2)
bt2_with_unit %>%
  tidyr::gather(region, proportion, -time_unit) %>%
  ggplot() +
    geom_col(aes(x = as.factor(time_unit), y = proportion)) +
    facet_wrap(~region)

bt2 <- bt2_with_unit %>%
  dplyr::select(-time_unit)

###
  
bt_ts <- ts(bt, start = -2200, end = -800, frequency = 1)

ts.plot(bt_ts, type = "l")

adf <- lapply(
  bt_ts,
  function(x) {
    broom::tidy(tseries::adf.test(x))
  }
) %>%
  dplyr::bind_rows(.id = "region")

adf_diff <- lapply(
  bt_ts,
  function(x) {
    broom::tidy(tseries::adf.test(diff(x)))
  }
) %>%
  dplyr::bind_rows(.id = "region")

regions <- proportion_development_burial_type$region_name %>% unique()
forms <- expand.grid(regions, regions) %>%
  dplyr::mutate(
    form = paste0(Var1, "~", Var2)
  ) %$%
  form 

dynlm_results <- lapply(
  forms,
  function(x) {
    dynlm::dynlm(as.formula(x), data = bt_ts)
  }
)

adf_residuals <- lapply(
  residuals,
  function(x) {
    tseries::adf.test(x)
  }
)

residuals <- lapply(
  dynlm_results,
  function(x) {
    resid(x)
  }
)

###

diffi <- sapply(bt2, diff)

varfit <- vars::VAR(diffi, type = "both", p = 1)

summary(varfit)

acf(residuals(varfit)[,1])
acf(residuals(varfit)[,2])
acf(residuals(varfit)[,3])
acf(residuals(varfit)[,4])
acf(residuals(varfit)[,5])
acf(residuals(varfit)[,6])
acf(residuals(varfit)[,7])
acf(residuals(varfit)[,8])

acf(residuals(varfit))


impresp <- vars::irf(varfit)

plot(impresp)

plot(vars::fevd(varfit), col = terrain.colors(8))

causality(varfit, cause = "Austria.and.Czechia")
