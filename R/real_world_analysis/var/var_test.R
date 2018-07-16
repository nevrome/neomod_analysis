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
  
bt_ts <- ts(bt, start = -2200, end = -800, frequency = 1)

ts.plot(bt_ts, type="l")

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
    adf.test()
  }
)

residuals <- lapply(
  dynlm_results,
  function(x) {
    resid(x)
  }
)

###

diffi <- sapply(bt, diff)

varfit <- vars::VAR(bt, p = 1)

summary(varfit)

impresp <- vars::irf(varfit)

plot(impresp)

plot(vars::fevd(varfit))

causality(varfit, cause = "Benelux")
