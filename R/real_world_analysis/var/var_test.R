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

bt <- proportion_development_burial_type %>%
  dplyr::filter(idea == "cremation") %>%
  dplyr::select(-idea) %>%
  tidyr::spread(region_name, proportion) %>%
  dplyr::select(-timestep) 
  
bt_ts <- ts(bt, start = -2200, end = -800, frequency = 1)

ts.plot(bt_ts, type="l")

lapply(
  bt_ts,
  function(x) {
    broom::tidy(tseries::adf.test(x))
  }
) %>%
  dplyr::bind_rows(.id = "region")

sapply(bt, diff)
