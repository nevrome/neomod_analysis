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
  tidyr::spread(region_name, proportion)
  
gdp <- ts(bt, start=c(1970,1), end=c(2000,4), frequency=4)
