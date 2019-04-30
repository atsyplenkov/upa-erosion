##################################################################################
#
# Soil Erosion modelling
# Upa river
#
# Part 1. Data preparation
# Anatoly Tsyplenkov
# atsyplenkov@gmail.com
#
##################################################################################

Sys.setlocale("LC_ALL", "Russian_Russia")

library(tidyverse)
library(rvest)
library(magrittr)
library(XML)
library(sf)

# 1) Get meteostations positions
url <- "http://meteomaps.ru/meteostation_codes.html"
url %>%
  readHTMLTable(.,
                header = T,
                which = 1, 
                stringsAsFactors = F,) -> meteo

meteo %<>%
  as_tibble() %>% 
  set_colnames(c("wmo_id", "station", "lat", "lon", "H", "country")) %>% 
  mutate_at(vars(lat, lon, H),
            list(~as.numeric(sub(",", ".", ., fixed = TRUE))))

