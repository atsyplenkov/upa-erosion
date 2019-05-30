##################################################################################
#
# Soil Erosion modelling
# Upa river
#
# Part 2. RUSLE
# Anatoly Tsyplenkov
# atsyplenkov@gmail.com
#
##################################################################################

Sys.setlocale("LC_ALL", "Russian_Russia")

library(tidyverse)
library(magrittr)
library(sf)
library(elevatr)
library(spgrass6)
library(raster)

# 1) Basin
c_factor <- st_read("data/spatial/lulc/Landuse1985_2/Landuse_1985_var2.shp")

# Create watershed
# c_factor %>% 
#   st_buffer(dist = 0) %>% 
#   st_union() -> upa
# 
# upa %<>%
#   st_transform(32637)
# 
# # Save as shapefile
# st_write(upa, "data/tidy/upa_basin.shp", delete_dsn = T)

upa <- st_read("data/tidy/upa_basin.shp")

# 3) C-factor
# Define C-factor
c_factor %>%
  mutate(area = st_area(geometry)) %>% 
  as.data.frame() %>% 
  mutate(area = as.numeric(area) / 10^6) %>% 
  group_by(Code) %>% 
  summarise(area = sum(area)) %>% 
  mutate(area_p = area * 100 / (as.numeric(st_area(upa))/10^6),
         C = case_when(
           Code == 1 ~ 0,
           Code == 2 ~ 0.003,
           Code == 3 ~ 0.15,
           Code == 4 ~ 0.05,
           Code == 5 ~ 0.03,
           Code == 6 ~ 0
         )) -> C_summary

c_factor %<>% 
  mutate(C = case_when(
    Code == 1 ~ 0,
    Code == 2 ~ 0.003,
    Code == 3 ~ 0.15,
    Code == 4 ~ 0.05,
    Code == 5 ~ 0.03,
    Code == 6 ~ 0
  ))

# 4) Soil
## Harmonized World Soil Database
dsvm <- st_read("/WORK/00_GLOBAL/DSVM/DSMW.shp") %>% 
  st_set_crs(4326)

letters_only <- function(x) !grepl("[^A-Za-z]", x)

dsvm_k <- readxl::read_xlsx("/WORK/00_GLOBAL/DSVM/Generalized_SU_Info.xlsx") %>% 
  rename(DOMSOI = 1) %>% 
  filter(letters_only(DOMSOI)) %>%  
  mutate(DOMSOI = stringr::str_to_title(DOMSOI))

dsvm %<>%
  st_buffer(0) %>% 
  st_intersection(st_transform(upa, 4326)) %>% 
  st_transform(32637)

# Add Soil info
dsvm %<>% 
  left_join(dsvm_k, by = "DOMSOI")

## Russinan soil database
egrpr <- st_read("/WORK/00_GLOBAL/soil_map_M2_5-1.0/soil_map_M2_5-1.0.shp")

egrpr %<>%
  st_buffer(0) %>% 
  st_intersection(st_transform(upa, 4326)) %>% 
  st_transform(32637)

egrpr_t <- readxl::read_xls("/WORK/00_GLOBAL/soil_map_M2_5-1.0/soil_map_M2_5_legend-1.0.xls") %>% 
  rename(SOIL0 = 1)

egrpr %<>% 
  left_join(egrpr_t, by = "SOIL0")

egrpr %<>% 
  mutate(area = st_area(.),
         area = signif(as.numeric(area) / 10^6, 2),
         proc = signif(area*100/1349.2, 2),
         K_usle = case_when(
           Descript == "Темно-серые лесные" ~ 0.164,
           Descript == "Лугово-черноземные выщелоченные" ~ 0.319,
           TRUE ~ 0.154
         ))

# Save as shapefile
st_write(egrpr, "data/tidy/upa_K.shp", delete_dsn = T)

# 5) R-factor
glored <- raster("/WORK/00_GLOBAL/GlobalR/GlobalR_NoPol.tif")

R_upa <- raster::mask(glored, 
                      upa %>% 
                        st_transform(projection(glored)) %>% 
                        as_Spatial())

# Select Meteostations
sf::st_bbox(st_transform(upa, 4326))

library(worldmet)
getMeta(country = "RS")

# Download meteo data
tula <- importNOAA(code = "277190-99999",
                   precip = T,
                   year = 1959:2019,
                   parallel = 2)

kaluga <- importNOAA(code = "277030-99999",
                     precip = T,
                     year = 1932:2019,
                     parallel = 2)

UZLOVAJA <- importNOAA(code = "278210-99999",
                       precip = T,
                       year = 2003:2019,
                       parallel = 2)

MCENSK <- importNOAA(code = "278170-99999",
                     precip = T,
                     year = 1991:2019,
                     parallel = 2)

efremov <- importNOAA(code = "279210-99999",
                     precip = T,
                     year = 1959:2019,
                     parallel = 2)

SUHINICHI <- importNOAA(code = "277070-99999",
                        precip = T,
                        year = 1959:2019,
                        parallel = 2)

kaluga %>% 
  mutate(year = year(date)) %>% 
  filter(year > 1963) %>% 
  group_by(year) %>% 
  summarise(sum(precip, na.rm = T)) %>% View
