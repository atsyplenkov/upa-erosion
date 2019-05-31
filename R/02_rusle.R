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
library(extrafont)

source("https://raw.githubusercontent.com/atsyplenkov/caucasus-sediment-yield/master/R/00_own-functions.R")


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
# Read meteo archive from 1966  to 2017-----
# WMO ID: 27814
# Source: http://aisori.meteo.ru/ClimateR
meteo <- read.csv("data/raw/SCH91.txt",
                  sep = ";",
                  header = F,
                  dec = ".")

meteo <- meteo[c(2:4, 8, 12)]
colnames(meteo) <- c("year", "month", "day", "TTT", "RRR")

meteo %<>%  
  mutate(date = glue::glue("{year}-{month}-{day}"),
         date = as.character(date),
         date = as.Date(strptime(date, "%Y-%m-%d")),
         year = lubridate::floor_date(date, "year"),
         year = str_extract(as.character(year), "\\d{1,4}"),
         year = as.integer(year)) %>% 
  dplyr::select(date, year, RRR, TTT) %>% 
  as_tibble() 

meteo %<>% 
  filter(TTT > 2, RRR >= 1) %>% 
  group_by(year) %>% 
  summarise(P = sum(RRR),
            n = n(),
            SDII = P/n,
            logR = -0.5 + 0.266 * log10(P) + 3.1 * log10(SDII) - 0.131 * log10(240),
            R = 10^logR)

meteo %>% 
  dplyr::select(P, R, year) %>% 
  gather(variable, value, -year) %>% 
  ggplot(aes(y = value, x = year)) +
  geom_line(aes(color = variable),
            size = .8,
            alpha = .8,
            na.rm = T) +
  geom_smooth(aes(color = variable),
              method = "lm", se = F, linetype = "dashed") +
  geom_vline(xintercept = 1986,
             linetype = "dashed") +
  annotate("text", x = 1983, y = 620, label = "1986 г.", vjust = -0.2) +
  geom_curve(aes(x = 1983, y = 620, xend = 1985.5, yend = 500),
             curvature = 0.2,
             size = 0.2,
             arrow = arrow(
               length = unit(0.01, "npc")
             )) +
  labs(x = "") +
  ggsci::scale_color_lancet(name = "",
                            labels = c("Годовая сумма осадков",
                                      "Эрозионный Потенциал Осадков")) +
  scale_x_continuous(breaks = seq(1963, 2018, 10)) +
  scale_y_continuous(name = "Годовая суммая осадков, мм",
                     sec.axis = sec_axis(~.,
                                         name = expression(italic("ЭПО")*~"МДж"%.%"мм"%.%"ч"^"-1"%.%"га"^"-1"%.%"год"^"-1"))) +
  theme_clean() -> upa_meteo

ggsave("figures/upa_meteo-graph.png",
       upa_meteo, dpi = 500,
       w = 8, h = 5)  

# 6) OSM data
rivers <- st_read("/WORK/00_GLOBAL/OSM/Central_may2019/gis_osm_waterways_free_1.shp")

rivers %<>% 
  st_intersection(st_transform(upa, projection(.))) %>% 
  st_transform(32637) %>% 
  st_buffer(10)

lakes <- st_read("/WORK/00_GLOBAL/OSM/Central_may2019/gis_osm_water_a_free_1.shp")

lakes %<>% 
  st_intersection(st_transform(upa, projection(.))) %>% 
  st_transform(32637)

water <- rbind(lakes, rivers %>% dplyr::select(-width)) %>% 
  st_union()

st_write(water, "data/tidy/osm_water.shp", delete_dsn = T)
