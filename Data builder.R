library(readr)
library(dplyr)

## Import the various data sets ####

## 1. Climate data
# All weather station readings from 2014-2024
SMHI_combined <- read_csv("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SMHI/SMHI_combined.csv")

# All weather station names
info_SMHI_Weather_stations <- read_delim("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SMHI/info SMHI Weather stations.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
## 2. RASE data
# Skogsstyrelesens data from 2015-2023
RASE_sks <- read_delim("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/AFO data.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                           encoding = "WINDOWS-1252"), trim_ws = TRUE)
## Refine and combine data ####

# Merge both climte files on the basis of the station number column
climate_data <- SMHI_combined %>% left_join(info_SMHI_Weather_stations, by = c("weather_station" = "Stationnumber"))
names(climate_data)

# Something to do with RASE data
