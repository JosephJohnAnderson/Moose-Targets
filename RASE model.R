# Load the other datasets
library(readxl)
SKS_ABIN <- read_excel("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/SKS_ABIN.xlsx", 
                       sheet = "Data")

MMA_weather <- read_excel("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SMHI/AFO_weather_data.xlsx")

Moose_density <- read_excel("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Moose/Täthet vinterstam 2013-2024, med missing data för enstaka år i främst vargområden, kompletterad.xlsx", 
                            sheet = "Täthet vinterstam")

## Combine the datasets ####

# Mutate MMA_weather "winter_season" column
library(dplyr)
library(stringr)
MMA_weather <- MMA_weather %>%
  mutate(InvAr = str_sub(winter_season, start = 6))

# Mutate MMA_weather "ÄFO-id" column
Moose_density <- Moose_density %>%
  mutate(Registreri = str_sub(`ÄFO-id`, start = 5))

# Set InvAr to numeric
MMA_weather$InvAr <- as.numeric(MMA_weather$InvAr)

# Add weather data to big data set
Big_data <- SKS_ABIN %>%
  left_join(MMA_weather, by = c("Registreri", "InvAr"))

# Add moose densities to big data set
Big_data <- Big_data %>%
  left_join(Moose_density, by = c("Registreri", "InvAr" = "År"))


## GLMM with big data ####
library(lme4)
glmm_model <- lmer(`mean_seasonal_snowdepth[cm]` ~ ArsskadaTallAndel + (1 | Registreri), 
                   data = Big_data)