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


## betareg with big data ####
library(betareg)

# RASE per hectare
RASE_Ha_model <- betareg(AntalRASEHa ~ scale(varaible1) + scale(varaible2) + scale(varaible3) + 
                           scale(varaible4) + scale(varaible5) + scale(varaible6) +
                           scale(mvaraible7), data = Big_data, na.action=na.exclude)

# RASE at competitive height
RASE_competative_model <- betareg(RASEAndelGynnsam ~ scale(varaible1) + scale(varaible2) + scale(varaible3) + 
                        scale(varaible4) + scale(varaible5) + scale(varaible6) +
                        scale(mvaraible7), data = Big_data, na.action=na.exclude)
