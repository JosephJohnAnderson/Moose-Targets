# Load the other datasets
library(readxl)
SKS_ABIN <- read_excel("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/AFO data corrected.xlsx", 
                       sheet = "Data")

MMA_weather <- read_excel("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SMHI/AFO_weather_data.xlsx")

# Mutate MMA_weather "winter_season" column
library(dplyr)
library(stringr)
MMA_weather <- MMA_weather %>%
  mutate(InvAr = str_sub(winter_season, start = 6))

# Set InvAr to numeric
MMA_weather$InvAr <- as.numeric(MMA_weather$InvAr)

# Associate data sheets based on "Registreri" and "InvAr"

Big_data <- SKS_ABIN %>%
  left_join(MMA_weather, by = c("Registreri", "InvAr"))

# Create the base plot
plot(Big_data$`mean_seasonal_snowdepth[cm]` ~ Big_data$ArsskadaTallAndel,
     xlab = "ArsskadaTallAndel",
     ylab = "Mean Seasonal Snow Depth [cm]",
     main = "Relationship between Snow Depth and Damage Proportion")

# Fit the GLMM
library(lme4)
glmm_model <- lmer(`mean_seasonal_snowdepth[cm]` ~ ArsskadaTallAndel + (1 | Registreri), 
                   data = Big_data)