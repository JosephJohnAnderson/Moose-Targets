# Pre-requisites
library(openxlsx)
library(tidyr)
library(dplyr)
library(stringr)

## Import data ####

# ABIN data
SKS_ABIN <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/SKS_ABIN.xlsx", 
                      sheet = "Data")

# SMHI data
Weather <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SMHI/MMA_full_weather_data_13_01_2025.xlsx")

    # Set InvAr to numeric
      Weather$InvAr <- as.numeric(Weather$InvAr)

# Moose density
Moose_density <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Moose/kjell_moose_data_250105.xlsx")

    # Mutate Moose_density "ÄFO-id" column
      Moose_density <- Moose_density %>%
      mutate(Registreri = str_sub(`ÄFO-id`, start = 5))

# Wolf predation
Wolf_predation <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Moose/Täthet vinterstam 2013-2024, kompletterad, KLrev.xlsx", 
                            sheet = "Täthet vinterstam")

    # Extract the 2nd and 17th columns as a data frame
      Wolf_predation <- as.data.frame(Wolf_predation[, c(2, 5, 17)])

    # Rename columns
      colnames(Wolf_predation) <- c("Registreri", "InvAr", "Wolf_predation")

Wolf_predation$Registreri <- substr(Wolf_predation$Registreri, start = 5, stop = nchar(Wolf_predation$Registreri))


# Bear observations
Bear_obs <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/BearObs.xlsx",)

    # Mutate Moose_density "ÄFO-id" column
      Bear_obs <- Bear_obs %>%
      mutate(Registreri = str_sub(`ÄFO`, start = 5))

# Other ungulates
Ungulate_data <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/HuntingBags/RoeFallowRedWildBAllYear.xlsx", sheet = "AllYears")

    # Mutate ungulate_data "MMA" column
      Ungulate_data <- Ungulate_data %>%
      mutate(Registreri = str_sub(`MMA`))
      
    # Convert Red100 to numeric
      Ungulate_data <- Ungulate_data %>%
      mutate(Red100 = as.numeric(Red100))

    # Convert Red1000 to numeric
      Ungulate_data <- Ungulate_data %>%
      mutate(Red1000 = as.numeric(Red1000))
      
# Calves per female      
Calves_per_female <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Calves per female and proportion of males 2013-2023/Kalv per hondjur och Andel tjur 2013-2023 Nuvarande gränser.xlsx", sheet = "Kalv per hondjur")

    # Pivot dataframe to have year data as column with corresponding measurement as separate columns
      Calves_per_female <- Calves_per_female %>%
      pivot_longer(cols = starts_with("Kalvar"),  # Columns with years
               names_to = "Year",         # New column for year
               values_to = "calves_per_female") # New column for measurement values


    # Mutate ungulate_data "Älgförvaltningsområde" column
      Calves_per_female$Registreri <- substr(Calves_per_female$Älgförvaltningsområde, start = 5, stop = nchar(Calves_per_female$Älgförvaltningsområde))
      Calves_per_female$Year <- substr(Calves_per_female$Year, start = 20, stop = nchar(Calves_per_female$Year))
      
    # Convert Year to numeric  
      Calves_per_female <- Calves_per_female %>%
      mutate(Year = as.numeric(Year))
      
# Sex ratio
Males_proportion <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Calves per female and proportion of males 2013-2023/Kalv per hondjur och Andel tjur 2013-2023 Nuvarande gränser.xlsx", sheet = "Andel tjur")

    # Pivot dataframe to have year data as column with corresponding measurement as separate columns
      Males_proportion <- Males_proportion %>%
      pivot_longer(cols = starts_with("Andel"),  # Columns with years
               names_to = "Year",         # New column for year
               values_to = "Males_proportion") # New column for measurement values
      as.numeric(Males_proportion$Year)

    # Mutate "Älgförvaltningsområde" column
      Males_proportion$Registreri <- substr(Males_proportion$Älgförvaltningsområde, start = 5, stop = nchar(Males_proportion$Älgförvaltningsområde))
      Males_proportion$Year <- substr(Males_proportion$Year, start = 24, stop = nchar(Males_proportion$Year))
  
    # Convert Year to numeric
      Males_proportion <- Males_proportion %>%
      mutate(Year = as.numeric(Year))
      
      
# Calf weights
# data in wrong format for big data, would need one one average weighted values per AFO  to be merged with rest of data  
Calf_weights <- read.csv("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Calf weights/CalfWeights_AFO_Season_MeanWeight_Count_WeightedWeight20241217.csv")
  
    # Mutate "InvAr" column
      Calf_weights <- Calf_weights %>%
      mutate(InvAr = str_sub(season, start = 6)) # use for season end year
    
    # Convert "InvAr" to numeric
      Calf_weights <- Calf_weights %>%
      mutate(InvAr = as.numeric(InvAr))
     
      # Calf_weights <- Calf_weights %>%
      # mutate(InvAr = str_sub(season, end = 4)) # use for season start year
      
    # Mutate Calf_weights "Area_ID" column
      Calf_weights <- Calf_weights %>%
      mutate(Registreri = str_sub(`Area_ID`))
     

## Join data ####

# Add weather data to big data set
Big_data <- SKS_ABIN %>%
  left_join(Weather, by = c("Registreri", "InvAr"))

# Add moose densities
Big_data <- Big_data %>%
  left_join(Moose_density, by = c("Registreri", "InvAr" = "ÄBINår"))

# Add wolf predation
Big_data <- Big_data %>%
  left_join(Wolf_predation, by = c("Registreri", "InvAr"))

# Add bear observations 
Big_data <- Big_data %>%
  left_join(Bear_obs, by = c("Registreri", "InvAr" = "År"))

# Add calves weights
Big_data <- Big_data %>%
  left_join(Calf_weights, by = c("Registreri", "InvAr"))

# Add calves per female
Big_data <- Big_data %>%
  left_join(Calves_per_female, by = c("Registreri", "InvAr" = "Year"))

# Add "sex ratio"
Big_data <- Big_data %>%
  left_join(Males_proportion, by = c("Registreri", "InvAr" = "Year"))

# Add other ungulates
Big_data <- Big_data %>%
  left_join(Ungulate_data, by = c("Registreri", "InvAr" = "Year"))

## Generalised linear mixed model with big data ####
library(lme4)
library(betareg)
library(MuMIn)

# Select the most relevant ecological variables for RASE per ha. (AntalRASEHa) and 
# % RASE at competative height (RASEAndelGynnsam)
RASE_data <- Big_data %>%
  dplyr::select(AntalRASEHa, RASEAndelGynnsam, Älgtäthet.i.vinterstam, Roe1000, FD1000, Red1000,
                AntalGranarHa, AntalTallarHa, AndelMargraMarker, 
                `Mean_seasonal_temp[c]`, `Mean_seasonal_precipitation[mm]`, `mean_seasonal_snowdepth[cm]`,
                BestHojdAllaAVG, BestandAlder, InvAr, Registreri)

# remove rows with NA values (need for model selection)
RASE_data_clean <- na.omit(RASE_data)


# Check for potential co-linearity
# Calculate correlation matrix
cor_matrix <- cor(RASE_data_clean[, c("Älgtäthet.i.vinterstam", "Roe1000", "FD1000", "Red1000", 
                                        "AntalGranarHa", "AntalTallarHa", "AndelMargraMarker", 
                                        "Mean_seasonal_temp[c]", "Mean_seasonal_precipitation[mm]", "mean_seasonal_snowdepth[cm]",
                                        "BestHojdAllaAVG", "BestandAlder")], method = "pearson")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor <- cor_matrix
filtered_cor[abs(filtered_cor) <= 0.7 | abs(filtered_cor) == 1] <- NA

# View the filtered correlation matrix
filtered_cor

# RASE per hectare 
RASE_Ha_glm <- glmer(AntalRASEHa ~ scale(Älgtäthet.i.vinterstam) + scale(Roe1000) + scale(FD1000) +
                          scale(AntalTallarHa) + scale(BestHojdAllaAVG) + scale(AndelMargraMarker) +
                          scale(`Mean_seasonal_precipitation[mm]`) + (1 | InvAr) + (1 | Registreri), 
                        family = poisson, data = RASE_data, na.action = na.exclude)

summary(RASE_Ha_glm)

# Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_RASEglm <- dredge(RASE_Ha_glm)
summary(dredged_RASEglm)

# Get the best model (rank 1)
best_RASEglm <- get.models(dredged_RASEglm, 1)
summary(best_RASEglm)










# RASE at competitive height
library(betareg)
RASE_competative_betar <- betareg(RASEAndelGynnsam ~ scale(AntalTallarHa) + scale(AntalGranarHa) + 
                                    scale(Älgtäthet.i.vinterstam) +  scale(Roe1000) + 
                                    scale(FD1000) +  scale(AndelMargraMarker) + 
                                    scale(`Mean_seasonal_temp[c]`), 
                                  data = Big_data, na.action = na.exclude)

summary(RASE_competative_betar)

# Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_RASEbetar <- dredge(RASE_competative_betar)
summary(dredged_RASEbetar)

# Get the best model (rank 1)
best_RASEbetar <- get.models(dredged_RASEbetar, 1)
summary(best_RASEbetar)

## Beta regression ####
library(betareg)

# Create a version of RASE_data for rescaled variables
RASE_data_beta <- RASE_data

# Use a min-max normalisation to rescale each variable
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply to all tested variables
#Create list of all varaibles
variables_to_normalize <- c("RASEAndelGynnsam", "AntalRASEHa", "AntalTallarHa", "AntalGranarHa", 
                            "Älgtäthet.i.vinterstam", "Roe1000", "FD1000", 
                            "AndelMargraMarker", "Mean_seasonal_temp[c]")

# Then normalise them
RASE_data_beta[variables_to_normalize] <- lapply(RASE_data_beta[variables_to_normalize], normalize)

# After normalization, ensure no variables contain exact 0 or 1, especially the response variable
RASE_data_beta$AntalRASEHa <- (RASE_data_beta$AntalRASEHa * (nrow(RASE_data_beta) - 1) + 0.5) / nrow(RASE_data_beta)

RASE_data_beta$RASEAndelGynnsam <- (RASE_data_beta$AntalRASEAndelGynnsam * (nrow(RASE_data_beta) - 1) + 0.5) / nrow(RASE_data_beta)

# Run beta regressions
# RASE per hectare 
RASE_Ha_betar <- betareg(AntalRASEHa ~ AntalTallarHa + AntalGranarHa + 
                           Älgtäthet.i.vinterstam + Roe1000 + 
                           FD1000 + AndelMargraMarker + 
                           `Mean_seasonal_temp[c]`, 
                         data = RASE_data_beta, na.action = na.exclude)

summary(RASE_Ha_betar)

# Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_RASEglm <- dredge(RASE_Ha_glm)
summary(dredged_RASEglm)

# Get the best model (rank 1)
best_RASEglm <- get.models(dredged_RASEglm, 1)
summary(best_RASEglm)