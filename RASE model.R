# Pre-requisites
library(openxlsx)
library(tidyr)
library(dplyr)
library(stringr)

## Import data ####

# ABIN data
SKS_ABIN <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/SKS_ABIN.xlsx", 
                      sheet = "Data")

# ABIN data
Young_forest <- read.csv("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/YoungForest_prop_results.csv")

# SMHI data
Weather <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SMHI/MMA_full_weather_data_with_imputed_NAs_21_01_2025.xlsx")

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
      
    # Convert NAs to zero for bag numnbers
      Ungulate_data <- Ungulate_data %>%
      replace_na(list(Red100 = 0, Red1000 = 0, FD100 = 0, FD1000 = 0, Roe100 = 0, Roe1000 = 0, WB100 = 0, WB1000 = 0))
      
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

# Add young forest area to data set
Big_data <- Weather %>%
  left_join(Young_forest, by = c("Registreri" = "moose_area_id", "InvAr" = "year"))      
      
# Add weather data to big data set
Big_data <- Big_data %>%
  left_join(SKS_ABIN, by = c("Registreri", "InvAr"))
      
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

## Calculate and add ungulate index ####
# Calculate ungulate_index, replacing NA with 0
Big_data$ungulate_index <- (
  (Big_data$Roe1000/7.37) + 
  (Big_data$Red1000/2.07) + 
  (Big_data$FD1000/4.45)
)

## Model data selection model with big data ####

# Select the most relevant ecological variables for RASE per ha. (AntalRASEHa) and 
# % RASE at competative height (RASEAndelGynnsam)
RASE_data <- Big_data %>%
  dplyr::select(AntalRASEHa, RASEAndelGynnsam, # Independent variables 
                Älgtäthet.i.vinterstam, Roe1000, FD1000, Red1000, WB1000, ungulate_index, # Browsers
                youngforest_area_ha, proportion_young_forest, AndelBordigaMarker, BestHojdAllaAVG, BestandAlder, # Site
                AntalGranarHa, AntalTallarHa, AntalBjorkarHa, # Competitor species
                `Mean_seasonal_temp[c]_imputed`, `Mean_seasonal_precipitation[mm]_imputed`, `mean_seasonal_snowdepth[cm]_imputed`, # Climate
                InvAr, Registreri) # Random effects

# Rename variables for plotting later
dplyr::rename(
  Moose_density= Älgtäthet.i.vinterstam,
  Roe_deer_density= Roe1000,
  Fallow_deer_density = FD1000,
  Red_deer_density = Red1000,
  Wild_boar_density = WB1000,
  Ungulate_index = ungulate_index,
  Young_forest_Ha = youngforest_area_ha,
  Proportion_young_forest = proportion_young_forest,
  Propotion_on_fertile_soils = AndelBordigaMarker,
  AvgHeight = BestHojdAllaAVG,
  StandAge = BestandAlder,
  PineDensity = AntalGranarHa,
  SpruceDensity = AntalTallarHa,
  BirchDensity = AntalBjorkarHa,
  MeanTemp = `Mean_seasonal_temp[c]_imputed`,
  MeanPrecip = `Mean_seasonal_precipitation[mm]_imputed`,
  MeanSnowDepth = `mean_seasonal_snowdepth[cm]_imputed`
)

# View the first few rows to check the selection and renaming
head(RASE_data)


# Take data form 2018 onwwards when RASE per ha. (AntalRASEHa) is actually measureed
RASE_data_18_23 <- RASE_data %>%
  filter(InvAr %in% c(2018, 2019, 2020, 2021))

# See which variables have most NA and consider removing them
sort(colSums(is.na(RASE_data_18_23)), decreasing = TRUE)

# remove rows with NA values (need for model selection)
RASE_data_NA <- na.omit(RASE_data_18_23)

# Check for potential co-linearity
# Calculate correlation matrix
cor_matrix <- cor(RASE_data_NA[, c("Älgtäthet.i.vinterstam", "ungulate_index", "WB1000", "Roe1000", "FD1000", "Red1000", "WB1000", # Browsers
                                        "BestHojdAllaAVG", "BestandAlder", "AndelBordigaMarker", "youngforest_area_ha", "proportion_young_forest", # Site
                                        "AntalGranarHa", "AntalTallarHa", "AntalBjorkarHa", # Competitor species
                                        "Mean_seasonal_temp[c]_imputed", "Mean_seasonal_precipitation[mm]_imputed","mean_seasonal_snowdepth[cm]_imputed")], # Climate
                                    method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor <- cor_matrix
filtered_cor[abs(filtered_cor) <= 0.7 | abs(filtered_cor) == 1] <- NA

# View the filtered correlation matrix
filtered_cor

# Select the cliamte variables with heighest AIC
# Individual climate models
glm_RASE_Ha_temp <- glmer(AntalRASEHa ~ scale(`Mean_seasonal_temp[c]_imputed`) + (1 | InvAr) + (1 | Registreri), family = poisson, data = RASE_data_NA)
glm_RASE_Ha_precip <- glmer(AntalRASEHa ~ scale(`Mean_seasonal_precipitation[mm]_imputed`) + (1 | InvAr) + (1 | Registreri), family = poisson, data = RASE_data_NA)
glm_RASE_Ha_snow <- glmer(AntalRASEHa ~ scale(`mean_seasonal_snowdepth[cm]_imputed`) + (1 | InvAr) + (1 | Registreri), family = poisson, data = RASE_data_NA)

# Compare Models Using AIC
AIC(glm_RASE_Ha_temp, glm_RASE_Ha_precip, glm_RASE_Ha_snow)

## RASE per hectare ####
library(dplyr)
library(lme4)
library(MuMIn)
library(sjPlot)
library(ggplot2)

glm_RASE_Ha <- glmer(AntalRASEHa ~ scale(Älgtäthet.i.vinterstam) + scale(FD1000) + scale(WB1000) +  
                       scale(AntalTallarHa) + scale(AntalBjorkarHa) + 
                       scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) + 
                       scale(`mean_seasonal_snowdepth[cm]_imputed`) + scale(`Mean_seasonal_precipitation[mm]_imputed`)+
                       (1 | InvAr) + (1 | Registreri), family = poisson, data = RASE_data_NA)

summary(glm_RASE_Ha)

# Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_glm_RASE <- dredge(glm_RASE_Ha)
summary(dredged_glm_RASE)

# Get the best model (rank 1)
best_glm_RASE <- get.models(dredged_glm_RASE, subset = 1)[[1]]
summary(best_glm_RASE)

# Plot fixed effects from the GLMM
# Extract coefficients for the fixed effects
coef_glm <- summary(glm_RASE_Ha)$coefficients
fixed_effects_glm <- data.frame(
  Term = rownames(coef_glm),
  Estimate = coef_glm[, "Estimate"],
  SE = coef_glm[, "Std. Error"],
  z_value = coef_glm[, "z value"],
  p_value = coef_glm[, "Pr(>|z|)"]
)

# Remove the intercept term from the data
fixed_effects_glm <- fixed_effects_glm[fixed_effects_glm$Term != "(Intercept)", ]

# Add significance markers based on p-values
fixed_effects_glm$Significance <- case_when(
  fixed_effects_glm$p_value < 0.001 ~ "***",
  fixed_effects_glm$p_value < 0.01  ~ "**",
  fixed_effects_glm$p_value < 0.05  ~ "*",
  TRUE ~ ""
)

# Plot with ggplot2
ggplot(fixed_effects_glm, aes(x = Term, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "solid", size = 1.2, color = "black") +  # Add thick line at 0
  geom_text(aes(label = Significance), vjust = -1, size = 5) +  # Add significance asterisks
  coord_flip() +  # To flip the x-axis for better readability
  theme_minimal() +
  labs(title = "Fixed Effects from GLMM (Poisson)", y = "Estimate") +
  theme(axis.text.x = element_text(size = 10))

## RASE at competitive height ####
library(dplyr)
library(betareg)
library(MuMIn)
library(ggplot2)

RASE_competative_betar <- betareg(RASEAndelGynnsam ~ scale(Älgtäthet.i.vinterstam) + scale(FD1000) + scale(WB1000) +  
                                    scale(AntalTallarHa) + scale(AntalBjorkarHa) + 
                                    scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) + 
                                    scale(`mean_seasonal_snowdepth[cm]_imputed`) + scale(`Mean_seasonal_precipitation[mm]_imputed`), 
                                  data = Big_data, na.action = na.exclude)

summary(RASE_competative_betar)

# Plot fixed effects from the GLMM
plot_model(RASE_competative_betar, type = "est", show.values = TRUE, show.p = TRUE)

# Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_RASEbetar <- dredge(RASE_competative_betar)
summary(dredged_RASEbetar)

# Get the best model (rank 1)
best_RASEbetar <- get.models(dredged_RASEbetar, subset = 1)[[1]]
summary(best_RASEbetar)

# Plot the model
# Extract coefficients for the 'mu' (mean) model
coef_mu <- summary(RASE_competative_betar)$coefficients$mu
fixed_effects <- data.frame(
  Term = rownames(coef_mu),
  Estimate = coef_mu[, "Estimate"],
  SE = coef_mu[, "Std. Error"],
  p_value = coef_mu[, "Pr(>|z|)"]
)

# Remove the intercept term from the data
fixed_effects <- fixed_effects[fixed_effects$Term != "(Intercept)", ]

# Add significance markers based on p-values
fixed_effects$Significance <- case_when(
  fixed_effects$p_value < 0.001 ~ "***",
  fixed_effects$p_value < 0.01  ~ "**",
  fixed_effects$p_value < 0.05  ~ "*",
  TRUE ~ ""
)

# Plot with ggplot2
library(ggplot2)
ggplot(fixed_effects, aes(x = Term, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "solid", size = 1.2, color = "black") +  # Add thick line at 0
  geom_text(aes(label = Significance), vjust = -1, size = 5) +  # Add significance asterisks
  coord_flip() +  # To flip the x-axis for better readability
  theme_minimal() +
  labs(title = "Fixed Effects from Beta Regression", y = "Estimate") +
  theme(axis.text.x = element_text(size = 10))

