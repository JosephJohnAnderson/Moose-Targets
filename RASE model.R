# Pre-requisites
library(openxlsx)
library(tidyr)
library(dplyr)
library(stringr)

## Import data ####

# ABIN data
SKS_ABIN <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/SKS_ABIN.xlsx", 
                      sheet = "Data")

# Young forest data
Young_forest <- read.csv("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/YoungForest_prop_results.csv")

# Mutate Young_forest "moose_area_id" column
Young_forest <- Young_forest %>%
  mutate(Registreri = str_sub(`moose_area_id`))

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

# Add a new column with year + 1
Ungulate_data$InvAr <- Ungulate_data$Year + 1

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

# Add ÄBIN and weather data to big data set
Big_data <- SKS_ABIN %>%
  left_join(Weather, by = c("Registreri", "InvAr"))

# Add young forest area to data set
Big_data <- Big_data %>%
  left_join(Young_forest, by = c("Registreri" = "moose_area_id", "InvAr" = "year"))      

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
  left_join(Ungulate_data, by = c("Registreri", "InvAr"))

## Calculate and add ungulate index ####
# Calculate ungulate_index, replacing NA with 0
Big_data$ungulate_index <- (
  (Big_data$Roe1000/7.37) + 
    (Big_data$Red1000/2.07) + 
    (Big_data$FD1000/4.45)
)


## Model data selection model with big data ####
library(lme4)

# Select the most relevant ecological variables for RASE per ha. (AntalRASEHa) and 
# % RASE at competative height (RASEAndelGynnsam)
RASE_data <- Big_data %>%
  dplyr::select(LandsdelNamn,LanNamn, # Regional data
                AntalRASEHa, RASEAndelGynnsam, # Independent variables 
                Älgtäthet.i.vinterstam, Roe1000, FD1000, Red1000, WB1000, ungulate_index, # Browsers
                youngforest_area_ha, proportion_young_forest, AndelBordigaMarker, BestHojdAllaAVG, BestandAlder, # Site
                Medelbestandshojd, AndelRojt...18, # Site
                AntalGranarHa, AntalTallarHa, AntalBjorkarHa, AntalOvrigtHa, # Competitor species
                `Mean_seasonal_temp[c]`, `Mean_seasonal_precipitation[mm]`, `mean_seasonal_snowdepth[cm]`, # Climate
                InvAr, Registreri) # Random effects

# Take data form 2018 onwards when RASE per ha. (AntalRASEHa) is actually measured
RASE_data_18_24 <- RASE_data %>%
  filter(InvAr %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024))

# See which variables have most NA and consider removing them
sort(colSums(is.na(RASE_data_18_24)), decreasing = TRUE)

# remove rows with NA values (need for model selection)
RASE_data_NA <- na.omit(RASE_data_18_24)

# Check for potential co-linearity
# Calculate correlation matrix
cor_matrix <- cor(RASE_data_NA[, c("Älgtäthet.i.vinterstam", "ungulate_index", "WB1000", "Roe1000", "FD1000", "Red1000", "WB1000", # Browsers
                                        "BestHojdAllaAVG", "BestandAlder", "Medelbestandshojd", "AndelRojt...18", # Site
                                        "AndelBordigaMarker", "youngforest_area_ha", "proportion_young_forest", # Site
                                        "AntalGranarHa", "AntalTallarHa", "AntalBjorkarHa", "AntalOvrigtHa", # Competitor species
                                        "Mean_seasonal_temp[c]", "Mean_seasonal_precipitation[mm]","mean_seasonal_snowdepth[cm]")], # Climate
                                    method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor <- cor_matrix
filtered_cor[abs(filtered_cor) <= 0.7 | abs(filtered_cor) == 1] <- NA

# View the filtered correlation matrix
filtered_cor

        # Select the cliamte variables with heighest AIC
# Individual climate models
glm_RASE_Ha_temp <- glmer.nb(AntalRASEHa ~ scale(`Mean_seasonal_temp[c]`) + (1 | InvAr) + (1 | Registreri), data = RASE_data_NA)
glm_RASE_Ha_precip <- glmer.nb(AntalRASEHa ~ scale(`Mean_seasonal_precipitation[mm]`) + (1 | InvAr) + (1 | Registreri), data = RASE_data_NA)
glm_RASE_Ha_snow <- glmer.nb(AntalRASEHa ~ scale(`mean_seasonal_snowdepth[cm]`) + (1 | InvAr) + (1 | Registreri), data = RASE_data_NA)

# Compare Models Using AIC
AIC(glm_RASE_Ha_temp, glm_RASE_Ha_precip, glm_RASE_Ha_snow)

# Individual height model
glm_RASE_Ha_MBH <- glmer.nb(AntalRASEHa ~ scale(Medelbestandshojd) + (1 | InvAr) + (1 | Registreri), data = RASE_data_NA)
glm_RASE_Ha_BHAA <- glmer.nb(AntalRASEHa ~ scale(BestHojdAllaAVG) + (1 | InvAr) + (1 | Registreri), data = RASE_data_NA)

# Compare Models Using AIC
AIC(glm_RASE_Ha_MBH, glm_RASE_Ha_BHAA)

## RASE per hectare national ####
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(car)
library(DHARMa)

glm_RASE_Ha <- glmer.nb(AntalRASEHa ~ scale(Älgtäthet.i.vinterstam) + 
                          scale(ungulate_index) + 
                          scale(WB1000) +  
                          scale(AntalTallarHa) + 
                          scale(AntalBjorkarHa) + 
                          scale(AntalOvrigtHa) +
                          scale(proportion_young_forest) + 
                          scale(AndelBordigaMarker) + 
                          scale(youngforest_area_ha) +
                          scale(Medelbestandshojd) + 
                          scale(AndelRojt...18) + 
                          scale(BestandAlder) +
                          scale(`mean_seasonal_snowdepth[cm]`) + 
                          scale(`Mean_seasonal_precipitation[mm]`) +
                          (1 | Registreri) + (1 | InvAr), # Should I remove InvAr as random effect (is singular)?
                          data = RASE_data_NA)
summary(glm_RASE_Ha)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha)

# Check for over dispersal
glm_RASE_Ha_simres <- simulateResiduals(glm_RASE_Ha)
testDispersion(glm_RASE_Ha_simres)

# Use "drop1" to drop one predictor at a time and compare AIC values
drop1(glm_RASE_Ha, test = "Chisq")

# Create a reduced model from these results
glm_RASE_Ha_reduced <- glmer.nb(AntalRASEHa ~ 
                                  scale(AntalBjorkarHa) + # competators
                                  scale(youngforest_area_ha) + scale(Medelbestandshojd) + scale(AndelRojt...18) + scale(BestandAlder) + # forest
                                  scale(`mean_seasonal_snowdepth[cm]`) + #weather
                                  (1 | Registreri) + (1 | InvAr),  
                                data = RASE_data_NA)

summary(glm_RASE_Ha_reduced)

# Once happy with model type and variables Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_glm_RASE <- dredge(glm_RASE_Ha_reduced)
summary(dredged_glm_RASE)

# Select models within ΔAIC < 2 of the best model
top_GLM_RASE <- get.models(dredged_glm_RASE, subset = delta < 2)[[1]]
summary(top_GLM_RASE)

# Get the best model (rank 1)
best_glm_RASE <- get.models(dredged_glm_RASE, subset = 1)[[1]]
summary(best_glm_RASE)

# Plot fixed effects from the GLMM
# Extract coefficients for the fixed effects
coef_glm <- summary(best_glm_RASE)$coefficients
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

# Rename terms using recode (without !!!)
fixed_effects_glm$Term <- dplyr::recode(fixed_effects_glm$Term,
                                        "scale(Älgtäthet.i.vinterstam)" = "Moose Density",        
                                        "scale(FD1000)" = "Fallow Deer Density",
                                        "scale(WB1000)" = "Wild Boar Density",
                                        "scale(AntalTallarHa)" = "Number of Pine Trees per Ha",
                                        "scale(AntalBjorkarHa)" = "Number of Birch Trees per Ha",
                                        "scale(proportion_young_forest)" = "Proportion of Young Forest",
                                        "scale(AndelBordigaMarker)" = "Proportion on Productive Land",
                                        "scale(youngforest_area_ha)" = "Young Forest Area (Ha)",
                                        "scale(BestandAlder)" = "Stand Age",
                                        "scale(Medelbestandshojd)" = "Average stand height",
                                        "scale(AndelRojt...18)" = "Proportion PCT",
                                        "scale(`mean_seasonal_snowdepth[cm]`)" = "Mean Seasonal Snow Depth", 
                                        "scale(`Mean_seasonal_precipitation[mm]`)" = "Mean Seasonal Precipitation"
)


# Plot with ggplot2
RASE_Ha_plot <- ggplot(fixed_effects_glm, aes(x = Term, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
                geom_pointrange() +
                geom_hline(yintercept = 0, linetype = "solid", size = 1.2, color = "black") +  # Add thick line at 0
                geom_text(aes(label = Significance), vjust = -1, size = 5) +  # Add significance asterisks
                coord_flip() +  # To flip the x-axis for better readability
                theme_classic() +
                labs(title = "Fixed Effects on RASE per Ha.", y = "Estimate") +
                theme(axis.text.x = element_text(size = 10))

RASE_Ha_plot
  
# Export with ggsave 
ggsave("RASE_Ha_plot.tiff", plot = RASE_Ha_plot, device = NULL, 
       path = "~/GitHub/Moose-Targets/Plots", scale = 1, width = 14, 
       height = 14, dpi = 300, limitsize = TRUE, units = "cm")


## RASE per hectare regions ####

# Take RASE_data_NA and filter for regions

## Norrland
RASE_data_Norrland <- RASE_data_NA %>%
  filter(LandsdelNamn %in% c("Södra Norrland", "Norra Norrland"))

# Run the model
glm_RASE_Ha_N <- glmer.nb(AntalRASEHa ~ scale(Älgtäthet.i.vinterstam) + scale(WB1000) +  
                          scale(AntalTallarHa) + scale(AntalBjorkarHa) + 
                          scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) +
                          scale(Medelbestandshojd) + scale(AndelRojt...18) + scale(BestandAlder) +
                          scale(`mean_seasonal_snowdepth[cm]`) + scale(`Mean_seasonal_precipitation[mm]`) +
                          (1 | Registreri) + (1 | InvAr), # is singular?
                        data = RASE_data_Norrland)

summary(glm_RASE_Ha_N)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_N)

# Check for over dispersal
glm_RASE_Ha_N_simres <- simulateResiduals(glm_RASE_Ha_N)
testDispersion(glm_RASE_Ha_N_simres)

# Once happy with model type and variables Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_glm_RASE_ha_N <- dredge(glm_RASE_Ha_N)
summary(dredged_glm_RASE)

# Select models within ΔAIC < 2 of the best model
top_GLM_RASE <- subset(dredged_glm_RASE, delta < 2)
summary(top_GLM_RASE)

# Get the best model (rank 1)
best_glm_RASE <- get.models(dredged_glm_RASE, subset = 1)[[1]]
summary(best_glm_RASE)

## Svealand
RASE_data_Svealand <- RASE_data_NA %>%
  filter(LandsdelNamn %in% c("Svealand"))

glm_RASE_Ha_S <- glmer.nb(AntalRASEHa ~ scale(Älgtäthet.i.vinterstam) + scale(FD1000) + scale(WB1000) + scale(Roe1000) +
                            scale(AntalTallarHa) + scale(AntalBjorkarHa) + 
                            scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) +
                            scale(Medelbestandshojd) + scale(AndelRojt...18) + scale(BestandAlder) +
                            scale(`Mean_seasonal_precipitation[mm]`) +
                            (1 | Registreri) + (1 | InvAr), # is singular?
                          data = RASE_data_Svealand)

summary(glm_RASE_Ha_S)

## Götaland
RASE_data_Gotaland <- RASE_data_NA %>%
  filter(LandsdelNamn %in% c("Götaland"))

glm_RASE_Ha_G <- glmer.nb(AntalRASEHa ~ scale(Älgtäthet.i.vinterstam) + scale(FD1000) + scale(WB1000) + scale(Roe1000) + 
                            scale(AntalTallarHa) + scale(AntalBjorkarHa) + 
                            scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) +
                            scale(Medelbestandshojd) + scale(AndelRojt...18) + scale(BestandAlder) +
                            scale(`Mean_seasonal_precipitation[mm]`) +
                            (1 | Registreri) + (1 | InvAr), # is singular?
                          data = RASE_data_Gotaland)

summary(glm_RASE_Ha_G)

## RASE at competitive height national ####
library(dplyr)
library(glmmTMB)
library(MuMIn)
library(ggplot2)
library(sjPlot)

RASE_competative_betar <- glmmTMB(RASEAndelGynnsam ~ scale(Älgtäthet.i.vinterstam) + scale(ungulate_index) + scale(WB1000) +  
                                    scale(AntalTallarHa) + scale(AntalBjorkarHa) + scale(AntalOvrigtHa) +
                                    scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) +
                                    scale(Medelbestandshojd) + scale(AndelRojt...18) + scale(BestandAlder) +
                                    scale(`mean_seasonal_snowdepth[cm]`) + scale(`Mean_seasonal_precipitation[mm]`) +
                                    (1 | Registreri) + (1 | InvAr),
                                  family = beta_family(link = "logit"),
                                  data = RASE_data_NA)

summary(RASE_competative_betar)

# Use "drop1" to drop one predictor at a time and compare AIC values
drop1(RASE_competative_betar, test = "Chisq")

# Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_RASEbetar <- dredge(RASE_competative_betar)
summary(dredged_RASEbetar)

# Get the best model (rank 1)
best_RASEbetar <- get.models(dredged_RASEbetar, subset = 1)[[1]]
summary(best_RASEbetar)

plot_model(RASE_competative_betar, type = "est", show.values = TRUE, show.p = TRUE)


# Plot fixed effects from the GLMM
# Extract coefficients for the fixed effects
coef_glm <- summary(best_RASEbetar)$coefficients$cond
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

# Rename terms using recode (without !!!)
fixed_effects_glm$Term <- dplyr::recode(fixed_effects_glm$Term,
                                        "scale(Älgtäthet.i.vinterstam)" = "Moose Density",        
                                        "scale(FD1000)" = "Fallow Deer Density",
                                        "scale(WB1000)" = "Wild Boar Density",
                                        "scale(AntalTallarHa)" = "Number of Pine Trees per Ha",
                                        "scale(AntalBjorkarHa)" = "Number of Birch Trees per Ha",
                                        "scale(proportion_young_forest)" = "Proportion of Young Forest",
                                        "scale(AndelBordigaMarker)" = "Proportion on Productive Land",
                                        "scale(youngforest_area_ha)" = "Young Forest Area (Ha)",
                                        "scale(BestandAlder)" = "Stand Age",
                                        "scale(`mean_seasonal_snowdepth[cm]`)" = "Mean Seasonal Snow Depth", 
                                        "scale(`Mean_seasonal_precipitation[mm]`)" = "Mean Seasonal Precipitation"
)

# Plot with ggplot2
RASE_Gyn_plot <- ggplot(fixed_effects_glm, aes(x = Term, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "solid", size = 1.2, color = "black") +  # Add thick line at 0
  geom_text(aes(label = Significance), vjust = -1, size = 5) +  # Add significance asterisks
  coord_flip() +  # To flip the x-axis for better readability
  theme_classic() +
  labs(title = "Fixed Effects on competative RASE", y = "Estimate") +
  theme(axis.text.x = element_text(size = 10))

RASE_Gyn_plot

# Export with ggsave 
ggsave("RASE_Gyn_plot_dredge.tiff", plot = RASE_Gyn_plot, device = NULL, 
       path = "~/GitHub/Moose-Targets/Plots", scale = 1, width = 14, 
       height = 14, dpi = 300, limitsize = TRUE, units = "cm")

## RASE at competitive height regions ####

# Take RASE_data_NA and filter for regions

## Norrland
RASE_data_Norrland <- RASE_data_NA %>%
  filter(LandsdelNamn %in% c("Södra Norrland", "Norra Norrland"))

# Run the model
RASE_competative_betar_N <- glmmTMB(RASEAndelGynnsam ~ scale(Älgtäthet.i.vinterstam) + scale(FD1000) + scale(WB1000) +  
                                                     scale(AntalTallarHa) + scale(AntalBjorkarHa) + 
                                                     scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) + scale(BestandAlder) +
                                                     scale(`mean_seasonal_snowdepth[cm]`) + scale(`Mean_seasonal_precipitation[mm]`) +
                                                     (1 | Registreri) + (1 | InvAr), 
                                                   data = RASE_data_Norrland)

summary(RASE_competative_betar_N)

# Check variance inflation factor (VIF)
vif(RASE_competative_betar_N)

# Check for over dispersal
glm_RASE_Ha_N_simres <- simulateResiduals(RASE_competative_betar_N)
testDispersion(glm_RASE_Ha_N_simres)

# Once happy with model type and variables Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_glm_comp_ha_N <- dredge(RASE_competative_betar_N)
summary(dredged_glm_comp_ha_N)

# Select models within ΔAIC < 2 of the best model
top_GLM_RASE <- subset(dredged_glm_comp_ha_N, delta < 2)
summary(top_GLM_RASE)

# Get the best model (rank 1)
best_glm_RASE <- get.models(dredged_glm_comp_ha_N, subset = 1)[[1]]
summary(best_glm_RASE)

## Svealand
RASE_data_Svealand <- RASE_data_NA %>%
  filter(LandsdelNamn %in% c("Svealand"))

RASE_competative_betar_S <- glmmTMB(RASEAndelGynnsam ~ scale(Älgtäthet.i.vinterstam) + scale(FD1000) + scale(WB1000) +  
                                      scale(AntalTallarHa) + scale(AntalBjorkarHa) + 
                                      scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) + scale(BestandAlder) +
                                      scale(`mean_seasonal_snowdepth[cm]`) + scale(`Mean_seasonal_precipitation[mm]`) +
                                      (1 | Registreri) + (1 | InvAr), 
                                    data = RASE_data_Svealand)

summary(RASE_competative_betar_S)

## Götaland
RASE_data_Gotaland <- RASE_data_NA %>%
  filter(LandsdelNamn %in% c("Götaland"))

RASE_competative_betar_G <- glmmTMB(RASEAndelGynnsam ~ scale(Älgtäthet.i.vinterstam) + scale(FD1000) + scale(WB1000) +  
                                      scale(AntalTallarHa) + scale(AntalBjorkarHa) + 
                                      scale(proportion_young_forest) + scale(AndelBordigaMarker) + scale(youngforest_area_ha) + scale(BestandAlder) +
                                      scale(`mean_seasonal_snowdepth[cm]`) + scale(`Mean_seasonal_precipitation[mm]`) +
                                      (1 | Registreri) + (1 | InvAr), 
                                    data = RASE_data_Gotaland)

summary(RASE_competative_betar_G)
## Create summary table of all models ####
library(insight) # needed for sjPlot
library(sjPlot) # needed for tab_model for summary table of glmmTMB results

tab_model(final_model_national, final_model_norrland, final_model_svealand, final_model_gotaland,
          transform = NULL, 
          show.ci = FALSE, 
          show.se = TRUE, 
          show.aic = TRUE,
          show.stat = TRUE,
          #show.bic = TRUE,
          #show.icc = FALSE,
          file = "RASE_2018_2023_table.html")