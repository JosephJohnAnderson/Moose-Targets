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
Moose_density <- read.xlsx("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Moose/kjell_moose_data_corrected_250314.xlsx")

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

# Select the most relevant ecological variables for RASE per ha. (AntalRASEHa) and 
# % RASE at competative height (RASEAndelGynnsam)
RASE_data <- Big_data %>%
  dplyr::select(LandsdelNamn,LanNamn, # Regional data
                AntalRASEHa, RASEAndelGynnsam, # Independent variables 
                Älgtäthet.i.vinterstam, Roe1000, FD1000, Red1000, WB1000, ungulate_index, # Browsers
                youngforest_area_ha, proportion_young_forest, AndelBordigaMarker, BestHojdAllaAVG, BestandAlder, # Site
                Medelbestandshojd, AndelRojt...18, # Site
                AntalGranarHa, AntalTallarHa, AntalBjorkarHa, AntalOvrigtHa, # Competitor species
                `Mean_seasonal_temp[c]_imputed`, `Mean_seasonal_precipitation[mm]_imputed`, `mean_seasonal_snowdepth[cm]_imputed`, # Climate
                InvAr, Registreri) # Random effects

# Take data form 2018 onwards when RASE per ha. (AntalRASEHa) is actually measured
RASE_data_18_24 <- RASE_data %>%
  filter(InvAr %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024))

# See which variables have most NA and consider removing them
sort(colSums(is.na(RASE_data_18_24)), decreasing = TRUE)

# remove rows with NA values (need for model selection)
RASE_data_NA <- na.omit(RASE_data_18_24)

# check how many MMAs remain
length(unique(RASE_data$Registreri))
length(unique(RASE_data_18_24$Registreri))
length(unique(RASE_data_NA$Registreri))

## Three point averages ####

# Last three-point average using five years (2020-2024)

RASE_data_5y_3_point_avg <- RASE_data_NA %>%
  filter(InvAr >= 2020 & InvAr <= 2024) %>%  # Keep only relevant years
  group_by(Registreri, LandsdelNamn, LanNamn) %>%
  arrange(Registreri, desc(InvAr)) %>%  # Sort in descending order
  slice_head(n = 3) %>%  # Select most recent 3 years within range
  summarise(
    across(where(is.numeric) & !all_of("InvAr"), list(mean = ~mean(.x, na.rm = TRUE),
                                                      sd = ~sd(.x, na.rm = TRUE))),
    years_used = paste(InvAr, collapse = ", ")  # Keep track of years used
  ) %>%
  ungroup()

# Last three-point average using all data (2018-2024)
RASE_data_abin_3_point_avg <- RASE_data_NA %>%
  filter(InvAr >= 2018 & InvAr <= 2024) %>%  # Keep only relevant years
  group_by(Registreri, LandsdelNamn, LanNamn) %>%
  arrange(Registreri, desc(InvAr)) %>%  # Sort in descending order
  slice_head(n = 3) %>%  # Select earliest 3 years within range
  summarise(
    across(where(is.numeric) & !all_of("InvAr"), list(mean = ~mean(.x, na.rm = TRUE),
                                                      sd = ~sd(.x, na.rm = TRUE))),
    years_used = paste(InvAr, collapse = ", ")  # Keep track of years used
  ) %>%
  ungroup()

# Print results
print(RASE_data_5y_3_point_avg)
print(RASE_data_abin_3_point_avg)

# Check for potential co-linearity
# Calculate correlation matrices
cor_matrix_5y <- cor(RASE_data_5y_3_point_avg[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                               "BestHojdAllaAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                               "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", # Site
                                               "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                               "Mean_seasonal_temp[c]_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                  method = "pearson", use = "pairwise.complete.obs")

cor_matrix_abin <- cor(RASE_data_abin_3_point_avg[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                                  "BestHojdAllaAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                                  "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", # Site
                                                  "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                                  "Mean_seasonal_temp[c]_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                     method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_5y <- cor_matrix_5y
filtered_cor_5y[abs(filtered_cor_5y) <= 0.7 | abs(filtered_cor_5y) == 1] <- NA

filtered_cor_abin <- cor_matrix_abin
filtered_cor_abin[abs(filtered_cor_abin) <= 0.7 | abs(filtered_cor_abin) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_5y
filtered_cor_abin


## RASE per hectare national ####
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(car)
library(DHARMa)
library(MASS)

# Use gamma model (log link) as data is count-like and positively skewed
glm_RASE_Ha <- glm(AntalRASEHa_mean ~ scale(Älgtäthet.i.vinterstam_mean) +
                     scale(ungulate_index_mean) +
                     scale(WB1000_mean) +
                     scale(AntalTallarHa_mean) +
                     scale(AntalBjorkarHa_mean) +
                     scale(AntalOvrigtHa_mean) +
                     scale(proportion_young_forest_mean) +
                     scale(AndelBordigaMarker_mean) +
                     scale(youngforest_area_ha_mean) +
                     scale(Medelbestandshojd_mean) +
                     scale(AndelRojt...18_mean) +
                     # scale(`mean_seasonal_snowdepth[cm]_imputed_mean`) +
                     scale(`Mean_seasonal_precipitation[mm]_imputed_mean`),
                   family = Gamma(link = "log"),
                   data = RASE_data_abin_3_point_avg)

summary(glm_RASE_Ha)

# Alternatively try a negative binomial model (NB response variable is not an integer!)
# glm_RASE_Ha_nb <- glm.nb(AntalRASEHa_mean ~ scale(Älgtäthet.i.vinterstam_mean) +
#                           scale(ungulate_index_mean) +
#                           scale(WB1000_mean) +
#                           scale(AntalTallarHa_mean) +
#                           scale(AntalBjorkarHa_mean) +
#                           scale(AntalOvrigtHa_mean) +
#                           scale(proportion_young_forest_mean) +
#                           scale(AndelBordigaMarker_mean) +
#                           scale(youngforest_area_ha_mean) +
#                           scale(Medelbestandshojd_mean) +
#                           scale(AndelRojt...18_mean) +
#                           scale(BestandAlder_mean) +
#                           scale(`Mean_seasonal_precipitation[mm]_imputed_mean`),
#                         data = RASE_data_5y_3_point_avg)


# Check variance inflation factor (VIF)
vif(glm_RASE_Ha)

# Check for over dispersal
glm_RASE_Ha_simres <- simulateResiduals(glm_RASE_Ha_nb)
testDispersion(glm_RASE_Ha_simres)

# Check for normality of residuals
shapiro.test(residuals(glm_RASE_Ha))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_Ha)); qqline(residuals(glm_RASE_Ha), col = "red")  # Q-Q plot

# Use "drop1" to drop one predictor at a time and compare AIC values
drop1(glm_RASE_Ha, test = "Chisq")

# Once happy with model type and variables Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_glm_RASE <- dredge(glm_RASE_Ha)
summary(dredged_glm_RASE)

# Select models within ΔAIC < 2 of the best model
top_glm_RASE <- subset(dredged_glm_RASE, subset = delta < 2)
summary(top_glm_RASE)

# Get the best model (rank 1)
best_glm_RASE_Ha <- get.models(dredged_glm_RASE, subset = 1)[[1]]
summary(best_glm_RASE_Ha)

# Check the AIC compared to the original model
AIC(best_glm_RASE_Ha, glm_RASE_Ha)

# Plot fixed effects from the GLM
# Extract coefficients for the fixed effects
coef_glm <- summary(best_glm_RASE_Ha)$coefficients

# Convert to dataframe
fixed_effects_glm <- data.frame(
  Term = rownames(coef_glm),
  Estimate = coef_glm[, "Estimate"],
  SE = coef_glm[, "Std. Error"],
  t_value = coef_glm[, "t value"],  # Change from z-value (GLMM) to t-value (LM)
  p_value = coef_glm[, "Pr(>|t|)"]  # Change Pr(>|z|) to Pr(>|t|)
)

# Remove intercept
fixed_effects_glm <- fixed_effects_glm[fixed_effects_glm$Term != "(Intercept)", ]

# Add significance markers
fixed_effects_glm$Significance <- dplyr::case_when(
  fixed_effects_glm$p_value < 0.001 ~ "***",
  fixed_effects_glm$p_value < 0.01  ~ "**",
  fixed_effects_glm$p_value < 0.05  ~ "*",
  TRUE ~ ""
)

# Rename terms for plotting
fixed_effects_glm$Term <- dplyr::recode(fixed_effects_glm$Term,
                                       "scale(Älgtäthet.i.vinterstam_mean)" = "Moose Density",        
                                       "scale(FD1000_mean)" = "Fallow Deer Density",
                                       "scale(WB1000_mean)" = "Wild Boar Density",
                                       "scale(AntalTallarHa_mean)" = "Number of Pines per Ha",
                                       "scale(AntalBjorkarHa_mean)" = "Number of Birches per Ha",
                                       "scale(AntalOvrigtHa_mean)" = "Number of Other Trees per Ha",
                                       "scale(proportion_young_forest_mean)" = "Proportion Young Forest",
                                       "scale(AndelBordigaMarker_mean)" = "Proportion on Productive Land",
                                       "scale(youngforest_area_ha_mean)" = "Young Forest Area (Ha)",
                                       "scale(BestandAlder_mean)" = "Stand Age",
                                       "scale(Medelbestandshojd_mean)" = "Average Stand Height",
                                       "scale(AndelRojt...18_mean)" = "Proportion PCT",
                                       "scale(`mean_seasonal_snowdepth[cm]_imputed_mean`)" = "Mean Seasonal Snow Depth", 
                                       "scale(`Mean_seasonal_precipitation[mm]_imputed_mean`)" = "Mean Winter Precipitation"
)

# Plot with ggplot2
RASE_Ha_plot <- ggplot(fixed_effects_glm, aes(x = Term, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "solid", size = 1.2, color = "black") +  # Add thick line at 0
  geom_text(aes(label = Significance), vjust = -1, size = 5) +  # Add significance asterisks
  coord_flip() +  # Flip x-axis for readability
  theme_classic() +
  labs(title = "Fixed Effects on RASE per Ha.", y = "Estimate") +
  theme(axis.text.x = element_text(size = 10))

RASE_Ha_plot

# Export plot
ggsave("RASE_Ha_abin_plot.tiff", plot = RASE_Ha_plot, path = "~/GitHub/Moose-Targets/Plots", 
       scale = 1, width = 14, height = 14, dpi = 300, units = "cm")

## RASE per hectare regions ####

# Take RASE_data_NA and filter for regions

## Norrland
RASE_data_Norrland <- RASE_data_abin_3_point_avg %>%
  filter(LandsdelNamn %in% c("Södra Norrland", "Norra Norrland"))

# Create a correlation matrix
cor_matrix_norr <- cor(RASE_data_Norrland[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                                      "BestHojdAllaAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                                      "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", # Site
                                                      "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                                      "Mean_seasonal_temp[c]_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_norr <- cor_matrix_norr
filtered_cor_norr[abs(filtered_cor_norr) <= 0.7 | abs(filtered_cor_norr) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_norr

# Run the model
glm_RASE_Ha_N <- glm(AntalRASEHa_mean ~ scale(Älgtäthet.i.vinterstam_mean) +
                      # scale(ungulate_index_mean) +
                      scale(WB1000_mean) +
                      scale(AntalTallarHa_mean) +
                      scale(AntalBjorkarHa_mean) +
                      scale(AntalOvrigtHa_mean) +
                      scale(proportion_young_forest_mean) +
                      # scale(AndelBordigaMarker_mean) +
                      scale(youngforest_area_ha_mean) +
                      scale(Medelbestandshojd_mean) +
                      scale(AndelRojt...18_mean) +
                      scale(`mean_seasonal_snowdepth[cm]_imputed_mean`) +
                      scale(`Mean_seasonal_precipitation[mm]_imputed_mean`),
                    family = Gamma(link = "log"),
                    data = RASE_data_Norrland)

summary(glm_RASE_Ha_N)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_N)

# Check for over dispersal
glm_RASE_Ha_N_simres <- simulateResiduals(glm_RASE_Ha_N)
testDispersion(glm_RASE_Ha_N_simres)

# Use "drop1" to drop one predictor at a time and compare AIC values
drop1(glm_RASE_Ha_N, test = "Chisq")

# Once happy with model type and variables Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_glm_RASE_Ha_N <- dredge(glm_RASE_Ha_N)
summary(dredged_glm_RASE_Ha_N)

# Select models within ΔAIC < 2 of the best model
top_glm_RASE <- subset(dredged_glm_RASE_Ha_N, subset = delta < 2)
summary(top_glm_RASE)

# Get the best model (rank 1)
best_glm_RASE_Ha_N <- get.models(dredged_glm_RASE_Ha_N, subset = 1)[[1]]
summary(best_glm_RASE_Ha_N)

# Check the AIC compared to the original model
AIC(best_glm_RASE_Ha_N, glm_RASE_Ha_N)

## Svealand
RASE_data_Svealand <- RASE_data_abin_3_point_avg %>%
  filter(LandsdelNamn %in% c("Svealand"))

# Create a correlation matrix
cor_matrix_svea <- cor(RASE_data_Svealand[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                              "BestHojdAllaAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                              "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", # Site
                                              "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                              "Mean_seasonal_temp[c]_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_svea <- cor_matrix_svea
filtered_cor_svea[abs(filtered_cor_svea) <= 0.7 | abs(filtered_cor_svea) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_svea

# Run the model
glm_RASE_Ha_S <- glm(AntalRASEHa_mean ~ scale(Älgtäthet.i.vinterstam_mean) +
                     scale(ungulate_index_mean) +
                     scale(WB1000_mean) +
                     scale(AntalTallarHa_mean) +
                     scale(AntalBjorkarHa_mean) +
                     scale(AntalOvrigtHa_mean) +
                     scale(proportion_young_forest_mean) +
                     scale(AndelBordigaMarker_mean) +
                     scale(youngforest_area_ha_mean) +
                     scale(Medelbestandshojd_mean) +
                     scale(AndelRojt...18_mean) +
                     scale(`mean_seasonal_snowdepth[cm]_imputed_mean`) +
                     scale(`Mean_seasonal_precipitation[mm]_imputed_mean`),
                   family = Gamma(link = "log"),
                   data = RASE_data_Svealand)

summary(glm_RASE_Ha_S)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_S)

# Check for over dispersal
glm_RASE_Ha_S_simres <- simulateResiduals(glm_RASE_Ha_S)
testDispersion(glm_RASE_Ha_S_simres)

# Use "drop1" to drop one predictor at a time and compare AIC values
drop1(glm_RASE_Ha_S, test = "Chisq")

# Once happy with model type and variables Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_glm_RASE_Ha_S <- dredge(glm_RASE_Ha_S)
summary(dredged_glm_RASE_Ha_S)

# Select models within ΔAIC < 2 of the best model
top_glm_RASE_s <- subset(dredged_glm_RASE_Ha_S, delta < 2)
summary(top_glm_RASE_s)

# Get the best model (rank 1)
best_glm_RASE_Ha_S <- get.models(dredged_glm_RASE_Ha_S, subset = 1)[[1]]
summary(best_glm_RASE_Ha_S)

# Check the AIC compared to the original model
AIC(best_glm_RASE_Ha_S, glm_RASE_Ha_S)

## Götaland
RASE_data_Gotaland <- RASE_data_abin_3_point_avg %>%
  filter(LandsdelNamn %in% c("Götaland"))

# Create a correlation matrix
cor_matrix_gota <- cor(RASE_data_Gotaland[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                              "BestHojdAllaAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                              "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", # Site
                                              "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                              "Mean_seasonal_temp[c]_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_gota <- cor_matrix_gota
filtered_cor_gota[abs(filtered_cor_gota) <= 0.7 | abs(filtered_cor_gota) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_gota

# Run the model
glm_RASE_Ha_G <- glm(log(AntalRASEHa_mean) ~ scale(Älgtäthet.i.vinterstam_mean) +
                     scale(ungulate_index_mean) +
                     scale(WB1000_mean) +
                     scale(AntalTallarHa_mean) +
                     scale(AntalBjorkarHa_mean) +
                     scale(AntalOvrigtHa_mean) +
                     scale(proportion_young_forest_mean) +
                     scale(AndelBordigaMarker_mean) +
                     scale(youngforest_area_ha_mean) +
                     scale(Medelbestandshojd_mean) +
                     scale(AndelRojt...18_mean) +
                     scale(`mean_seasonal_snowdepth[cm]_imputed_mean`) +
                     scale(`Mean_seasonal_precipitation[mm]_imputed_mean`),
                   family = Gamma(link = "log"),
                   data = RASE_data_Gotaland)


summary(glm_RASE_Ha_G)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_G)

# Check for over dispersal
glm_RASE_Ha_G_simres <- simulateResiduals(glm_RASE_Ha_G)
testDispersion(glm_RASE_Ha_G_simres)

# Once happy with model type and variables Run model selection 
options(na.action = "na.fail")  # Prevent `dredge` from failing silently due to missing data
dredged_glm_RASE_Ha_G <- dredge(glm_RASE_Ha_G)
summary(dredged_glm_RASE_Ha_G)

# Select models within ΔAIC < 2 of the best model
top_glm_RASE_G <- subset(dredged_glm_RASE_Ha_G, delta < 2)
summary(top_glm_RASE_G)

# Get the best model (rank 1)
best_glm_RASE_Ha_G <- get.models(dredged_glm_RASE_Ha_G, subset = 1)[[1]]
summary(best_glm_RASE_Ha_G)

# Check the AIC compared to the original model
AIC(best_glm_RASE_Ha_G, glm_RASE_Ha_G)

## RASE at competitive height national ####
library(dplyr)
library(betareg)
library(MuMIn)
library(ggplot2)
library(sjPlot)

RASE_competative_betar <- betareg(RASEAndelGynnsam_mean ~ scale(Älgtäthet.i.vinterstam_mean) +
                                    scale(ungulate_index_mean) +
                                    scale(WB1000_mean) +
                                    scale(AntalTallarHa_mean) +
                                    scale(AntalBjorkarHa_mean) +
                                    scale(AntalOvrigtHa_mean) +
                                    scale(proportion_young_forest_mean) +
                                    scale(AndelBordigaMarker_mean) +
                                    scale(youngforest_area_ha_mean) +
                                    scale(Medelbestandshojd_mean) +
                                    scale(AndelRojt...18_mean) +
                                    scale(`mean_seasonal_snowdepth[cm]_imputed_mean`) +
                                    scale(`Mean_seasonal_precipitation[mm]_imputed_mean`),
                                  data = RASE_data_5y_3_point_avg)

summary(RASE_competative_betar)

# Check variance inflation factor (VIF)
vif(RASE_competative_betar)

# Check for over dispersal
glm_RASE_Ha_simres <- simulateResiduals(RASE_competative_betar)
testDispersion(glm_RASE_Ha_simres)

# Check for normality of residuals
shapiro.test(residuals(RASE_competative_betar))  # Shapiro-Wilk test
qqnorm(residuals(RASE_competative_betar)); qqline(residuals(RASE_competative_betar), col = "red")  # Q-Q plot

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
                                        "scale(Älgtäthet.i.vinterstam_mean)" = "Moose Density",        
                                        "scale(FD1000_mean)" = "Fallow Deer Density",
                                        "scale(WB1000_mean)" = "Wild Boar Density",
                                        "scale(AntalTallarHa_mean)" = "Number of Pines per Ha",
                                        "scale(AntalBjorkarHa_mean)" = "Number of Birches per Ha",
                                        "scale(AntalOvrigtHa_mean)" = "Number of Other Trees per Ha",
                                        "scale(proportion_young_forest_mean)" = "Proportion Young Forest",
                                        "scale(AndelBordigaMarker_mean)" = "Proportion on Productive Land",
                                        "scale(youngforest_area_ha_mean)" = "Young Forest Area (Ha)",
                                        "scale(BestandAlder_mean)" = "Stand Age",
                                        "scale(Medelbestandshojd_mean)" = "Average Stand Height",
                                        "scale(AndelRojt...18_mean)" = "Proportion PCT",
                                        "scale(`mean_seasonal_snowdepth[cm]_imputed_mean`)" = "Mean Seasonal Snow Depth", 
                                        "scale(`Mean_seasonal_precipitation[mm]_imputed_mean`)" = "Mean Winter Precipitation"
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

# RASE per Ha.
tab_model(best_glm_RASE_Ha, best_glm_RASE_Ha_N, best_glm_RASE_Ha_S, best_glm_RASE_Ha_G,
          transform = NULL, 
          show.ci = FALSE, 
          show.se = TRUE, 
          show.aic = TRUE,
          show.stat = TRUE,
          #show.bic = TRUE,
          #show.icc = FALSE,
          file = "~/GitHub/Moose-Targets/Tables/RASE_Ha_abin_table.html")
