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

## Select variables for modelling from big data ####

# Select the most relevant ecological variables for RASE per ha. (AntalRASEHa) and 
# % RASE at competative height (RASEAndelGynnsam)
RASE_data <- Big_data %>%
  dplyr::select(LandsdelNamn,LanNamn, # Regional data
                AntalRASEHa, RASEAndelGynnsam, # Independent variables 
                Älgtäthet.i.vinterstam, Roe1000, FD1000, Red1000, WB1000, ungulate_index, # Browsers
                youngforest_area_ha, proportion_young_forest, AndelBordigaMarker, BestHojdAbinAVG, BestandAlder, # Site
                Medelbestandshojd, AndelRojt...18, BestHojdAbinArealV, # Site
                AntalGranarHa, AntalTallarHa, AntalBjorkarHa, AntalOvrigtHa, # Competitor species
                prop_snow_30_plus_imputed, `Mean_seasonal_precipitation[mm]_imputed`, `mean_seasonal_snowdepth[cm]_imputed`, # Climate
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

# Last three-point average using all data (2018-2024)
RASE_data_abin_3_point_avg <- RASE_data_NA %>%
  filter(InvAr >= 2018 & InvAr <= 2024) %>%  # Keep only relevant years
  group_by(Registreri, LandsdelNamn, LanNamn) %>%
  arrange(Registreri, desc(InvAr)) %>%  # Sort in descending order
  slice_head(n = 3) %>%  # Select latest 3 years within range
  summarise(
    across(where(is.numeric) & !all_of("InvAr"), list(mean = ~mean(.x, na.rm = TRUE),
                                                      sd = ~sd(.x, na.rm = TRUE))),
    years_used = paste(InvAr, collapse = ", ")  # Keep track of years used
  ) %>%
  ungroup()

# Print results
print(RASE_data_abin_3_point_avg)

# Check for potential co-linearity
# Calculate correlation matrices
cor_matrix_abin <- cor(RASE_data_abin_3_point_avg[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                                  "BestHojdAbinAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                                  "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", "BestHojdAbinArealV_mean", # Site
                                                  "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                                  "prop_snow_30_plus_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                     method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_abin <- cor_matrix_abin
filtered_cor_abin[abs(filtered_cor_abin) <= 0.7 | abs(filtered_cor_abin) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_abin


## RASE per hectare national ####
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(car)
library(DHARMa)
library(MASS)

# Read "Predictors of browsing damage on commercial forests – A study linking 
# nationwide management data" Pfeffer et al. 2021

# Use gamma model (log link) as data is count-like and positively skewed
glm_RASE_Ha <- glm(AntalRASEHa_mean ~ 
                     scale(Älgtäthet.i.vinterstam_mean) +
                     scale(ungulate_index_mean) +
                     scale(AntalTallarHa_mean) +
                     scale(AntalBjorkarHa_mean) +
                     scale(AndelRojt...18_mean) +
                     scale(proportion_young_forest_mean) +
                     scale(prop_snow_30_plus_imputed_mean),
                   family = Gamma(link = "log"),
                   data = RASE_data_abin_3_point_avg)

summary(glm_RASE_Ha)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha)

# Check for over dispersion
glm_RASE_Ha_simres <- simulateResiduals(glm_RASE_Ha)
testDispersion(glm_RASE_Ha_simres)

# Check for normality of residuals
shapiro.test(residuals(glm_RASE_Ha))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_Ha)); qqline(residuals(glm_RASE_Ha), col = "red")  # Q-Q plot

# Backward stepwise selection
glm_RASE_Ha_bck <- step(glm_RASE_Ha, direction = "backward", trace = TRUE) # same result as drop1
summary(glm_RASE_Ha_bck)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_bck)

# Check for over dispersion
bck_glm_RASE_Ha_simres <- simulateResiduals(glm_RASE_Ha_bck)
testDispersion(bck_glm_RASE_Ha_simres)

# Check for normality of residuals
shapiro.test(residuals(glm_RASE_Ha_bck))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_Ha_bck)); qqline(residuals(glm_RASE_Ha_bck), col = "red")  # Q-Q plot

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_Ha_null <- glm(AntalRASEHa_mean ~ 1,  # Model with no predictors
                          family = Gamma(link = "log"),
                          data = RASE_data_abin_3_point_avg)

# Fit a forward stepwise model
glm_RASE_Ha_fwd <- step(glm_RASE_Ha_null, 
                          scope = list(lower = glm_RASE_Ha_null, 
                                       upper = ~ 
                                         scale(Älgtäthet.i.vinterstam_mean) +
                                         scale(ungulate_index_mean) +
                                         scale(AntalTallarHa_mean) +
                                         scale(AntalBjorkarHa_mean) +
                                         scale(AndelRojt...18_mean) +
                                         scale(proportion_young_forest_mean) +
                                         scale(prop_snow_30_plus_imputed_mean),
                                       direction = "forward", 
                                       trace = TRUE))

summary(glm_RASE_Ha_fwd)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_fwd)

# Check for over dispersion
fwd_glm_RASE_Ha_simres <- simulateResiduals(glm_RASE_Ha_fwd)
testDispersion(fwd_glm_RASE_Ha_simres)

# Check for normality of residuals
shapiro.test(residuals(glm_RASE_Ha_fwd))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_Ha_fwd)); qqline(residuals(glm_RASE_Ha_fwd), col = "red")  # Q-Q plot

# Check the final AIC compared to the original model
AIC(glm_RASE_Ha_bck, glm_RASE_Ha_fwd,  glm_RASE_Ha)

# Plot fixed effects from the GLM
# Extract coefficients for the fixed effects
coef_glm <- summary(glm_RASE_Ha_bck)$coefficients

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
                                        "scale(ungulate_index_mean)" = "Other* Deer Density",
                                        "scale(AntalTallarHa_mean)" = "Pines Density",
                                        "scale(AntalBjorkarHa_mean)" = "Birches Density",
                                        "scale(AntalOvrigtHa_mean)" = "Other* Trees Density",
                                        "scale(proportion_young_forest_mean)" = "Proportion Young Forest",
                                        "scale(AndelBordigaMarker_mean)" = "Proportion on Productive Land",
                                        "scale(youngforest_area_ha_mean)" = "Young Forest Area (Ha)",
                                        "scale(BestandAlder_mean)" = "Stand Age",
                                        "scale(Medelbestandshojd_mean)" = "Average Stand Height",
                                        "scale(AndelRojt...18_mean)" = "Proportion PCT",
                                        "scale(`mean_seasonal_snowdepth[cm]_imputed_mean`)" = "Seasonal Snow Depth", 
                                        "scale(prop_snow_30_plus_imputed_mean)" = "Winter Severity"
)

# Plot with ggplot2
RASE_Ha_plot <- ggplot(fixed_effects_glm, aes(x = Term, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "solid", size = 1.2, color = "black") +  # Add thick line at 0
  geom_text(aes(label = Significance), vjust = -1, size = 5) +  # Add significance asterisks
  coord_flip() +  # Flip x-axis for readability
  theme_classic() +
  labs(title = "Factors Related to RASE Density", y = "Estimate") +
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
cor_matrix_norr <- cor(RASE_data_Norrland[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                              "BestHojdAbinAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                              "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", "BestHojdAbinArealV_mean", # Site
                                              "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                              "prop_snow_30_plus_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_norr <- cor_matrix_norr
filtered_cor_norr[abs(filtered_cor_norr) <= 0.7 | abs(filtered_cor_norr) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_norr

# Run the model
glm_RASE_Ha_N <- glm(AntalRASEHa_mean ~ 
                       scale(Älgtäthet.i.vinterstam_mean) +
                       # scale(ungulate_index_mean) +
                       scale(AntalTallarHa_mean) +
                       scale(AntalBjorkarHa_mean) +
                       scale(AndelRojt...18_mean) +
                       scale(proportion_young_forest_mean) +
                       scale(prop_snow_30_plus_imputed_mean),
                     family = Gamma(link = "log"),
                     data = RASE_data_Norrland)

summary(glm_RASE_Ha_N)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_N)

# Check for over dispersion
glm_RASE_Ha_N_simres <- simulateResiduals(glm_RASE_Ha_N)
testDispersion(glm_RASE_Ha_N_simres)

# Use step() for backwards stepwise selection
glm_RASE_Ha_N_bck <- step(glm_RASE_Ha_N, direction = "backward", trace = TRUE) # same result as drop1
summary(glm_RASE_Ha_N_bck)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_Ha_N_null <- glm(AntalRASEHa_mean ~ 1,  # Model with no predictors
                          family = Gamma(link = "log"),
                          data = RASE_data_Norrland)

# Fit a forward stepwise model
glm_RASE_Ha_N_fwd <- step(glm_RASE_Ha_N_null, 
                          scope = list(lower = glm_RASE_Ha_N_null, 
                                       upper = ~ 
                                         scale(Älgtäthet.i.vinterstam_mean) +
                                         # scale(ungulate_index_mean) +
                                         scale(AntalTallarHa_mean) +
                                         scale(AntalBjorkarHa_mean) +
                                         scale(AndelRojt...18_mean) +
                                         scale(proportion_young_forest_mean) +
                                         scale(prop_snow_30_plus_imputed_mean),
                                       direction = "forward", 
                                       trace = TRUE))

summary(glm_RASE_Ha_N_fwd)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_N_fwd)

# Check for over dispersion
glm_RASE_Ha_N_simres <- simulateResiduals(glm_RASE_Ha_N_fwd)
testDispersion(glm_RASE_Ha_N_simres)

# Check the AIC compared to the original model
AIC(glm_RASE_Ha_N_fwd ,glm_RASE_Ha_N_bck, glm_RASE_Ha_N)

## Svealand
RASE_data_Svealand <- RASE_data_abin_3_point_avg %>%
  filter(LandsdelNamn %in% c("Svealand"))

# Create a correlation matrix
cor_matrix_svea <- cor(RASE_data_Svealand[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                              "BestHojdAbinAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                              "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", "BestHojdAbinArealV_mean", # Site
                                              "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                              "prop_snow_30_plus_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_svea <- cor_matrix_svea
filtered_cor_svea[abs(filtered_cor_svea) <= 0.7 | abs(filtered_cor_svea) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_svea

# Run the model (potentially over fitted, remove TallarHa?)
glm_RASE_Ha_S <- glm(AntalRASEHa_mean ~ 
                       scale(Älgtäthet.i.vinterstam_mean) +
                       scale(ungulate_index_mean) +
                       scale(AntalTallarHa_mean) +
                       scale(AntalBjorkarHa_mean) +
                       scale(AndelRojt...18_mean) +
                       scale(proportion_young_forest_mean) +
                       scale(prop_snow_30_plus_imputed_mean),
                     family = Gamma(link = "log"),
                     data = RASE_data_Svealand)

summary(glm_RASE_Ha_S)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_S)

# Check for over dispersion
glm_RASE_Ha_S_simres <- simulateResiduals(glm_RASE_Ha_S)
testDispersion(glm_RASE_Ha_S_simres)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_Ha_S_null <- glm(AntalRASEHa_mean ~ 1,  # Model with no predictors
                          family = Gamma(link = "log"),
                          data = RASE_data_Svealand)

# Fit a forward stepwise model
glm_RASE_Ha_S_fwd <- step(glm_RASE_Ha_S_null, 
                          scope = list(lower = glm_RASE_Ha_S_null, 
                                       upper = ~ 
                                         scale(Älgtäthet.i.vinterstam_mean) +
                                         scale(ungulate_index_mean) +
                                         scale(AntalTallarHa_mean) +
                                         scale(AntalBjorkarHa_mean) +
                                         scale(AndelRojt...18_mean) +
                                         scale(proportion_young_forest_mean) +
                                         scale(prop_snow_30_plus_imputed_mean),
                                       direction = "forward", 
                                       trace = TRUE))

summary(glm_RASE_Ha_S_fwd)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_S_fwd)

# Check for over dispersion
glm_RASE_Ha_S_simres <- simulateResiduals(glm_RASE_Ha_S_fwd)
testDispersion(glm_RASE_Ha_S_simres)

# Check the AIC compared to the original model
AIC(glm_RASE_Ha_S_fwd, glm_RASE_Ha_S)

## Götaland
RASE_data_Gotaland <- RASE_data_abin_3_point_avg %>%
  filter(LandsdelNamn %in% c("Götaland"))

# Create a correlation matrix
cor_matrix_gota <- cor(RASE_data_Gotaland[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                              "BestHojdAbinAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                              "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", "BestHojdAbinArealV_mean", # Site
                                              "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                              "prop_snow_30_plus_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_gota <- cor_matrix_gota
filtered_cor_gota[abs(filtered_cor_gota) <= 0.7 | abs(filtered_cor_gota) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_gota

# Run the model
glm_RASE_Ha_G <- glm(AntalRASEHa_mean ~ 
                       # scale(Älgtäthet.i.vinterstam_mean) +
                       scale(ungulate_index_mean) +
                       scale(AntalGranarHa_mean) +
                       scale(AntalBjorkarHa_mean) +
                       scale(AndelRojt...18_mean) +
                       scale(proportion_young_forest_mean) +
                       scale(prop_snow_30_plus_imputed_mean),
                     family = Gamma(link = "log"),
                     data = RASE_data_Gotaland)


summary(glm_RASE_Ha_G)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_G)

# Check for over dispersion
glm_RASE_Ha_G_simres <- simulateResiduals(glm_RASE_Ha_G)
testDispersion(glm_RASE_Ha_G_simres)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_Ha_G_null <- glm(AntalRASEHa_mean ~ 1,  # Model with no predictors
                          family = Gamma(link = "log"),
                          data = RASE_data_Gotaland)

# Fit a forward stepwise model
glm_RASE_Ha_G_fwd <- step(glm_RASE_Ha_G_null, 
                          scope = list(lower = glm_RASE_Ha_G_null, 
                                       upper = ~ 
                                         # scale(Älgtäthet.i.vinterstam_mean) +
                                         scale(ungulate_index_mean) +
                                         scale(AntalGranarHa_mean) +
                                         scale(AntalBjorkarHa_mean) +
                                         scale(AndelRojt...18_mean) +
                                         scale(proportion_young_forest_mean) +
                                         scale(prop_snow_30_plus_imputed_mean),
                                       direction = "forward", 
                                       trace = TRUE))

summary(glm_RASE_Ha_G_fwd)

# Check variance inflation factor (VIF)
vif(glm_RASE_Ha_G_fwd)

# Check for over dispersion
glm_RASE_Ha_G_simres <- simulateResiduals(glm_RASE_Ha_G_fwd)
testDispersion(glm_RASE_Ha_G_simres)

# Check the AIC compared to the original model
AIC(glm_RASE_Ha_G_fwd, glm_RASE_Ha_G)

## RASE at competitive height national ####
library(dplyr)
library(glmmTMB)
library(MuMIn)
library(ggplot2)
library(sjPlot)

# Clip the values of RASEAndelGynnsam_mean
RASE_data_abin_3_point_avg$RASEAndelGynnsam_mean <- pmax(pmin(RASE_data_abin_3_point_avg$RASEAndelGynnsam_mean, 0.9999), 0.0001)

# Run the model
glm_RASE_comp <- glmmTMB(RASEAndelGynnsam_mean ~ 
                                         scale(Älgtäthet.i.vinterstam_mean) +
                                         scale(ungulate_index_mean) +
                                         scale(AntalTallarHa_mean) +
                                         scale(AntalBjorkarHa_mean) +
                                         scale(AndelRojt...18_mean) +
                                         scale(proportion_young_forest_mean) +
                                         scale(prop_snow_30_plus_imputed_mean),
                                  family = beta_family(link = "logit"),
                                  data = RASE_data_abin_3_point_avg)

summary(glm_RASE_comp)

# Check for over dispersion
glm_RASE_comp_simres <- simulateResiduals(glm_RASE_comp)
testDispersion(glm_RASE_comp_simres)

# Check for normality of residuals (less imprtant is not over-dispersed)
shapiro.test(residuals(glm_RASE_comp))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_comp)); qqline(residuals(glm_RASE_comp), col = "red")  # Q-Q plot

# Backward stepwise selection
glm_RASE_comp_bck <- step(glm_RASE_comp, direction = "backward", trace = TRUE) # same result as drop1
summary(glm_RASE_comp_bck)

# Check for over dispersion
bck_glm_RASE_comp_simres <- simulateResiduals(glm_RASE_comp_bck)
testDispersion(bck_glm_RASE_comp_simres)

# Check for normality of residuals
shapiro.test(residuals(glm_RASE_comp_bck))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_comp_bck)); qqline(residuals(glm_RASE_comp_bck), col = "red")  # Q-Q plot

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_comp_null <- glmmTMB(RASEAndelGynnsam_mean ~ 1,  # Model with no predictors
                        family = beta_family(link = "logit"),
                        data = RASE_data_abin_3_point_avg)

# Fit a forward stepwise model
glm_RASE_comp_fwd <- step(glm_RASE_comp_null, 
                        scope = list(lower = glm_RASE_comp_null, 
                                     upper = ~ 
                                       scale(Älgtäthet.i.vinterstam_mean) +
                                       scale(ungulate_index_mean) +
                                       scale(AntalTallarHa_mean) +
                                       scale(AntalBjorkarHa_mean) +
                                       scale(AndelRojt...18_mean) +
                                       scale(proportion_young_forest_mean) +
                                       scale(prop_snow_30_plus_imputed_mean),
                                     direction = "forward", 
                                     trace = TRUE))

summary(glm_RASE_comp_fwd)


# Check for over dispersion
fwd_glm_RASE_comp_simres <- simulateResiduals(glm_RASE_comp_fwd)
testDispersion(fwd_glm_RASE_comp_simres)

# Check for normality of residuals
shapiro.test(residuals(glm_RASE_comp_fwd))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_comp_fwd)); qqline(residuals(glm_RASE_comp_fwd), col = "red")  # Q-Q plot

# Check the final AIC compared to the original model
AIC(glm_RASE_comp_bck, glm_RASE_comp_fwd,  glm_RASE_comp)

# Plot fixed effects from the GLMM
# Extract coefficients for the fixed effects
coef_glm <- summary(glm_RASE_comp_fwd)$coefficients$cond
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
                                        "scale(ungulate_index_mean)" = "Other* Deer Density",
                                        "scale(AntalTallarHa_mean)" = "Pines Density",
                                        "scale(AntalBjorkarHa_mean)" = "Birches Density",
                                        "scale(AntalOvrigtHa_mean)" = "Other* Trees Density",
                                        "scale(proportion_young_forest_mean)" = "Proportion Young Forest",
                                        "scale(AndelBordigaMarker_mean)" = "Proportion on Productive Land",
                                        "scale(youngforest_area_ha_mean)" = "Young Forest Area (Ha)",
                                        "scale(BestandAlder_mean)" = "Stand Age",
                                        "scale(Medelbestandshojd_mean)" = "Average Stand Height",
                                        "scale(AndelRojt...18_mean)" = "Proportion PCT",
                                        "scale(`mean_seasonal_snowdepth[cm]_imputed_mean`)" = "Mean Seasonal Snow Depth", 
                                        "scale(prop_snow_30_plus_imputed_mean)" = "Winter Severity"
)

# Plot with ggplot2
RASE_Gyn_plot <- ggplot(fixed_effects_glm, aes(x = Term, y = Estimate, ymin = Estimate - 1.96 * SE, ymax = Estimate + 1.96 * SE)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "solid", size = 1.2, color = "black") +  # Add thick line at 0
  geom_text(aes(label = Significance), vjust = -1, size = 5) +  # Add significance asterisks
  coord_flip() +  # To flip the x-axis for better readability
  theme_classic() +
  labs(title = "Factors Related to RASE Competitive Status", y = "Estimate") +
  theme(axis.text.x = element_text(size = 10))

RASE_Gyn_plot

# Export with ggsave 
ggsave("RASE_comp_abin_plot.tiff", plot = RASE_Gyn_plot, device = NULL, 
       path = "~/GitHub/Moose-Targets/Plots", scale = 1, width = 14, 
       height = 14, dpi = 300, limitsize = TRUE, units = "cm")

## RASE at competitive height regions ####

# Take RASE_data_NA and filter for regions

## Norrland
RASE_data_Norrland <- RASE_data_abin_3_point_avg %>%
  filter(LandsdelNamn %in% c("Södra Norrland", "Norra Norrland"))

# Create a correlation matrix
cor_matrix_norr <- cor(RASE_data_Norrland[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                              "BestHojdAbinAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                              "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", "BestHojdAbinArealV_mean", # Site
                                              "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                              "prop_snow_30_plus_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_norr <- cor_matrix_norr
filtered_cor_norr[abs(filtered_cor_norr) <= 0.7 | abs(filtered_cor_norr) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_norr

# Run the model
glm_RASE_comp_N <- glmmTMB(RASEAndelGynnsam_mean ~ 
                                              scale(Älgtäthet.i.vinterstam_mean) +
                                              # scale(ungulate_index_mean) +
                                              scale(AntalTallarHa_mean) +
                                              scale(AntalBjorkarHa_mean) +
                                              scale(AndelRojt...18_mean) +
                                              scale(proportion_young_forest_mean) +
                                              scale(prop_snow_30_plus_imputed_mean),
                                            family = beta_family(link = "logit"),
                                            data = RASE_data_Norrland)

summary(glm_RASE_comp_N)

# Check for over dispersion
glm_RASE_comp_N_simres <- simulateResiduals(glm_RASE_comp_N)
testDispersion(glm_RASE_comp_N_simres)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_comp_null <- glmmTMB(RASEAndelGynnsam_mean ~ 1,  # Model with no predictors
                              family = beta_family(link = "logit"),
                              data = RASE_data_Norrland)

# Fit a forward stepwise model
glm_RASE_comp_N_fwd <- step(glm_RASE_comp_null, 
                          scope = list(lower = glm_RASE_comp_null, 
                                       upper = ~ 
                                         scale(Älgtäthet.i.vinterstam_mean) +
                                         # scale(ungulate_index_mean) +
                                         scale(AntalTallarHa_mean) +
                                         scale(AntalBjorkarHa_mean) +
                                         scale(AndelRojt...18_mean) +
                                         scale(proportion_young_forest_mean) +
                                         scale(prop_snow_30_plus_imputed_mean),
                                       direction = "forward", 
                                       trace = TRUE))

summary(glm_RASE_comp_N_fwd)

# Check for over dispersion
glm_RASE_comp_N_simres <- simulateResiduals(glm_RASE_comp_N_fwd)
testDispersion(glm_RASE_comp_N_simres)

## Svealand
RASE_data_Svealand <- RASE_data_abin_3_point_avg %>%
  filter(LandsdelNamn %in% c("Svealand"))

# Create a correlation matrix
cor_matrix_svea <- cor(RASE_data_Svealand[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                              "BestHojdAbinAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                              "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", "BestHojdAbinArealV_mean", # Site
                                              "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                              "prop_snow_30_plus_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_svea <- cor_matrix_svea
filtered_cor_svea[abs(filtered_cor_svea) <= 0.7 | abs(filtered_cor_svea) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_svea

# Run the model
glm_RASE_comp_S <- glmmTMB(RASEAndelGynnsam_mean ~ 
                             scale(Älgtäthet.i.vinterstam_mean) +
                             scale(ungulate_index_mean) +
                             scale(AntalTallarHa_mean) +
                             scale(AntalBjorkarHa_mean) +
                             scale(AndelRojt...18_mean) +
                             scale(proportion_young_forest_mean) +
                             scale(prop_snow_30_plus_imputed_mean),
                           family = beta_family(link = "logit"),
                           data = RASE_data_Svealand)

summary(glm_RASE_comp_S)

# Check for over dispersion
glm_RASE_comp_S_simres <- simulateResiduals(glm_RASE_comp_S)
testDispersion(glm_RASE_comp_S_simres)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_comp_null <- glmmTMB(RASEAndelGynnsam_mean ~ 1,  # Model with no predictors
                              family = beta_family(link = "logit"),
                              data = RASE_data_Svealand)

# Fit a forward stepwise model
glm_RASE_comp_S_fwd <- step(glm_RASE_comp_null, 
                            scope = list(lower = glm_RASE_comp_null, 
                                         upper = ~ 
                                           scale(Älgtäthet.i.vinterstam_mean) +
                                           scale(ungulate_index_mean) +
                                           scale(AntalGranarHa_mean) +
                                           scale(AntalBjorkarHa_mean) +
                                           scale(AndelRojt...18_mean) +
                                           scale(proportion_young_forest_mean) +
                                           scale(prop_snow_30_plus_imputed_mean),
                                         direction = "forward", 
                                         trace = TRUE))

summary(glm_RASE_comp_S_fwd)

# Check for over dispersion
glm_RASE_comp_N_simres <- simulateResiduals(glm_RASE_comp_N_fwd)
testDispersion(glm_RASE_comp_N_simres)

# Compare AIC
AIC(glm_RASE_comp_S_fwd, glm_RASE_comp_S)

## Götaland
RASE_data_Gotaland <- RASE_data_abin_3_point_avg %>%
  filter(LandsdelNamn %in% c("Götaland"))

# Create a correlation matrix
cor_matrix_gota <- cor(RASE_data_Gotaland[, c("Älgtäthet.i.vinterstam_mean", "ungulate_index_mean", "WB1000_mean", "Roe1000_mean", "FD1000_mean", "Red1000_mean", "WB1000_mean", # Browsers
                                              "BestHojdAbinAVG_mean", "BestandAlder_mean", "Medelbestandshojd_mean", "AndelRojt...18_mean", # Site
                                              "AndelBordigaMarker_mean", "youngforest_area_ha_mean", "proportion_young_forest_mean", "BestHojdAbinArealV_mean", # Site
                                              "AntalGranarHa_mean", "AntalTallarHa_mean", "AntalBjorkarHa_mean", "AntalOvrigtHa_mean", # Competitor species
                                              "prop_snow_30_plus_imputed_mean", "Mean_seasonal_precipitation[mm]_imputed_mean","mean_seasonal_snowdepth[cm]_imputed_mean")], # Climate
                       method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_gota <- cor_matrix_gota
filtered_cor_gota[abs(filtered_cor_gota) <= 0.7 | abs(filtered_cor_gota) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_gota

# Run the model
glm_RASE_comp_G <- glmmTMB(RASEAndelGynnsam_mean ~ 
                             # scale(Älgtäthet.i.vinterstam_mean) +
                             scale(ungulate_index_mean) +
                             scale(AntalGranarHa_mean) +
                             scale(AntalBjorkarHa_mean) +
                             scale(AndelRojt...18_mean) +
                             scale(proportion_young_forest_mean) +
                             scale(prop_snow_30_plus_imputed_mean),
                           family = beta_family(link = "logit"),
                           data = RASE_data_Gotaland)

summary(glm_RASE_comp_G)

# Check for over dispersion
glm_RASE_comp_G_simres <- simulateResiduals(glm_RASE_comp_G)
testDispersion(glm_RASE_comp_G_simres)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_comp_null <- glmmTMB(RASEAndelGynnsam_mean ~ 1,  # Model with no predictors
                              family = beta_family(link = "logit"),
                              data = RASE_data_Gotaland)

# Fit a forward stepwise model
glm_RASE_comp_G_fwd <- step(glm_RASE_comp_null, 
                            scope = list(lower = glm_RASE_comp_null, 
                                         upper = ~ 
                                           # scale(Älgtäthet.i.vinterstam_mean) +
                                           scale(ungulate_index_mean) +
                                           scale(AntalGranarHa_mean) +
                                           scale(AntalBjorkarHa_mean) +
                                           scale(AndelRojt...18_mean) +
                                           scale(proportion_young_forest_mean) +
                                           scale(prop_snow_30_plus_imputed_mean),
                                         direction = "forward", 
                                         trace = TRUE))

summary(glm_RASE_comp_G_fwd)

# Check for over dispersion
glm_RASE_comp_G_simres <- simulateResiduals(glm_RASE_comp_G_fwd)
testDispersion(glm_RASE_comp_G_simres)

## RASE competitive change calc ####

# Remove rows with NA values
RASE_data_NA <- na.omit(RASE_data)

# Calculate 3-point averages for 2020-2024
RASE_data_last_3_point_avg <- RASE_data_NA %>%
  filter(InvAr >= 2020 & InvAr <= 2024) %>%
  group_by(Registreri, LandsdelNamn, LanNamn) %>%
  arrange(Registreri, desc(InvAr)) %>%
  slice_head(n = 3) %>%
  summarise(across(where(is.numeric) & !any_of("InvAr"), list(mean = ~mean(.x, na.rm = TRUE),
                                                              sd = ~sd(.x, na.rm = TRUE))),
            years_used = paste(InvAr, collapse = ", ")) %>%
  ungroup()

# Calculate 3-point averages for 2015-2019
RASE_data_first_3_point_avg <- RASE_data_NA %>%
  filter(InvAr >= 2015 & InvAr <= 2019) %>%
  group_by(Registreri, LandsdelNamn, LanNamn) %>%
  arrange(Registreri, InvAr) %>%
  slice_head(n = 3) %>%
  summarise(across(where(is.numeric) & !any_of("InvAr"), list(mean = ~mean(.x, na.rm = TRUE),
                                                              sd = ~sd(.x, na.rm = TRUE))),
            years_used = paste(InvAr, collapse = ", ")) %>%
  ungroup()

# Merge datasets
RASE_data_change <- RASE_data_last_3_point_avg %>%
  left_join(RASE_data_first_3_point_avg, by = c("Registreri", "LandsdelNamn", "LanNamn"),
            suffix = c("_2020_2024", "_2015_2019")) %>%
  mutate(
    first_year_2024 = as.numeric(substr(years_used_2020_2024, 1, 4)),
    last_year_2024 = as.numeric(substr(years_used_2020_2024, nchar(years_used_2020_2024) - 3, nchar(years_used_2020_2024))),
    first_year_2019 = as.numeric(substr(years_used_2015_2019, 1, 4)),
    last_year_2019 = as.numeric(substr(years_used_2015_2019, nchar(years_used_2015_2019) - 3, nchar(years_used_2015_2019))),
    midpoint_year_2024 = (first_year_2024 + last_year_2024) / 2,
    midpoint_year_2019 = (first_year_2019 + last_year_2019) / 2,
    year_diff = midpoint_year_2024 - midpoint_year_2019
  )


RASE_data_change <- RASE_data_change %>%
  mutate(
    # Calculate absolute change
    across(contains("mean_2015_2019"),
           ~ (RASE_data_change[[sub("_2015_2019", "_2020_2024", cur_column())]] - .),
           .names = "abs_change_{.col}"),
    
    # Calculate yearly absolute change
    across(starts_with("abs_change"), ~ . / year_diff, .names = "yrly_{.col}")
  ) %>%
  rename_with(~ sub("_mean_2015_2019", "", .), starts_with("abs_change")) %>%
  rename_with(~ sub("_mean_2015_2019", "", .), starts_with("yrly_abs_change"))

RASE_data_change <- RASE_data_change %>%
  mutate(
    # Calculate percentage change
    across(contains("mean_2015_2019"),
           ~ 100 * (RASE_data_change[[sub("_2015_2019", "_2020_2024", cur_column())]] - .) / .,
           .names = "pct_{.col}"),
    
    # Calculate yearly percentage change
    across(starts_with("pct_"), ~ . / year_diff, .names = "yrly_{.col}")
  ) %>%
  rename_with(~ sub("_mean_2015_2019", "", .), starts_with("pct_")) %>%
  rename_with(~ sub("_mean_2015_2019", "", .), starts_with("yrly_"))

# Remove infinite and NaN values
RASE_data_change <- RASE_data_change %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

# Filter change data so that it only includes relvant varialbes 
RASE_data_change <- RASE_data_change %>%
  dplyr::select(LandsdelNamn, LanNamn, Registreri,
         yrly_pct_RASEAndelGynnsam, 
         yrly_pct_Älgtäthet.i.vinterstam, 
         yrly_pct_ungulate_index, 
         yrly_pct_AntalTallarHa,
         yrly_pct_AntalGranarHa,
         yrly_pct_AntalBjorkarHa, 
         yrly_pct_AndelRojt...18, 
         yrly_pct_proportion_young_forest)

# Create a correlation matrix
cor_matrix_change <- cor(RASE_data_change[, c("yrly_pct_Älgtäthet.i.vinterstam", "yrly_pct_ungulate_index", # Browsers
                                              "yrly_pct_proportion_young_forest", "yrly_pct_AndelRojt...18", # Site
                                              "yrly_pct_AntalGranarHa", "yrly_pct_AntalTallarHa", "yrly_pct_AntalBjorkarHa")], # Competitor species
                         method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_change <- cor_matrix_change
filtered_cor_change[abs(filtered_cor_change) <= 0.7 | abs(filtered_cor_change) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_change


## RASE at competitive height percent change national #### 
library(dplyr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(car)
library(DHARMa)
library(MASS)

# Remove rows with NA values from RASE_data_change
RASE_data_change <- na.omit(RASE_data_change)

# Run the model
glm_RASE_comp_chg <- glm(yrly_pct_RASEAndelGynnsam ~ 
                           scale(yrly_pct_Älgtäthet.i.vinterstam) +
                           scale(yrly_pct_ungulate_index) +
                           scale(yrly_pct_AntalTallarHa) +
                           scale(yrly_pct_AntalBjorkarHa) +
                           scale(yrly_pct_AndelRojt...18) +
                           scale(yrly_pct_proportion_young_forest),
                         family = gaussian(link = "identity"),
                         data = RASE_data_change)

summary(glm_RASE_comp_chg)

# Check for over dispersion
glm_RASE_comp_chg_simres <- simulateResiduals(glm_RASE_comp_chg)
testDispersion(glm_RASE_comp_chg_simres)

# Check for normality of residuals (less imprtant is not over-dispersed)
shapiro.test(residuals(glm_RASE_comp_chg))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_comp_chg)); qqline(residuals(glm_RASE_comp_chg), col = "red")  # Q-Q plot

# Backward stepwise selection
glm_RASE_comp_chg_bck <- step(glm_RASE_comp_chg, direction = "backward", trace = TRUE) # same result as drop1
summary(glm_RASE_comp_chg_bck)

# Check for over dispersion
bck_glm_RASE_comp_chg_simres <- simulateResiduals(glm_RASE_comp_chg_bck)
testDispersion(bck_glm_RASE_comp_chg_simres)

# Check for normality of residuals
shapiro.test(residuals(glm_RASE_comp_chg_bck))  # Shapiro-Wilk test
qqnorm(residuals(glm_RASE_comp_chg_bck)); qqline(residuals(glm_RASE_comp_chg_bck), col = "red")  # Q-Q plot

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_comp_chg_null <- glm(yrly_pct_RASEAndelGynnsam ~ 1,  # Model with no predictors
                              family = gaussian(link = "identity"),
                              data = RASE_data_change)

# Fit a forward stepwise model
glm_RASE_comp_chg_fwd <- step(glm_RASE_comp_chg_null, 
                          scope = list(lower = glm_RASE_comp_chg_null, 
                                       upper = ~ 
                                         scale(yrly_pct_Älgtäthet.i.vinterstam) +
                                         scale(yrly_pct_ungulate_index) +
                                         scale(yrly_pct_AntalTallarHa) +
                                         scale(yrly_pct_AntalBjorkarHa) +
                                         scale(yrly_pct_AndelRojt...18) +
                                         scale(yrly_pct_proportion_young_forest),
                                       direction = "forward", 
                                       trace = TRUE))

summary(glm_RASE_comp_chg_fwd)

# Compare AIC of all model
AIC(glm_RASE_comp_chg, glm_RASE_comp_chg_fwd, glm_RASE_comp_chg_bck)

## RASE at competitive height percent change regions ####

# Remove rows with NA values from RASE_data_change
RASE_data_change <- na.omit(RASE_data_change)

# Take RASE_data_change and filter for regions
## Norrland
RASE_change_Norrland <- RASE_data_change %>%
  filter(LandsdelNamn %in% c("Södra Norrland", "Norra Norrland"))

# Create a correlation matrix
cor_matrix_change_N <- cor(RASE_change_Norrland[, c("yrly_pct_Älgtäthet.i.vinterstam", "yrly_pct_ungulate_index", # Browsers
                                              "yrly_pct_proportion_young_forest", "yrly_pct_AndelRojt...18", # Site
                                              "yrly_pct_AntalGranarHa", "yrly_pct_AntalTallarHa", "yrly_pct_AntalBjorkarHa")], # Competitor species
                         method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_change_N <- cor_matrix_change_N
filtered_cor_change_N[abs(filtered_cor_change_N) <= 0.7 | abs(filtered_cor_change_N) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_change_N

# Run the model
glm_RASE_comp_chg_N <- glm(yrly_pct_RASEAndelGynnsam ~ 
                             scale(yrly_pct_Älgtäthet.i.vinterstam) +
                             scale(yrly_pct_ungulate_index) +
                             scale(yrly_pct_AntalTallarHa) +
                             scale(yrly_pct_AntalBjorkarHa) +
                             scale(yrly_pct_AndelRojt...18) +
                             scale(yrly_pct_proportion_young_forest),
                           family = gaussian(link = "identity"),
                           data = RASE_change_Norrland)

summary(glm_RASE_comp_chg_N)

# Check for over dispersion
glm_RASE_comp_chg_N_simres <- simulateResiduals(glm_RASE_comp_chg_N)
testDispersion(glm_RASE_comp_chg_N_simres)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_comp_chg_N_null <- glm(yrly_pct_RASEAndelGynnsam ~ 1,  # Model with no predictors
                              family = gaussian(link = "identity"),
                              data = RASE_change_Norrland)

# Fit a forward stepwise model
glm_RASE_comp_chg_N_fwd <- step(glm_RASE_comp_chg_N_null, 
                            scope = list(lower = glm_RASE_comp_chg_N_null, 
                                         upper = ~ 
                                           scale(yrly_pct_Älgtäthet.i.vinterstam) +
                                           scale(yrly_pct_ungulate_index) +
                                           scale(yrly_pct_AntalTallarHa) +
                                           scale(yrly_pct_AntalBjorkarHa) +
                                           scale(yrly_pct_AndelRojt...18) +
                                           scale(yrly_pct_proportion_young_forest),
                                         direction = "forward", 
                                         trace = TRUE))

summary(glm_RASE_comp_chg_N_fwd)

# Check for over dispersion
glm_RASE_comp_chg_N_simres <- simulateResiduals(glm_RASE_comp_chg_N_fwd)
testDispersion(glm_RASE_comp_chg_N_simres)

## Svealand
RASE_change_Svealand <- RASE_data_change %>%
  filter(LandsdelNamn %in% c("Svealand"))

# Create a correlation matrix
cor_matrix_change_S <- cor(RASE_change_Svealand[, c("yrly_pct_Älgtäthet.i.vinterstam", "yrly_pct_ungulate_index", # Browsers
                                                    "yrly_pct_proportion_young_forest", "yrly_pct_AndelRojt...18", # Site
                                                    "yrly_pct_AntalGranarHa", "yrly_pct_AntalTallarHa", "yrly_pct_AntalBjorkarHa")], # Competitor species
                           method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_change_S <- cor_matrix_change_S
filtered_cor_change_S[abs(filtered_cor_change_S) <= 0.7 | abs(filtered_cor_change_S) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_change_S

# Run the model
glm_RASE_comp_chg_S <- glm(yrly_pct_RASEAndelGynnsam ~ 
                             scale(yrly_pct_Älgtäthet.i.vinterstam) +
                             scale(yrly_pct_ungulate_index) +
                             scale(yrly_pct_AntalTallarHa) +
                             scale(yrly_pct_AntalBjorkarHa) +
                             scale(yrly_pct_AndelRojt...18) +
                             scale(yrly_pct_proportion_young_forest),
                           family = gaussian(link = "identity"),
                           data = RASE_change_Svealand)

summary(glm_RASE_comp_chg_S)

# Check for over dispersion
glm_RASE_comp_chg_S_simres <- simulateResiduals(glm_RASE_comp_chg_S)
testDispersion(glm_RASE_comp_chg_S_simres)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_comp_chg_S_null <- glm(yrly_pct_RASEAndelGynnsam ~ 1,  # Model with no predictors
                                family = gaussian(link = "identity"),
                                data = RASE_change_Svealand)

# Fit a forward stepwise model
glm_RASE_comp_chg_S_fwd <- step(glm_RASE_comp_chg_S_null, 
                                scope = list(lower = glm_RASE_comp_chg_S_null, 
                                             upper = ~ 
                                               scale(yrly_pct_Älgtäthet.i.vinterstam) +
                                               scale(yrly_pct_ungulate_index) +
                                               scale(yrly_pct_AntalTallarHa) +
                                               scale(yrly_pct_AntalBjorkarHa) +
                                               scale(yrly_pct_AndelRojt...18) +
                                               scale(yrly_pct_proportion_young_forest),
                                             direction = "forward", 
                                             trace = TRUE))

summary(glm_RASE_comp_chg_S_fwd)

# Check for over dispersion
glm_RASE_comp_chg_S_simres <- simulateResiduals(glm_RASE_comp_chg_S_fwd)
testDispersion(glm_RASE_comp_chg_S_simres)

## Götaland
RASE_change_Gotaland <- RASE_data_change %>%
  filter(LandsdelNamn %in% c("Götaland"))

# Create a correlation matrix
cor_matrix_change_G <- cor(RASE_change_Gotaland[, c("yrly_pct_Älgtäthet.i.vinterstam", "yrly_pct_ungulate_index", # Browsers
                                                    "yrly_pct_proportion_young_forest", "yrly_pct_AndelRojt...18", # Site
                                                    "yrly_pct_AntalGranarHa", "yrly_pct_AntalTallarHa", "yrly_pct_AntalBjorkarHa")], # Competitor species
                           method = "pearson", use = "pairwise.complete.obs")

# Filter correlations greater than 0.7 or less than -0.7, excluding 1
filtered_cor_change_G <- cor_matrix_change_G
filtered_cor_change_G[abs(filtered_cor_change_G) <= 0.7 | abs(filtered_cor_change_G) == 1] <- NA

# View the filtered correlation matrix
filtered_cor_change_G

# Run the model
glm_RASE_comp_chg_G <- glm(yrly_pct_RASEAndelGynnsam ~ 
                             scale(yrly_pct_Älgtäthet.i.vinterstam) +
                             scale(yrly_pct_ungulate_index) +
                             scale(yrly_pct_AntalTallarHa) +
                             scale(yrly_pct_AntalBjorkarHa) +
                             scale(yrly_pct_AndelRojt...18) +
                             scale(yrly_pct_proportion_young_forest),
                           family = gaussian(link = "identity"),
                           data = RASE_change_Gotaland)

summary(glm_RASE_comp_chg_G)

# Check for over dispersion
glm_RASE_comp_chg_G_simres <- simulateResiduals(glm_RASE_comp_chg_G)
testDispersion(glm_RASE_comp_chg_G_simres)

# USE step() for forwards stepwise selection
# Fit an initial empty model
glm_RASE_comp_chg_G_null <- glm(yrly_pct_RASEAndelGynnsam ~ 1,  # Model with no predictors
                                family = gaussian(link = "identity"),
                                data = RASE_change_Gotaland)

# Fit a forward stepwise model
glm_RASE_comp_chg_G_fwd <- step(glm_RASE_comp_chg_G_null, 
                                scope = list(lower = glm_RASE_comp_chg_G_null, 
                                             upper = ~ 
                                               scale(yrly_pct_Älgtäthet.i.vinterstam) +
                                               scale(yrly_pct_ungulate_index) +
                                               scale(yrly_pct_AntalTallarHa) +
                                               scale(yrly_pct_AntalBjorkarHa) +
                                               scale(yrly_pct_AndelRojt...18) +
                                               scale(yrly_pct_proportion_young_forest),
                                             direction = "forward", 
                                             trace = TRUE))

summary(glm_RASE_comp_chg_G_fwd)

# Check for over dispersion
glm_RASE_comp_chg_G_simres <- simulateResiduals(glm_RASE_comp_chg_G_fwd)
testDispersion(glm_RASE_comp_chg_G_simres)

## Create summary table of all models ####

library(insight) # needed for sjPlot
library(sjPlot) # needed for tab_model for summary table of glmmTMB results

# RASE per Ha.
tab_model(glm_RASE_Ha_fwd, glm_RASE_Ha_N_fwd, glm_RASE_Ha_S_fwd, glm_RASE_Ha_G_fwd,
          transform = NULL, 
          show.ci = FALSE, 
          show.se = TRUE, 
          show.aic = TRUE,
          show.stat = TRUE,
          #show.bic = TRUE,
          #show.icc = FALSE,
          file = "~/GitHub/Moose-Targets/Tables/RASE_Ha_abin_table.html")

# RASE comp.
tab_model(glm_RASE_comp_fwd, glm_RASE_comp_N_fwd, glm_RASE_comp_S_fwd, glm_RASE_comp_G_fwd,
          transform = NULL, 
          show.ci = FALSE, 
          show.se = TRUE, 
          show.aic = TRUE,
          show.stat = TRUE,
          #show.bic = TRUE,
          #show.icc = FALSE,
          file = "~/GitHub/Moose-Targets/Tables/RASE_comp_abin_table.html")

# Change in RASE comp.
tab_model(glm_RASE_comp_chg_fwd, glm_RASE_comp_chg_N_fwd, glm_RASE_comp_chg_S_fwd, glm_RASE_comp_chg_G_fwd,
          transform = NULL, 
          show.ci = FALSE, 
          show.se = TRUE, 
          show.aic = TRUE,
          show.stat = TRUE,
          #show.bic = TRUE,
          #show.icc = FALSE,
          file = "~/GitHub/Moose-Targets/Tables/RASE_comp_change_abin_table.html")
