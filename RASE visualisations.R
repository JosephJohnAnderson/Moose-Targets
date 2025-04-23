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

## Select variables for plotting from big data ####

# Select the data needed for plotting 
RASE_data_plotting <- Big_data %>%
  dplyr::select(LandsdelNamn,LanNamn,Registreri, # Regional data
                InvAr, # Year
                AndelMargraMarker, # Stand productive for target status
                AntalRASEHa, RASEAndelGynnsam) # Independent variables 

# See which variables have most NA and consider removing them
sort(colSums(is.na(RASE_data_plotting)), decreasing = TRUE)

## Three point averages ####

# First three-point average using five year data (2015-2019)
RASE_first <- RASE_data_plotting %>%
  filter(InvAr >= 2015 & InvAr <= 2019) %>%  # Keep only relevant years
  group_by(Registreri, LandsdelNamn, LanNamn) %>%
  arrange(Registreri, InvAr) %>%  # Sort in ascending order
  slice_head(n = 3) %>%  # Select earliest 3 years within range
  summarise(
    across(where(is.numeric) & !all_of("InvAr"), list(mean = ~mean(.x, na.rm = TRUE),
                                                      sd = ~sd(.x, na.rm = TRUE))),
    years_used = paste(InvAr, collapse = ", ")  # Keep track of years used
  ) %>%
  ungroup()

# Last three-point average using five year data (2020-2024)
RASE_last <- RASE_data_plotting %>%
  filter(InvAr >= 2020 & InvAr <= 2024) %>%  # Keep only relevant years
  group_by(Registreri, LandsdelNamn, LanNamn) %>%
  arrange(Registreri, desc(InvAr)) %>%  # Sort in descending order
  slice_head(n = 3) %>%  # Select latest 3 years within range
  summarise(
    across(where(is.numeric) & !all_of("InvAr"), list(mean = ~mean(.x, na.rm = TRUE),
                                                      sd = ~sd(.x, na.rm = TRUE))),
    years_used = paste(InvAr, collapse = ", ")  # Keep track of years used
  ) %>%
  ungroup()

# Last three-point average using all data (2015-2024)
RASE_abin <- RASE_data_plotting %>%
  filter(InvAr >= 2015 & InvAr <= 2024) %>%  # Keep only relevant years
  group_by(Registreri, LandsdelNamn, LanNamn) %>%
  arrange(Registreri, desc(InvAr)) %>%  # Sort in descending order
  slice_head(n = 3) %>%  # Select latest 3 years within range
  summarise(
    across(where(is.numeric) & !all_of("InvAr"), list(mean = ~mean(.x, na.rm = TRUE),
                                                      sd = ~sd(.x, na.rm = TRUE))),
    years_used = paste(InvAr, collapse = ", ")  # Keep track of years used
  ) %>%
  ungroup()

# Change between the periods
RASE_change <- RASE_first %>%
  rename(AntalRASEHa_mean_first = AntalRASEHa_mean, 
         RASEAndelGynnsam_mean_first = RASEAndelGynnsam_mean) %>%
  full_join(
    RASE_last %>%
      rename(AntalRASEHa_mean_last = AntalRASEHa_mean, 
             RASEAndelGynnsam_mean_last = RASEAndelGynnsam_mean),
    by = "Registreri"
  ) %>%
  mutate(
    Change_RASEHa = AntalRASEHa_mean_last - AntalRASEHa_mean_first,
    Change_RASEAndelGynnsam = RASEAndelGynnsam_mean_last - RASEAndelGynnsam_mean_first
  )

# View the result
head(RASE_abin)
head(RASE_change)

## Create shape files ####
library(sf)
library(tmap)

# Get AFO shape file
AFO_shp <- st_read("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Clipped MMA Shape File/updated_AFO_shapefile.shp")

# Create shape file for current and target status maps 
AFO_RASE <- AFO_shp %>%
  left_join(RASE_abin, by = "Registreri")

# Create shape file for change maps 
AFO_RASE_change <- AFO_shp %>%
  left_join(RASE_change, by = "Registreri")

## RASE per ha. ####

# RASE per ha. current (gradient)
RASEperHa_current <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill(
    "AntalRASEHa_mean", 
    fill.scale = tm_scale_intervals(
      values = "blues", # Auto blue gradient
      breaks = c(0, 199, 399, 799, 1600),
      value.na = "grey",
      labels = c("< 200", "200 till 399", "400 till 799", "800 till 1600")
    ),
    fill.legend = tm_legend(title = "Stammar per hektar") 
  ) +
  tm_title("RASE st/ha", size = 1.0) +
  tm_shape(AFO_RASE) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()
  ) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(
    legend.outside = FALSE, # Keep legend inside the map area
    legend.position = c(0.0, 1.0), # Adjust position (near top-left)
    legend.bg.color = "white", # Background color for visibility
    legend.bg.alpha = 0.0, # Fully transparent background
  )

RASEperHa_current

# Save the tmap object as a PNG file
tmap_save(RASEperHa_current, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/RASE_ha/Maps/RASE_stems_per_hectare_current_2022-2024.png",
          width = 7, height = 16, dpi = 300, units = "cm")

# RASE per ha. target status

# Simple map
RASEperHa_target <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill("AntalRASEHa_mean", fill.scale = tm_scale(
    values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"), # Custom colour gradient
    breaks = c(0, 199, 399, 10000),
    label.na = "NA",
    value.na = "grey",
    labels = c("< 200", "200 till 399", "> 400") ),
    fill.legend = tm_legend(title = "Stammar per hektar")) +
  tm_title("RASE st/ha målstatus", size = 1.0) +
  tm_shape(AFO_RASE) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()) +
    tm_borders(col = "black", lwd = 1.5)+
  tm_layout(
    legend.outside = FALSE, # Keep legend inside the map area
    legend.position = c(0.0, 1.0), # Adjust position (near top-left)
    legend.bg.color = "white", # Background color for visibility
    legend.bg.alpha = 0.0 # Transparent background
  )

RASEperHa_target

# Save the tmap object as a PNG file
tmap_save(RASEperHa_target, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/RASE_ha/Maps/RASE_stems_per_hectare_targets_2022-2024.png",
          width = 7, height = 16, dpi = 300, units = "cm")

# Add a new column with pass/fail depending on AndelMargraMarker_mean
AFO_RASE$target_status <- with(AFO_RASE, ifelse(
  AndelMargraMarker_mean > 0.4 & AntalRASEHa_mean >= 200, "Low productivity pass",
  ifelse(AndelMargraMarker_mean > 0.4 & AntalRASEHa_mean < 200, "Low productivity fail",
         ifelse(AndelMargraMarker_mean < 0.4 & AntalRASEHa_mean >= 400, "High productivity pass",
                "High productivity fail")))
)

# Target status map
RASEperHa_target_status <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill("target_status",
          fill.scale = tm_scale(
            values = c(
              "High productivity fail" = "#d7191c",
              "High productivity pass" = "#2c7bb6",
              "Low productivity fail" = "#fdae61",
              "Low productivity pass" = "#abd9e9"
            ),
            labels = c("Nej < 400", "Ja ≥ 400", "Nej < 200", "Ja ≥ 200"),
            na.value = "grey"
          ),
          fill.legend = tm_legend(
            title = "Uppfyllt mål?",
            show = TRUE,
            na.text = "NA"
          )
  ) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_title("RASE st/ha målstatus", size = 1.0) +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c(0.0, 1.0),
    legend.bg.color = "white",
    legend.bg.alpha = 0.0
  )

RASEperHa_target_status

# Save the tmap object as a PNG file
tmap_save(RASEperHa_target_status, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/RASE_ha/Maps/RASE_stems_per_hectare_targets_2022-2024.png",
          width = 7, height = 16, dpi = 300, units = "cm")

# Arrange the two maps in a row
RASEperHa_combined <- tmap_arrange(RASEperHa_current, RASEperHa_target_status, ncol = 2)
RASEperHa_combined

# Save the tmap object as a PNG file
tmap_save(RASEperHa_combined, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/RASE_ha/Maps/RASE_stems_per_hectare_combined.png",
          width = 14, height = 16, dpi = 300, units = "cm")


## Proportion RASE competitive ####

# RASE competitive current (gradient)
RASEcomp_current <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill(
    "RASEAndelGynnsam_mean", 
    fill.scale = tm_scale_intervals(
      values = "blues",# Auto blue gradient
      breaks = c(0, 0.05, 0.1, 0.2, 0.4),
      value.na = "grey",   # Color for NA values
      labels = c("< 5%", "5 till 10%", "10 till 20%", "20 till 40%")
    ),
    fill.legend = tm_legend(title = "Andel gynnsam") 
  ) +
  tm_title("RASE andel gynnsam", size = 1.0) +
  tm_shape(AFO_RASE) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()
  ) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(
    legend.outside = FALSE, # Keep legend inside the map area
    legend.position = c(0.0, 1.0), # Adjust position (near top-left)
    legend.bg.color = "white", # Background color for visibility
    legend.bg.alpha = 0.0 # Fully transparent background
  )


RASEcomp_current

# Save the tmap object as a PNG file
tmap_save(RASEcomp_current, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/RASE_comp/Maps/RASE_competative_status_current_2022-2024.png",
          width = 7, height = 16, dpi = 300, units = "cm")

# RASE competitive target status
RASEcomp_target <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill("RASEAndelGynnsam_mean", fill.scale = tm_scale(
    values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"), # Custom colour gradient
    breaks = c(0, 0.05, 0.1, 1),
    label.na = "NA",
    value.na = "grey",
    labels = c("< 5%", "5 till 10 %", "> 10 %") ),fill.legend = tm_legend(title = "Andel gynnsam")) +
  tm_title("RASE andel gynnsam målstatus", size = 1.0) +
  tm_shape(AFO_RASE) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()) +
  tm_borders(col = "black", lwd = 1.5)+
  tm_layout(
    legend.outside = FALSE, # Keep legend inside the map area
    legend.position = c(0.0, 1.0), # Adjust position (near top-left)
    legend.bg.color = "white", # Background color for visibility
    legend.bg.alpha = 0.0 # Transparent background
  )

RASEcomp_target

# Add a new column with pass/fail depending on AndelMargraMarker_mean
AFO_RASE$target_status <- with(AFO_RASE, ifelse(
  AndelMargraMarker_mean > 0.4 & RASEAndelGynnsam_mean >= 200, "Low productivity pass",
  ifelse(AndelMargraMarker_mean > 0.4 & RASEAndelGynnsam_mean < 200, "Low productivity fail",
         ifelse(AndelMargraMarker_mean < 0.4 & RASEAndelGynnsam_mean >= 400, "High productivity pass",
                "High productivity fail")))
)

# Target status map
RASEperHa_target_status <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill("target_status",
          fill.scale = tm_scale(
            values = c(
              "High productivity fail" = "#d7191c",
              "High productivity pass" = "#2c7bb6",
              "Low productivity fail" = "#fdae61",
              "Low productivity pass" = "#abd9e9"
            ),
            labels = c("Nej < 400", "Ja ≥ 400", "Nej < 200", "Ja ≥ 200"),
            na.value = "grey"
          ),
          fill.legend = tm_legend(
            title = "Uppfyllt mål?",
            show = TRUE,
            na.text = "NA"
          )
  ) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_title("RASE st/ha målstatus", size = 1.0) +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c(0.0, 1.0),
    legend.bg.color = "white",
    legend.bg.alpha = 0.0
  )

RASEcomp_target_status

# Save the tmap object as a PNG file
tmap_save(RASEcomp_target_status, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/RASE_comp/Maps/RASE_competative_status_targets_2022-2024.png",
          width = 7, height = 16, dpi = 300, units = "cm")

# RASE competitive change
RASEcomp_change_map <- tm_shape(AFO_RASE_change) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill(
    "Change_RASEAndelGynnsam", 
    fill.scale = tm_scale_intervals(
      values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),  # Diverging colors
      breaks = c(-0.9, -0.05, 0, 0.05, 0.9),  # Custom breakpoints
      labels = c("> -5%", "0 till -5 %", "0 till +5 %", "> +5 %"),
      value.na = "grey"  # Color for NA values
    ),
    fill.legend = tm_legend(title = "Förändringsandel")
  ) +
  tm_title("RASE förändringandel gynnsam", size = 1.0) +
  tm_shape(AFO_RASE_change) +
  tm_borders(col = "black", lwd = 1.5)+
  tm_layout(
    legend.outside = FALSE, # Keep legend inside the map area
    legend.position = c(0.0, 1.0), # Adjust position (near top-left)
    legend.bg.color = "white", # Background color for visibility
    legend.bg.alpha = 0.0 # Transparent background
  )

RASEcomp_change_map

# Save the tmap object as a PNG file
tmap_save(RASEcomp_change_map, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/RASE_comp/Maps/RASE_competative_status_change.png",
          width = 7, height = 16, dpi = 300, units = "cm")

# Arrange the three maps in a row
RASEcomp_combined <- tmap_arrange(RASEcomp_current, RASEcomp_target, RASEcomp_change_map, ncol = 3)
RASEcomp_combined

# Save the tmap object as a PNG file
tmap_save(RASEcomp_combined, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Results/Joseph/RASE_comp/Maps/RASE_competative_combined.png",
          width = 21, height = 16, dpi = 300, units = "cm")
