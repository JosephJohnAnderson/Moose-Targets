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
                AntalRASEHa, RASEAndelGynnsam) # Independent variables 

# See which variables have most NA and consider removing them
sort(colSums(is.na(RASE_data_plotting)), decreasing = TRUE)

## Three point averages ####

# First three-point average using all data (2015-2024)
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

# Last three-point average using all data (2020-2024)
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

# Change between the periods
RASE_change <- RASE_first %>%
  select(Registreri, LandsdelNamn, LanNamn, 
         AntalRASEHa_mean_first = AntalRASEHa_mean, 
         RASEAndelGynnsam_mean_first = RASEAndelGynnsam_mean) %>%
  inner_join(
    RASE_last %>%
      select(Registreri, 
             AntalRASEHa_mean_last = AntalRASEHa_mean, 
             RASEAndelGynnsam_mean_last = RASEAndelGynnsam_mean),
    by = "Registreri"
  ) %>%
  mutate(
    Change_RASEHa = AntalRASEHa_mean_last - AntalRASEHa_mean_first,  # Change for AntalRASEHa
    Change_RASEAndelGynnsam = RASEAndelGynnsam_mean_last - RASEAndelGynnsam_mean_first  # Change for RASEAndelGynnsam
  )


# View the result
head(RASE_last)
head(RASE_change)

## Create shape files ####
library(sf)
library(tmap)

# Get AFO shape file
AFO_shp <- st_read("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/Clipped MMA Shape File/updated_AFO_shapefile")

# Create shape file for current and target status maps 
AFO_RASE <- AFO_shp %>%
  left_join(RASE_last, by = "Registreri")

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
      values = "blues",# Auto blue gradient
      breaks = c(0, 200, 400, 800, 1600),
      value.na = "grey"   # Color for NA values
    ),
    fill.legend = tm_legend(title = "RASE stems per ha.")
  ) +
  tm_title("RASE density 2022/24", size = 1.0) +
  tm_shape(AFO_joined) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()
  ) +
  tm_borders(col = "black", lwd = 1.5)


RASEperHa_current

# Save the tmap object as a TIFF file
tmap_save(RASEperHa_current, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/RASE stems per hectare ÄFO 2022-2024.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")

# RASE per ha. target status
RASEperHa_target <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill("AntalRASEHa_mean", fill.scale = tm_scale(
    values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"), # Custom colour gradient
    breaks = c(0, 200, 400, 10000),
    label.na = "NA",
    value.na = "grey",
    labels = c("< 200", "200 – 400", "> 400") ),fill.legend = tm_legend(title = "RASE stems per ha.")) +
  tm_title("RASE density targets 2022/24", size = 1.0) +
  tm_shape(AFO_joined) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()) +
    tm_borders(col = "black", lwd = 1.5)

RASEperHa_target

# Save the tmap object as a TIFF file
tmap_save(RASEperHa_target, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/RASE stems per hectare ÄFO 2022-2024.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")

## Proportion RASE competitive ####

# RASE competitive current (gradient)
RASEcomp_current <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill(
    "RASEAndelGynnsam_mean", 
    fill.scale = tm_scale_intervals(
      values = "blues",# Auto blue gradient
      breaks = c(0, 0.05, 0.1, 0.25, 0.5),
      value.na = "grey"   # Color for NA values
    ),
    fill.legend = tm_legend(title = "RASE stems per ha.")
  ) +
  tm_title("RASE competitive 2022/24", size = 1.0) +
  tm_shape(AFO_joined) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()
  ) +
  tm_borders(col = "black", lwd = 1.5)


RASEcomp_current

# Save the tmap object as a TIFF file
tmap_save(RASEcomp_current, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/RASE stems per hectare ÄFO 2022-2024.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")

# RASE competitive target status
RASEcomp_target <- tm_shape(AFO_RASE) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill("RASEAndelGynnsam_mean", fill.scale = tm_scale(
    values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"), # Custom colour gradient
    breaks = c(0, 0.05, 0.1, 1),
    label.na = "NA",
    value.na = "grey",
    labels = c("< 5%", "5 – 10 %", "> 10 %") ),fill.legend = tm_legend(title = "RASE stems per ha.")) +
  tm_title("RASE competitive targets 2022/24", size = 1.0) +
  tm_shape(AFO_joined) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()) +
  tm_borders(col = "black", lwd = 1.5)

RASEcomp_target

# Save the tmap object as a TIFF file
tmap_save(RASEcomp_target, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/RASE stems per hectare ÄFO 2022-2024.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")


# RASE competitive change
RASEcomp_change_map <- tm_shape(AFO_RASE_change) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill(
    "Change_RASEAndelGynnsam", 
    fill.scale = tm_scale_intervals(
      values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),  # Diverging colors
      breaks = c(-0.1, -0.05, 0, 0.05, 0.1),  # Custom breakpoints
      labels = c("> -5%", "0 to -5 %", "0 to +5 %", "> +5 %"),
      value.na = "grey"  # Color for NA values
    ),
    fill.legend = tm_legend(title = "Change in competitive")
  ) +
  tm_title("Change in RASE competitive (2020–2024 vs. 2015–2019)", size = 1.0) +
  tm_shape(AFO_RASE_change) +
  tm_borders(col = "black", lwd = 1.5)

RASEcomp_change_map

# Save the tmap object as a TIFF file
tmap_save(RASEcomp_change_map, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/RASE stems per hectare ÄFO 2022-2024.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")
