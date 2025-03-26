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
RASE_first <- RASE_data_NA %>%
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
RASE_last <- RASE_data_NA %>%
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

# View the result
head(RASE_last)

## Maps ####

AFO_shp <- st_read("~/GIS/ÄFO/updated_AFO_shapefile.shp")
AFO_joined <- AFO_shp %>%
  left_join(afo_summary, by = "Registreri")

# Plot with tmap
RASEperHa <- tm_shape(AFO_joined) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill("AntalRASEHa", fill.scale = tm_scale(
    values = c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"),
    breaks = c(0, 200, 400, 10000),
    label.na = "NA",
    value.na = "grey",
    labels = c("< 200", "200 – 400", "> 400") ),fill.legend = tm_legend(title = "")) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_title("RASE stems per ha. 2022/24", size = 1.0) +
  tm_shape(AFO_joined) +
  tm_fill(
    "Registreri",
    fill.scale = tm_scale(values = c("white", "white", "white")),
    fill_alpha = 0,
    fill.legend = tm_legend_hide()) +
  tm_borders(col = "black", lwd = 1.5)

RASEperHa

# Save the tmap object as a TIFF file
tmap_save(RASEperHa, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/RASE stems per hectare ÄFO 2022-2024.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")


# Change in RASE stems per hectare map
# Aggregate data for each period
afo_first_summary <- summarySE(data = AFO_data_first, 
                               measurevar = "AntalRASEHa", 
                               groupvars = "Registreri")

afo_last_summary <- summarySE(data = AFO_data_last, 
                              measurevar = "AntalRASEHa", 
                              groupvars = "Registreri")

# Rename the 'mean' column to reflect each period
afo_first_summary <- dplyr::rename(afo_first_summary, RASE_mean_first = AntalRASEHa)

afo_last_summary <- dplyr::rename(afo_last_summary, RASE_mean_last = AntalRASEHa)

# Calculate the change
afo_change <- afo_last_summary %>%
  left_join(afo_first_summary, by = "Registreri") %>%
  mutate(change = RASE_mean_last - RASE_mean_first)

# Join the change data to the shapefile
AFO_joined_change <- AFO_shp %>%
  left_join(afo_change, by = "Registreri")

# Map with distinct breaks
RASEChangeMap <- tm_shape(AFO_joined_change) +
  tm_graticules(alpha = 0.3, n.x = 3, n.y = 6) +
  tm_fill("change",
          textNA = "NA", 
          colorNA="#999999",
          title = "", 
          palette = c("#d73027", "orange", "#fee08b", "lightgreen", "#1a9850"),  # Red for decrease, yellow neutral, green for increase
          breaks = c(-Inf, -200, 0, 200, Inf),           # Define meaningful breaks
          labels = c("Decrease >200", "Decrease ≤200", "Increase ≤200", "Increase >200")) +
  tm_borders(col="black", lwd=1.5) +
  tm_layout(main.title ="Change 2018/20 to 2022/24", 
            title.size = 1.2, 
            legend.text.size = 1.0) +
  tm_scale_bar(position = c("right", "bottom"))

# Print the map
RASEChangeMap


# Save the tmap object as a TIFF file
tmap_save(RASEChangeMap, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/RASE stems per hectare change.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")