## prerequisites
library(ggplot2)
library(dplyr)

#import the data #
library(readr)
X2008_2024_ABIN <- read_delim("~/GitHub/SEPA-report-AROW/2008_2024_ABIN.csv", 
                              delim = ";", escape_double = FALSE, col_types = cols(`Half Height` = col_double()), 
                              locale = locale(decimal_mark = ",", encoding = "WINDOWS-1252"), 
                              trim_ws = TRUE)
View(X2008_2024_ABIN)

## Data summary function to find means and various SE data ####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  length2 <- function (x, na.rm=TRUE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)
  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Number of stems per ha ####

# # Select only data from 2023-2024 and clean up area names
data_23_24 <- X2008_2024_ABIN %>%
  filter(Year %in% c(2023, 2024)) %>%
  mutate(Area = case_when(
    Area == "Fredrika E" ~ "Fredrika",
    Area == "Fredrika M" ~ "Fredrika",
    Area == "Fredrika S" ~ "Fredrika",
    Area == "Fredrika W" ~ "Fredrika",
    Area == "Fågelåsen" ~ "Lofsdalen",
    Area == "Öster malma" ~ "Öster Malma",
    Area == "ÖsterMalma" ~ "Öster Malma",
    Area == "Åtvideberg" ~ "Åtvidaberg",
    TRUE ~ Area  # Keep other areas unchanged
  ))


# Define conversion factor from 38.48 m² to hectares
conversion_factor <- 259.8752598752599

# Replace NA values with 0 for the relevant columns and convert to per hectare
data_per_hectare <- data_23_24 %>%
  mutate(
    `Rowan Total` = ifelse(is.na(`Rowan Total`), 0, `Rowan Total` * conversion_factor),
    `Aspen Total` = ifelse(is.na(`Aspen Total`), 0, `Aspen Total` * conversion_factor),
    `Salix Total` = ifelse(is.na(`Salix Total`), 0, `Salix Total` * conversion_factor),
    `Oak Total` = ifelse(is.na(`Oak Total`), 0, `Oak Total` * conversion_factor)
  )

# Create a total column for RASE
# Calculate the total for each region by adding the averages
data_per_hectare <- data_per_hectare %>%
  mutate(RASE_total = `Rowan Total` + `Aspen Total` + `Salix Total` + `Oak Total`)

# Define the desired order of the regions
desired_order <- c("Växjö", "Åtvidaberg", "Öster Malma", "Barksätter", 
                   "Fågelåsen", "Furudal", "Ljusdal", "Lofsdalen", 
                   "Nordmaling", "Fredrika", "Lycksele", "Sorsele", "Råneå")

# Filter out rows with NA in Area and reorder the Area factor
data_per_hectare <- data_per_hectare %>%
  filter(!is.na(Area)) %>%
  mutate(Area = factor(Area, levels = desired_order))

# Calculate the summary statistics for total RASE stems per hectare by region
region_summary <- summarySE(data_per_hectare, 
                            measurevar = "RASE_total", 
                            groupvars = c("Area"),  # Group by Area and Year if needed
                            na.rm = TRUE)

# Plotting the region totals with confidence intervals
# One can also use "x = reorder(Area, -RASE_total)" if we want largest to smallest
ggplot(region_summary, aes(x = Area, y = RASE_total)) +
  geom_bar(stat = "identity", colour = "black", fill = "white") +  # Bars for total values
  geom_errorbar(aes(ymin = RASE_total - ci, ymax = RASE_total + ci), width = 0.0, 
                linetype = 1, colour = "black") +  # Error bars for CIs
  geom_hline(yintercept = c(200, 400), linetype = 2, color = "darkorange") +
  labs(title = "RASE stems per ha. by region 2023-2024",
       x = "Region",
       y = "RASE per hectare") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

## Export with ggsave (change file name/path depending on species)
ggsave("RASE stems per region.tiff", plot = last_plot(), device = NULL, path = "~/GitHub/SEPA-report-AROW/Moose",
       scale = 1, width = 11, height = 11, dpi = 400, limitsize = TRUE, units = "cm")

## Analysis for each species

# Calculate the summary statistics for total rowan stems per hectare by region
rowan_summary <- summarySE(data_per_hectare, 
                            measurevar = "Rowan Total", 
                            groupvars = c("Area"),  # Group by Area and Year if needed
                            na.rm = TRUE)

# Calculate the summary statistics for total aspen stems per hectare by region
aspen_summary <- summarySE(data_per_hectare, 
                           measurevar = "Aspen Total", 
                           groupvars = c("Area"),  # Group by Area and Year if needed
                           na.rm = TRUE)

# Calculate the summary statistics for total salix stems per hectare by region
salix_summary <- summarySE(data_per_hectare, 
                           measurevar = "Salix Total", 
                           groupvars = c("Area"),  # Group by Area and Year if needed
                           na.rm = TRUE)

# Calculate the summary statistics for total oak stems per hectare by region
oak_summary <- summarySE(data_per_hectare, 
                           measurevar = "Oak Total", 
                           groupvars = c("Area"),  # Group by Area and Year if needed
                           na.rm = TRUE)

# Plot for rowan totals with confidence intervals
ggplot(rowan_summary, aes(x = Area, y = `Rowan Total`)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bars for total values
  geom_errorbar(aes(ymin = `Rowan Total` - ci, ymax = `Rowan Total` + ci), width = 0.2) +  # Error bars for CIs
  labs(title = "Average rowan stems per ha. by region 2023-2024",
       x = "Region",
       y = "RASE per hectare") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility


# Plot for aspen totals with confidence intervals
ggplot(aspen_summary, aes(x = Area, y = `Aspen Total`)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bars for total values
  geom_errorbar(aes(ymin = `Aspen Total` - ci, ymax = `Aspen Total` + ci), width = 0.2) +  # Error bars for CIs
  labs(title = "Average aspen stems per ha. by region 2023-2024",
       x = "Region",
       y = "RASE per hectare") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Plot for salix totals with confidence intervals
ggplot(salix_summary, aes(x = Area, y = `Salix Total`)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bars for total values
  geom_errorbar(aes(ymin = `Salix Total` - ci, ymax = `Salix Total` + ci), width = 0.2) +  # Error bars for CIs
  labs(title = "Average salix stems per ha. by region 2023-2024",
       x = "Region",
       y = "RASE per hectare") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

# Plot for oak totals with confidence intervals
ggplot(oak_summary, aes(x = Area, y = `Oak Total`)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bars for total values
  geom_errorbar(aes(ymin = `Oak Total` - ci, ymax = `Oak Total` + ci), width = 0.2) +  # Error bars for CIs
  labs(title = "Average oak stems per ha. by region 2023-2024",
       x = "Region",
       y = "RASE per hectare") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility

## Stems at competitive height ####

detach("package:plyr", unload = TRUE)

## Select only data from 2023-2024 and clean up area names
data_23_24 <- X2008_2024_ABIN %>%
  filter(Year %in% c(2023, 2024)) %>%
  mutate(Area = case_when(
    Area == "Fredrika E" ~ "Fredrika",
    Area == "Fredrika M" ~ "Fredrika",
    Area == "Fredrika S" ~ "Fredrika",
    Area == "Fredrika W" ~ "Fredrika",
    Area == "Fågelåsen" ~ "Lofsdalen",
    Area == "Öster malma" ~ "Öster Malma",
    Area == "ÖsterMalma" ~ "Öster Malma",
    Area == "Åtvideberg" ~ "Åtvidaberg",
    TRUE ~ Area  # Keep other areas unchanged
  ))

# Define the desired order of the regions
desired_order <- c("Växjö", "Åtvidaberg", "Öster Malma", "Barksätter", 
                   "Furudal", "Ljusdal", "Lofsdalen", "Nordmaling", 
                   "Fredrika", "Lycksele", "Sorsele", "Råneå")

# Filter out rows with NA in Area and reorder the Area factor
data_23_24 <- data_23_24 %>%
  filter(!is.na(Area)) %>%
  mutate(Area = factor(Area, levels = desired_order))


# Create a new column that checks the condition for each height
data_23_24 <- data_23_24 %>%
  mutate(
    Condition_Met = ifelse(
      (`Rowan Height` >= 2 * `Half Height`) | 
        (`Aspen Height` >= 2 * `Half Height`) | 
        (`Salix Height` >= 2 * `Half Height`) | 
        (`Oak Height` >= 2 * `Half Height`), 
      1, 0  # 1 for TRUE, 0 for FALSE
    )
  )

# Calculate counts and percentages by Area using count() for total rows
height_analysis <- data_23_24 %>%
  group_by(Area) %>%
  summarise(
    Count_Met = sum(Condition_Met, na.rm = TRUE),  # Count where condition is met
    Total_Count = n(),                               # Total rows in the area
    Percentage_Met = (Count_Met / Total_Count) * 100,  # Calculate percentage
    .groups = 'drop'  # Drop the grouping after summarising
  )

# Display the results
print(height_analysis)

# Create a bar plot
ggplot(height_analysis, aes(x = Area, y = Percentage_Met)) +
  geom_bar(stat = "identity", colour = "black", fill = "white") +
  geom_hline(yintercept = c(5, 10), linetype = 2, color = "darkorange") +
  labs(title = "RASE at competative height",
       x = "Region",
       y = "Percentage of plots (%)") +
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

## Export with ggsave (change file name/path depending on species)
ggsave("Competative RASE stems.tiff", plot = last_plot(), device = NULL, path = "~/GitHub/SEPA-report-AROW/Moose",
       scale = 1, width = 11, height = 11, dpi = 800, limitsize = TRUE, units = "cm")

## Competitive height and browsing ####

detach("package:plyr", unload = TRUE)

## Select only data from 2023-2024 and clean up area names
data_23_24 <- X2008_2024_ABIN %>%
  filter(Year %in% c(2023, 2024)) %>%
  mutate(Area = case_when(
    Area == "Fredrika E" ~ "Fredrika",
    Area == "Fredrika M" ~ "Fredrika",
    Area == "Fredrika S" ~ "Fredrika",
    Area == "Fredrika W" ~ "Fredrika",
    Area == "Fågelåsen" ~ "Lofsdalen",
    Area == "Öster malma" ~ "Öster Malma",
    Area == "ÖsterMalma" ~ "Öster Malma",
    Area == "Åtvideberg" ~ "Åtvidaberg",
    TRUE ~ Area  # Keep other areas unchanged
  ))

# Define the desired order of the regions
desired_order <- c("Växjö", "Åtvidaberg", "Öster Malma", "Barksätter", 
                   "Furudal", "Ljusdal", "Lofsdalen", "Nordmaling", 
                   "Fredrika", "Lycksele", "Sorsele", "Råneå")

# Filter out rows with NA in Area and reorder the Area factor
data_23_24 <- data_23_24 %>%
  filter(!is.na(Area)) %>%
  mutate(Area = factor(Area, levels = desired_order))


# Create a new column that checks the condition for each height
data_23_24 <- data_23_24 %>%
  mutate(
    Condition_Met = ifelse(
      (`Rowan Height` >= 2 * `Half Height`) | 
        (`Aspen Height` >= 2 * `Half Height`) | 
        (`Salix Height` >= 2 * `Half Height`) | 
        (`Oak Height` >= 2 * `Half Height`), 
      1, 0  # 1 for TRUE, 0 for FALSE
    )
  )

# Calculate counts and percentages by Area using count() for total rows
height_analysis <- data_23_24 %>%
  group_by(Area) %>%
  summarise(
    Count_Met = sum(Condition_Met, na.rm = TRUE),  # Count where condition is met
    Total_Count = n(),                               # Total rows in the area
    Percentage_Met = (Count_Met / Total_Count) * 100,  # Calculate percentage
    .groups = 'drop'  # Drop the grouping after summarising
  )

# Create new binary columns for damage (1 = damaged, 0 = not damaged, NA for invalid/missing)
data_23_24 <- data_23_24 %>%
  mutate(
    Rowan_Damage_Binary = case_when(
      `Rowan Damaged` == "0" ~ 0,  # No damage if "0"
      !is.na(`Rowan Damaged`) & `Rowan Damaged` != "" ~ 1,  # Any non-NA and non-empty value means damaged
      TRUE ~ NA_real_  # Treat empty or NA as NA
    ),
    Aspen_Damage_Binary = case_when(
      `Aspen Damaged` == "0" ~ 0,
      !is.na(`Aspen Damaged`) & `Aspen Damaged` != "" ~ 1,
      TRUE ~ NA_real_
    ),
    Salix_Damage_Binary = case_when(
      `Salix Damaged` == "0" ~ 0,
      !is.na(`Salix Damaged`) & `Salix Damaged` != "" ~ 1,
      TRUE ~ NA_real_
    ),
    Oak_Damage_Binary = case_when(
      `Oak Damaged` == "0" ~ 0,
      !is.na(`Oak Damaged`) & `Oak Damaged` != "" ~ 1,
      TRUE ~ NA_real_
    )
  )

# Now calculate the percentages for damage using the new binary columns
damage_analysis <- data_23_24 %>%
  group_by(Area) %>%
  summarise(
    Rowan_Damage_Perc = (sum(Rowan_Damage_Binary, na.rm = TRUE) / sum(!is.na(`Rowan Total`))) * 100,
    Aspen_Damage_Perc = (sum(Aspen_Damage_Binary, na.rm = TRUE) / sum(!is.na(`Aspen Total`))) * 100,
    Salix_Damage_Perc = (sum(Salix_Damage_Binary, na.rm = TRUE) / sum(!is.na(`Salix Total`))) * 100,
    Oak_Damage_Perc = (sum(Oak_Damage_Binary, na.rm = TRUE) / sum(!is.na(`Oak Total`))) * 100,
    .groups = 'drop'
  )

# Display the results
print(damage_analysis)

# Combine the height analysis with the damage analysis
combined_analysis <- height_analysis %>%
  left_join(damage_analysis, by = "Area")

# Calculate the average damage across all species for each area
combined_data <- damage_analysis %>%
  rowwise() %>%
  mutate(Average_Damage_Perc = mean(c(Rowan_Damage_Perc, Aspen_Damage_Perc, Salix_Damage_Perc, Oak_Damage_Perc), na.rm = TRUE)) %>%
  ungroup()

# Merge the average damage data with height analysis data
combined_data <- merge(height_analysis, combined_data, by = "Area")

# Create a bar plot with average damage percentages as points
ggplot(combined_data, aes(x = Area, y = Percentage_Met)) +
  geom_bar(stat = "identity", colour = "black", fill = "white") + # Bar plot for the height data
  geom_hline(yintercept = c(5, 10), linetype = 2, color = "darkorange") +   # Horizontal reference lines
  geom_point(aes(y = Average_Damage_Perc), color = "black", shape = 8, size = 1, stroke = 1) +  # Star for average damage
  labs(title = "RASE at Competitive Height with Average Damage",
       x = "Region",
       y = "Percentage of Plots (%)") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

# Export with ggsave (adjust filename/path as needed)
ggsave("Competative_RASE_with_Average_Damage.tiff", plot = last_plot(), 
       path = "~/GitHub/SEPA-report-AROW/Moose",
       scale = 1, width = 11, height = 11, dpi = 400, units = "cm")

# Perform a linear regression
regression_model <- lm(Percentage_Met ~ Average_Damage_Perc, data = combined_data)

# Summary of the regression model
summary(regression_model)

# Get the R-squared and p-value from the regression model
r_squared <- summary(regression_model)$r.squared
p_value <- summary(regression_model)$coefficients[2, 4]

# Plot the regression along with the data points
ggplot(combined_data, aes(x = Average_Damage_Perc, y = Percentage_Met)) +
  geom_smooth(method = "lm", color = "darkorange", se = TRUE) +  # Regression line with confidence interval
  geom_point(size = 3, color = "black") +  # Points for each Area
  labs(title = "",
       x = "Average Damage Percentage (%)",
       y = "Percentage of Plots Meeting Height Criteria (%)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  
# Add annotations for R-squared and p-value
  annotate("text", x = 0, y = 14, 
           label = paste("R² =", round(r_squared, 2)), size = 5, color = "black", hjust = 0) +
  annotate("text", x = 0, y = 13, 
           label = paste("p-value =", format.pval(p_value, digits = 1)), size = 5, color = "black", hjust = 0)

# Export the regression plot (adjust filename/path as needed)
ggsave("Competative_RASE_vs_Average_Damage_Regression.tiff", plot = last_plot(), 
       path = "~/GitHub/SEPA-report-AROW/Moose",
       scale = 1, width = 11, height = 11, dpi = 400, units = "cm")
