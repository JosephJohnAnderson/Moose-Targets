## prerequisites
library(ggplot2)
library(tmap)
library(sf)
library(dplyr)
library(readxl)

# Load the data
AFO_data <- read_excel("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SKS/SKS_ABIN.xlsx", 
                       sheet = "Data")
# or 

AFO_data <- read_excel("GitHub/SEPA-report-AROW/AFO data.xlsx")

View(AFO_data)

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

## Filter years for more recent data (if needed)
AFO_data_RASE <- AFO_data %>%
  filter(InvAr %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024))

AFO_data_first <- AFO_data %>%
  filter(InvAr %in% c(2018, 2019, 2020))

AFO_data_last <- AFO_data %>%
  filter(InvAr %in% c(2022, 2023, 2024))


## RASE stems at the regional level

# Calculate the mean and error bars for RASE per hectare by region (LandsdelNamn)
region_summary <- summarySE(data = AFO_data_RASE, 
                            measurevar = "AntalRASEHa", 
                            groupvars = "LandsdelNamn")

# Plot the results
ggplot(region_summary, aes(x = reorder(LandsdelNamn, -AntalRASEHa), y = AntalRASEHa)) +
  geom_bar(stat = "identity", colour = "black", fill = "white") +  # Bars for the means
  geom_errorbar(aes(ymin = AntalRASEHa - ci, ymax = AntalRASEHa + ci), width = 0.0, 
                linetype = 1, colour = "black") +  # Error bars for confidence intervals
  geom_hline(yintercept = c(200, 400), linetype = 2, color = "darkorange") +  # Reference lines
  labs(title = "RASE stems per hectare by region",
       x = "Region",
       y = "RASE stems per hectare") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
    plot.title = element_text(hjust = 0.5)  # Center the title
  )


## Export with ggsave (change file name/path depending on species)
ggsave("RASE stems per region SKS.tiff", plot = last_plot(), device = NULL, path = "~/GitHub/SEPA-report-AROW/Moose",
       scale = 0.8, width = 12, height = 10, dpi = 400, limitsize = TRUE, units = "cm")


## RASE stems at the country level

# Calculate the mean and error bars for RASE per hectare by county (AFONr)
county_summary <- summarySE(data = AFO_data_RASE, 
                            measurevar = "AntalRASEHa", 
                            groupvars = "LanNamn")

# Plot the results
ggplot(county_summary, aes(x = reorder(LanNamn, -AntalRASEHa), y = AntalRASEHa)) +
  geom_bar(stat = "identity", colour = "black", fill = "white") +  # Bars for the means
  geom_errorbar(aes(ymin = AntalRASEHa - ci, ymax = AntalRASEHa + ci), width = 0.0, 
                linetype = 1, colour = "black") +  # Error bars for confidence intervals
  geom_hline(yintercept = c(200, 400), linetype = 2, color = "darkorange") +  # Reference lines
  labs(title = "RASE stems per hectare by county",
       x = "County name",
       y = "RASE stems per hectare") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

## Export with ggsave (change file name/path depending on species)
ggsave("RASE stems per county.tiff", plot = last_plot(), device = NULL, path = "~/GitHub/SEPA-report-AROW/Moose",
       scale = 1, width = 11, height = 11, dpi = 400, limitsize = TRUE, units = "cm")

## RASE stems at the ÄFO level

# Create the new column 'Registreri' by combining 'LanKod' and 'AFONr'
# library(stringr)
# AFO_data_RASE <- AFO_data_RASE %>%
#  mutate(Registreri = paste0(str_pad(LanKod, width = 2, pad = "0"), "-", 
#                         str_pad(AFONr, width = 3, pad = "0")))

# Calculate the mean and error bars for RASE per hectare by ÄFO
afo_summary <- summarySE(data = AFO_data_last, 
                            measurevar = "AntalRASEHa", 
                            groupvars = "Registreri")

# Spatial analysis in R

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

## ÄFO and site productivity 

# Perform a linear regression
productivity_model <- lm(AntalRASEHa ~ AndelMargraMarker, data = AFO_data)

# Summary of the regression model
summary(productivity_model)

# Get the R-squared and p-value from the regression model
r_squared <- summary(productivity_model)$r.squared
p_value <- summary(productivity_model)$coefficients[2, 4]

# Plot the regression along with the data points
ggplot(AFO_data, aes(x = (AndelMargraMarker*100), y = (AntalRASEHa))) +
  geom_point(size = 3, color = "black") +  # Points for each Area
  geom_smooth(method = "lm", color = "darkorange", se = TRUE) +  # Regression line with confidence interval
  labs(title = "",
       x = "% sites deemed low productivity",
       y = "RASE per ha") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  
  # Add annotations for R-squared and p-value
  annotate("text", x = 80, y = 3000, 
           label = paste("R² =", round(r_squared, 2)), size = 5, color = "black", hjust = 0) +
  annotate("text", x = 80, y = 2800, 
           label = paste("p-value =", format.pval(p_value, digits = 1)), size = 5, color = "black", hjust = 0)

## Stems at competitive height ####

detach("package:plyr", unload = TRUE)

## Filter years for more recent data (if needed)
AFO_data_RASE <- AFO_data %>%
  filter(InvAr %in% c(2018, 2019, 2020, 2021, 2022, 2023, 2024))

AFO_data_first <- AFO_data %>%
  filter(InvAr %in% c(2018, 2019, 2020))

AFO_data_last <- AFO_data %>%
  filter(InvAr %in% c(2022, 2023, 2024))

# Create the new column 'Registreri' by combining 'LanKod' and 'AFONr'
# library(stringr)
# AFO_data <- AFO_data %>%
#  mutate(Registreri = paste0(str_pad(LanKod, width = 2, pad = "0"), "-", 
#                             str_pad(AFONr, width = 3, pad = "0")))

# Calculate the mean and error bars for RASE per hectare by ÄFO
afo_summary2 <- summarySE(data = AFO_data_last, 
                         measurevar = "RASEAndelGynnsam", 
                         groupvars = "Registreri")
# Load the shapefile
library(sf)
AFO_shp <- st_read("~/GIS/ÄFO/updated_AFO_shapefile.shp")

# Join the new data to the shapefile
AFO_joined2 <- AFO_shp %>%
  left_join(afo_summary2, by = "Registreri")

# Plot with tmap
RASEComp <- tm_shape(AFO_joined2) +
  tm_graticules(alpha=0.3, n.x=3, n.y=6) +
  tm_fill("RASEAndelGynnsam", textNA = "NA", colorNA="#999999", title="", 
          palette=c("#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"), 
          breaks = c(0, 0.05, 0.1, 1), 
          labels = c("< 5%", "5 – 10%", "> 10%")) +
  tm_borders(col="black", lwd=1.5) +    #lwd=1
  tm_layout(main.title = "% RASE competitive 2022/24", title.size = 1.2, legend.text.size = 1.0) +
  tm_shape(AFO_joined2) + 
  tm_fill("Registreri", palette = c("white", "white", "white"), alpha=0, legend.show=FALSE) +    
  tm_borders(col="black", lwd=1.5) +      #lwd=2
  tm_scale_bar(position=c("right", "bottom")) 

RASEComp

# Save the tmap object as a TIFF file
tmap_save(RASEComp, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/Proportion RASE competative 2022 - 2024.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")

# Change in RASE at competative height map
# Aggregate data for each period
afo_first_summary <- summarySE(data = AFO_data_first, 
                               measurevar = "RASEAndelGynnsam", 
                               groupvars = "Registreri")

afo_last_summary <- summarySE(data = AFO_data_last, 
                              measurevar = "RASEAndelGynnsam", 
                              groupvars = "Registreri")

# Rename the 'mean' column to reflect each period
afo_first_summary <- dplyr::rename(afo_first_summary, Comp_mean_first = RASEAndelGynnsam)

afo_last_summary <- dplyr::rename(afo_last_summary, Comp_mean_last = RASEAndelGynnsam)

# Calculate the change
afo_change <- afo_last_summary %>%
  left_join(afo_first_summary, by = "Registreri") %>%
  mutate(change = Comp_mean_last - Comp_mean_first)

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
          breaks = c(-Inf, -0.0500, 0, 0.0500, Inf),           # Define meaningful breaks
          labels = c("Decrease >5%", "Decrease ≤5%", "Increase ≤5%", "Increase >5%")) +
  tm_borders(col="black", lwd=1.5) +
  tm_layout(main.title ="Change 2018/20 to 2022/24", 
            title.size = 1.2, 
            legend.text.size = 1.0) +
  tm_scale_bar(position = c("right", "bottom"))

# Save the tmap object as a TIFF file
tmap_save(RASEChangeMap, 
          filename = "//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/RASE plots/Competive RASE change 5.tiff",
          width = 26, height = 21, dpi = 300, units = "cm")

# Print the map
RASEChangeMap


## Competitive height and browsing ####

# Create the new column 'Registreri' by combining 'LanKod' and 'AFONr'
library(stringr)
AFO_data <- AFO_data %>%
  mutate(Registreri = paste0(str_pad(LanKod, width = 2, pad = "0"), "-", 
                             str_pad(AFONr, width = 3, pad = "0")))

# Calculate the mean and error bars for % competitive RASE and % damage on pine
dmg_summary <- summarySE(data = AFO_data, 
                         measurevar = "ArsskadaTallAndel", 
                         groupvars = "Registreri")

comp_summary <- summarySE(data = AFO_data, 
                         measurevar = "RASEAndelGynnsam", 
                         groupvars = "Registreri")

# Perform a linear regression
regression_model <- lm(RASEAndelGynnsam ~ ArsskadaTallAndel, data = AFO_data)

# Summary of the regression model
summary(regression_model)

# Get the R-squared and p-value from the regression model
r_squared <- summary(regression_model)$r.squared
p_value <- summary(regression_model)$coefficients[2, 4]

# Plot the regression along with the data points
ggplot(AFO_data, aes(x = (ArsskadaTallAndel*100), y = (RASEAndelGynnsam*100))) +
  geom_smooth(method = "lm", color = "darkorange", se = TRUE) +  # Regression line with confidence interval
  geom_point(size = 3, color = "black") +  # Points for each Area
  labs(title = "",
       x = "Pine damage (%)",
       y = "RASE at competative height (%)") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  
# Add annotations for R-squared and p-value
  annotate("text", x = 40, y = 40, 
           label = paste("R² =", round(r_squared, 3)), size = 5, color = "black", hjust = 0) +
  annotate("text", x = 40, y = 38, 
           label = paste("p-value =", format.pval(p_value, digits = 1)), size = 5, color = "black", hjust = 0)

# Export the regression plot (adjust filename/path as needed)
ggsave("Competative_RASE_vs_Pine_Damage_Regression.tiff", plot = last_plot(), 
       path = "~/GitHub/SEPA-report-AROW/Moose",
       scale = 1, width = 11, height = 11, dpi = 400, units = "cm")


