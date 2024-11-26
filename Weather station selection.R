## Import geodata
library(sf)
AFO_shp <- st_read("~/GIS/ÄFO/2022/ÄFO_2022_221117.shp")

# Calculate centroids
AFO_centroids <- st_centroid(AFO_shp)

# Print the first few centroid geometries
print(head(AFO_centroids))

# Optionally, save centroids to a new shapefile
st_write(AFO_centroids, "~/GIS/ÄFO/2022/ÄFO_Centroids.shp")

# Visualize polygons and centroids
library(ggplot2)

# Convert to a format suitable for ggplot
AFO_shp <- st_as_sf(AFO_shp)
AFO_centroids <- st_as_sf(AFO_centroids)

# Plot
ggplot() +
  geom_sf(data = AFO_shp_df, fill = "lightblue", color = "black") +
  geom_sf(data = AFO_centroids_df, color = "red", size = 2) +
  labs(title = "Polygons and their Centroids", 
       caption = "Red points are centroids") +
  theme_minimal()

# Add weather stations
library(readr)
info_SMHI_Weather_stations <- read_delim("//storage-um.slu.se/restricted$/vfm/Vilt-Skog/Moose-Targets/SMHI/info SMHI Weather stations.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Filter the dataset for stations with 100% coverage in temperature, precipitation, and snow depth
library(dplyr)
filtered_stations <- info_SMHI_Weather_stations %>%
  filter(`Temperature[%]` >= 80, 
         `Precipitation[%]` >= 80, 
         `Snowdepth[%]` >= 80)

# Convert weather station data to sf object
weather_stations_sf <- st_as_sf(info_SMHI_Weather_stations, coords = c("Longitude", "Latitude"), crs = 4326)
filtered_stations_sf <- st_as_sf(filtered_stations, coords = c("Longitude", "Latitude"), crs = 4326)

# Plot
ggplot() +
  geom_sf(data = AFO_shp_df, fill = "lightblue", color = "black") +
  geom_sf(data = AFO_centroids_df, color = "red", size = 2) +
  geom_sf(data = weather_stations_sf, color = "blue", size = 2)+
  geom_sf(data = filtered_stations_sf, color = "yellow", size = 2)+ 
  labs(title = "Polygons and their Centroids", 
       caption = "Red points are centroids") +
  theme_minimal()

