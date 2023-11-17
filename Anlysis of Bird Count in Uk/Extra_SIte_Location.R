# -------------------------------------------------------------------------
# Site_Location Appendix:
# This R script visualizes the distribution of sites partitioned by across the UK.
# 'subrur' factor and plotted on a map of the UK.
# -------------------------------------------------------------------------
library(ggplot2)
library(rnaturalearth)
library(sf)

data <- read.csv("Assignemnt_1_data.csv", header = TRUE, sep = ",")

# British National Grid to WGS84
data_wgs84 <- st_transform(st_as_sf(data, coords = c("easting", "northing"), crs = 27700), 4326)
data$longitude <- st_coordinates(data_wgs84)[,1]
data$latitude <- st_coordinates(data_wgs84)[,2]

# Get the UK boundary data
uk <- ne_countries(scale = "medium", returnclass = "sf", country = "United Kingdom")

# Map of UK
Map <- ggplot(data = uk) +
  geom_sf(fill = "white", color = "black") +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Sites Distribution in the UK")

# Add data points to map
Map_with_points <- Map +
  geom_point(data = data, aes(x = longitude, y = latitude, color = as.factor(subrur)), size = 3) +
  scale_color_manual(values = c("red", "blue"), name = "Subrur Factor", breaks = c("-1", "1"), labels = c("Type -1", "Type 1"))

print(Map_with_points)

