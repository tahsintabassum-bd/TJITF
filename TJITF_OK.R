#Install and load necessary packages and library
if (!requireNamespace("ggspatial", quietly = TRUE)) {
  install.packages("ggspatial")
} else {
  update.packages("ggspatial")
}
if (!requireNamespace("units", quietly = TRUE)) {
  install.packages("units")
}
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(units)
#LOAD THE DATA
counties <- st_read("COUNTY_BOUNDARY.shp")
municipalities <- st_read("munibnd.shp")
Stops <- st_read("STOPS.shp")
#Variables <- read.csv("FINAL VARIABLES.csv")
ggplot(data = OKC_MA) +
  geom_sf(aes(fill = COUNTY_NAM), color = "black", lwd = 0.5) + # Fill based on county
  name
26
scale_fill_viridis_d(option = "plasma", begin = 0.3, end = 0.9, direction = 1, name = "County")
+ # Use a diverging color palette
  annotation_scale(location = "br", width_hint = 0.5) +
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.5, "in"), pad_y =
                           unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Map of Counties in OKC Metropolitan Area", fill = "County") +
  theme_minimal() +
  theme(legend.position = "right")
# Define the counties of interest
OKC_MA <- c("CANADIAN", "CLEVELAND", "GRADY", "LINCOLN", "LOGAN",
            "MCCLAIN", "OKLAHOMA")
# Filter out the selected counties
OKC_MA <- counties %>%
  filter(COUNTY_NAM %in% OKC_MA)
# Plot to verify the selected counties
plot(OKC_MA$geometry)
# Save the clipped shapefile
st_write(OKC_MA, "selected_counties.shp")
# Define the ok county
OK <- c("OKLAHOMA")
# Filter out the selected counties
OK <- OKC_MA %>%
  27
filter(COUNTY_NAM %in% OK)
# Plot to verify the selected counties
plot(OK$geometry)
# Save the clipped shapefile
st_write(OK, "ok.shp")
# Filter municipalities based on CO_FIPS code
OKmunicipalities <- municipalities %>%
  filter(CO_FIPS == 109)
# Optional: Plot to verify the selected municipalities
plot(OKmunicipalities$geometry)
# Save the clipped shapefile
st_write(OKmunicipalities, "selected_municipalities.shp")
# Remove duplicate geometries
OKmunicipalities <- OKmunicipalities %>%
  distinct(geometry, .keep_all = TRUE)
# Assuming 'muni_name' is an attribute holding the name of each municipality
OKmunicipalities <- OKmunicipalities %>%
  distinct(FIPS, .keep_all = TRUE)
28
# Check the results
print(nrow(OKmunicipalities))
# Check the CRS of OKmunicipalities
Stops_crs <- st_crs(Stops)
print(Stops_crs )
OKC_MA <- st_set_crs(OKC_MA, 4326)
OKmunicipalities <- st_set_crs(OKmunicipalities, 4326)
# Reproject OKC_MA to match the CRS of OKmunicipalities
OKC_MA <- st_transform(OKC_MA, crs = Stops_crs )
OKmunicipalities <- st_transform(OKmunicipalities, crs = Stops_crs )
ggplot(data = OKmunicipalities) +
  geom_sf(aes(fill = CITYNAME), color = "black", lwd = 0.5) + # Fill based on county name
  scale_fill_viridis_d(option = "viridis", begin = 0.3, end = 0.9, direction = 1, name =
                         "Municipalities") + # Use a diverging color palette
  annotation_scale(location = "br", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.1, "in"), pad_y =
                           unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Map of Municipalities in Oklahoma County", fill = "Municipalities") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
# Convert FIPS in both data frames to character to ensure proper matching
29
OKmunicipalities$CITYNAME <- as.character(OKmunicipalities$CITYNAME)
FINAL_VARIABLES$CITYNAME <- as.character(FINAL_VARIABLES$CITYNAME)
# Join the data
joined_data <- OKmunicipalities %>%
  left_join(FINAL_VARIABLES, by = "CITYNAME")
str(joined_data)
ART <- data.frame(
  Race = c(11),
  Age = c(39),
  Disability = c(16),
  Household_income = c(58),
  Single_parent_household = c(39),
  Vehicles_per_household = c(4),
  Cost_of_living = c(50),
  Economic_development = c(1),
  Network_connectivity = c(2.20),
  Truck_Volume = c(3.88),
  Crash_rates = c(8.29)
)
# ART values - usually you will fetch this from a reference or calculate dynamically
art_values <- c(Race = 11, Age = 39, Disability = 16, Household_income = 58,
                Single_parent_household = 39, Vehicles_per_household = 4,
                Cost_of_living = 50, Economic_development = 1,
                Network_connectivity = 2.20, Truck_Volume = 3.88, Crash_rates = 8.29)
30
# Define a function to create scales and assign points
create_graduated_scale <- function(data, factor_name, art) {
  min_val <- min(data[[factor_name]], na.rm = TRUE)
  max_val <- max(data[[factor_name]], na.rm = TRUE)
  # Define the scale ranges
  step_size <- (max_val - min_val) / 6
  breaks <- seq(min_val, max_val, by = step_size)
  # Ensure that the ART is a break point
  breaks <- sort(unique(c(breaks, art)))
  # Define points
  points_below_art <- rev(seq(-3, 0, by = 1))
  points_above_art <- seq(1, 3, by = 1)
  points <- c(points_below_art, 0, points_above_art)
  # Assign points to ranges
  data[[paste0(factor_name, "_Points")]] <- findInterval(data[[factor_name]], breaks,
                                                         rightmost.closed = TRUE) %>%
    {points[.]}
  return(data)
}
# Apply function for each factor
joined_data <- create_graduated_scale(joined_data, "Race", art_values["Race"])
31
joined_data <- create_graduated_scale1(joined_data, "Age", art_values["Age"])
joined_data <- create_graduated_scale(joined_data, "Disability", art_values["Disability "])
joined_data <- create_graduated_scale(joined_data, "Household_income",
                                      art_values["Household_income"])
joined_data <- create_graduated_scale(joined_data, "Single_parent_household",
                                      art_values["Single_parent_household"])
joined_data <- create_graduated_scale(joined_data, "Vehicles_per_household", art_values["
Vehicles_per_household"])
joined_data <- create_graduated_scale(joined_data, "Cost_of_living",
                                      art_values["Cost_of_living"])
joined_data <- create_graduated_scale(joined_data, "Network_connectivity",
                                      art_values["Network_connectivity"])
joined_data <- create_graduated_scale(joined_data, "Truck_Volume",
                                      art_values["Truck_Volume"])
joined_data <- create_graduated_scale(joined_data, "Crash_rates", art_values["Crash_rates"])
joined_data <- create_graduated_scale(joined_data, "Economic_development",
                                      art_values["Economic_development"])
#joined_data <- joined_data %>%
#mutate(Economic_development_Points1 = (Economic_development_Points) * (-1))
# Check for invalid geometries
invalid <- st_is_valid(joined_data, NA_on_exception = FALSE)
if (any(!invalid)) {
  print("Invalid geometries detected, attempting to repair")
  # Attempt to make invalid geometries valid
  joined_data$geometry[!invalid] <- st_make_valid(joined_data$geometry[!invalid])
}
composite_scores <- joined_data %>%
  group_by(CITYNAME) %>%
  32
summarise(
  Total_Points = sum(Race_Points, Age_Points, Disability_Points, Household_income_Points,
                     Single_parent_household_Points, Vehicles_per_household_Points,
                     Cost_of_living_Points, Economic_development_Points,
                     Network_connectivity_Points, Truck_Volume_Points, Crash_rates_Points,
                     na.rm = TRUE) # Use na.rm = TRUE to ignore NA values if any
)
# View the result
print(composite_scores)
# Assuming composite_scores is already calculated and contains the necessary geometries
library(ggplot2)
library(sf)
library(dplyr)
# Example breaks based on your provided scale, adjust these as per your actual data range
breaks <- c(-18, -14, -10, -6,-4, 0, 4)
labels <- c("-18 to -14 points", "-14.1 to -10 points", "-10.1 to -6 points", "-6.1 to -4 points", "0
points", "0.1 to 4 points (TJ Area)")
colors <- c("#cdebc5","#97d690", "#60c25b", "#46b840", "yellow" ,"#e60000")
# Cut the scores into factor levels
composite_scores$Score_Class <- cut(composite_scores$Total_Points, breaks = breaks, labels =
                                      labels, include.lowest = TRUE)
# Ensure that the geometries are valid
composite_scores <- st_make_valid(composite_scores)
# Plot the map with North Arrow and Scale Bar
33
map_plot <- ggplot(data = composite_scores) +
  geom_sf(aes(fill = Score_Class), color = "black", size = 0.25) +
  scale_fill_manual(values = colors, labels = labels, name = "Composite Index Value") +
  annotation_scale(location = "br", width_hint = 0.1) +
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.02, "in"), pad_y =
                           unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Transportation Justice Threshold Index Framework",
       subtitle = "Oklahoma County, OK", fill = "Municipalities") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5))
# Display the plot with the added features
print(map_plot)
#labs(title = "Transportation Justice Threshold Index Framework",
subtitle = "Oklahoma County, OK") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotation_north_arrow(location = "bottomright", which_north = "true",
                         pad_x = unit(0.5, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering()) +
  annotation_scale(location = "bottomleft", width_hint = 0.5)
# Display the plot with the added features
print(map_plot)
34
# Plot the map
map_plot <- ggplot(data = composite_scores) +
  geom_sf(aes(fill = Score_Class), color = "black", size = 0.25) +
  scale_fill_manual(values = colors, labels = labels, name = "Composite Index Value") +
  labs(title = "Transportation Justice Threshold Index Framework",
       subtitle = "Oklahoma County, OK") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Display the plot
print(map_plot)
library(sf)
library(dplyr)
# Assuming composite_scores and joined_data are sf objects
# Convert composite_scores to a non-spatial data frame
composite_scores_df <- st_drop_geometry(composite_scores) %>%
  as.data.frame()
# Write composite_scores to CSV
write.csv(composite_scores_df, "composite_scores.csv", row.names = FALSE)
# Convert joined_data to a non-spatial data frame
joined_data_df <- st_drop_geometry(joined_data) %>%
  as.data.frame()
35
# Write joined_data to CSV
write.csv(joined_data_df, "joined_data.csv", row.names = FALSE)
x <- c(4.1, 26.0, 8.4, 17.2, -12.5)
c <- x > 10
y <- c(1, 2, 3, 4, 5)
d <- y[y <= 3]