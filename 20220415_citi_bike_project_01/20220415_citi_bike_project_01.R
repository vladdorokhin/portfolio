# Install and load the project-related packages
library(tidyverse)
library(janitor)
library(ggmap)
library(geosphere)
library(lubridate)
library(gridExtra)

# Import the data sets to RStudio
tripdata202103 <- read_csv("202103-citibike-tripdata.csv")
tripdata202104 <- read_csv("202104-citibike-tripdata.csv")
tripdata202105 <- read_csv("202105-citibike-tripdata.csv")
tripdata202106 <- read_csv("202106-citibike-tripdata.csv")
tripdata202107 <- read_csv("202107-citibike-tripdata.csv")
tripdata202108 <- read_csv("202108-citibike-tripdata.csv")
tripdata202109 <- read_csv("202109-citibike-tripdata.csv")
tripdata202110 <- read_csv("202110-citibike-tripdata.csv")
tripdata202111 <- read_csv("202111-citibike-tripdata.csv")
tripdata202112 <- read_csv("202112-citibike-tripdata.csv")
tripdata202201 <- read_csv("202201-citibike-tripdata.csv")
tripdata202202 <- read_csv("202202-citibike-tripdata.csv")
tripdata202203 <- read_csv("202203-citibike-tripdata.csv")

# Merge individual monthly data sets into a single large data set
tripdata <- bind_rows(tripdata202103, tripdata202104, tripdata202105, tripdata202106, tripdata202107, tripdata202108, tripdata202109, tripdata202110, tripdata202111, tripdata202112, tripdata202201, tripdata202202, tripdata202203)

# Warning message:
##  One or more parsing issues, see `problems()` for details 
## > tripdata <- bind_rows(tripdata202103, tripdata202104, tripdata202105, tripdata202106, tripdata202107, tripdata202108, tripdata202109, tripdata202110, tripdata202111, tripdata202112, tripdata202201, tripdata202202, tripdata202203)
## Error in `bind_rows()`:
##  ! Can't combine `end_station_id` <double> and `end_station_id` <character>.
## Run `rlang::last_error()` to see where the error occurred.

# A review of column types of the tripdata202110 data set
str(tripdata202110)

# Change end_station_id to the numeric type for the tripdata202110 data set
tripdata202110 <- mutate(tripdata202110, end_station_id = as.numeric(end_station_id))

# Merge individual monthly data sets into a single large data set, attempt no. 2
tripdata <- bind_rows(tripdata202103, tripdata202104, tripdata202105, tripdata202106, tripdata202107, tripdata202108, tripdata202109, tripdata202110, tripdata202111, tripdata202112, tripdata202201, tripdata202202, tripdata202203)

# View of the merged data set in a table form
View(tripdata)

# Check the merged data set
## See a list of column names
print("A list of column names:")
colnames(tripdata)
## See the first 6 rows
print("The first 6 rows:")
head(tripdata)
## See a list of columns and data types (numeric, character, etc.)
print("A list of columns and data types (numeric, character, etc.):")
str(tripdata)
## Glimpse of data
print("Glimpse:")
glimpse(tripdata)
## Statistical summary of data
print("Summary:")
summary(tripdata)

# Clean the tripdata database to be able to properly work with it:
## Drop all NA (null = empty values):
tripdata_clean <- drop_na(tripdata)
## View the merged clean database in a table form
View(tripdata_clean)

# Create new columns
## Change the format of the date row to an appropriate one for calculations
tripdata_clean$date <- as.Date(tripdata_clean$started_at) 
## Separate the dates into month
tripdata_clean$month <- format(as.Date(tripdata_clean$date), "%m")
## Separate the dates into day
tripdata_clean$day <- format(as.Date(tripdata_clean$date), "%d")
## Separate the dates into year
tripdata_clean$year <- format(as.Date(tripdata_clean$date), "%Y")
## Separate the dates into day of a week
tripdata_clean$day_of_week <- format(as.Date(tripdata_clean$date), "%A")

# Create new columns
## Duration of the ride length in seconds
tripdata_clean$ride_length <- difftime(tripdata_clean$ended_at, tripdata_clean$started_at)
## Ride distance traveled in km
tripdata_clean$ride_distance <- distGeo(matrix(c(tripdata_clean$start_lng, tripdata_clean$start_lat), ncol = 2), matrix(c(tripdata_clean$end_lng, tripdata_clean$end_lat), ncol = 2))
tripdata_clean$ride_distance <- tripdata_clean$ride_distance/1000
## Ride peed in km/h
tripdata_clean$ride_speed = c(tripdata_clean$ride_distance) / as.numeric(c(tripdata_clean$ride_length), units="hours")

# Double-check there will be no values when bikes were taken out of docks and checked for quality by Citi Bike employees or when ride_length was negative
tripdata_clean <- tripdata_clean[!(tripdata_clean$start_station_name == "HQ QR" | tripdata_clean$ride_length < 0),]

# Calculate the average distance for both the casual and member type users
member_casual_mean <- tripdata_clean %>%
  group_by(member_casual) %>%
  summarise(mean_time = mean(ride_length), mean_distance = mean(ride_distance))
## Build a table with the results
View(member_casual_mean)

# Build a plot of mean travel time by user type
member_casual_mean_time <- ggplot(member_casual_mean) + 
  geom_col(mapping = aes(x = member_casual, y = mean_time, fill = member_casual), show.legend = TRUE) +
  labs(title = "Mean travel time by user type: Member / Casual", x = "User type", y = "Mean time in sec", caption = "Data by Citi Bike. Plot by Vlad Dorokhin")
## Build a graph with the results
print(member_casual_mean_time)

# Build a plot of mean travel distance by user type
member_casual_mean_distance <- ggplot(userType_means) + 
  geom_col(mapping = aes(x = member_casual, y = mean_distance, fill = member_casual), show.legend = TRUE) +
  labs(title = "Mean travel distance by user type: Member / Casual", x = "User type", y = "Mean distance in km", caption = "Data by Citi Bike. Plot by Vlad Dorokhin")
## Build a graph with the results
print(member_casual_mean_distance)

# Combine two recent plots (mean travel time and mean travel distance) together
grid.arrange(member_casual_mean_time, member_casual_mean_distance, ncol = 2)

# Check the number of rides by user type during a week
## Build a tibble of the data
member_casual_rides_week <- tripdata_clean %>% 
    mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual, weekday) %>% 
    summarise(number_of_rides = n(), average_duration = mean(ride_length), .groups = 'drop') %>% 
    arrange(member_casual, weekday)
print(member_casual_rides_week)
## Build a plot of the data
tripdata_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length), .groups = 'drop') %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides by user type during the week", x = "Days of the week", y = "Number of rides", caption = "Data by Citi Bike. Plot by Vlad Dorokhin", fill = "User type") +
  theme(legend.position = "top")

# Create a new data set with only classic bikes and electric bikes in the rideable_type field
tripdata_clean_classic_electric <- tripdata_clean %>%
  filter(rideable_type == "classic_bike" | rideable_type == "electric_bike")

# Check the bike type usage by user type
tripdata_clean_classic_electric %>%
  group_by(member_casual, rideable_type) %>%
  summarise(totals=n(), .groups="drop")  %>%
  ggplot() +
  geom_col(aes(x = member_casual, y = totals, fill = rideable_type), position = "dodge") + 
  labs(title = "Bike type usage by user type: Classic Bike / Electric Bike", x = "User type", y = NULL, fill = "Bike type", caption = "Data by Citi Bike. Plot by Vlad Dorokhin") +
  scale_fill_manual(values = c("classic_bike" = "#ffa600", "electric_bike" = "#bc5090")) +
  theme_minimal() +
  theme(legend.position = "top")

# Check the bike usage by both user types during a week
tripdata_clean_classic_electric %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, rideable_type, weekday) %>%
  summarise(totals=n(), .groups="drop") %>%
  ggplot(aes(x = weekday, y = totals, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  facet_wrap(~member_casual) +
  labs(title = "Bike type usage by user type during a week", x = "User type", y = NULL, caption = "Data by Citi Bike. Plot by Vlad Dorokhin") +
  scale_fill_manual(values = c("classic_bike" = "#ffa600", "electric_bike" = "#bc5090")) +
  theme_minimal() +
  theme(legend.position="none")

# Check the coordinates data of the rides
## Create a table only for the most popular routes (>500 times)
tripdata_coordinates <- tripdata_clean %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>%
  summarise(total = n(), .groups="drop") %>%
  filter(total > 500)

# Check the coordinates data of the rides
## Create a table only for the most popular routes (>500 times)
tripdata_coordinates <- tripdata_clean %>% 
  filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>%
  summarise(total = n(), .groups="drop") %>%
  filter(total > 500)
## Create 2 sub-tables for each user type
casual <- tripdata_coordinates %>% filter(member_casual == "casual")
member <- tripdata_coordinates %>% filter(member_casual == "member")

# Store bounding box coordinates for ggmap:
nyc_bounding_box <- c(
  left = -74.15,
  bottom = 40.5774,
  right = -73.7004,
  top = 40.9176
)

# Store the stamen map of NYC
nyc_stamen_map <- get_stamenmap(
  bbox = nyc_bounding_box,
  zoom = 12,
  maptype = "toner"
)

# Plot the data by casual users on the map
ggmap(nyc_stamen_map, darken = c(0.8, "white")) +
  geom_curve(casual, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha = total, color = rideable_type), size = 0.5, curvature = .2, arrow = arrow(length = unit(0.2, "cm"), ends = "first", type = "closed")) +
  coord_cartesian() +
  labs(title = "Most popular routes by casual users", x = NULL, y = NULL, color = "User type", caption = "Data by Citi Bike. Plot by Vlad Dorokhin") +
  theme(legend.position="right")

# Plot the data by annual members on the map
ggmap(nyc_stamen_map, darken = c(0.8, "white")) +
  geom_curve(member, mapping = aes(x = start_lng, y = start_lat, xend = end_lng, yend = end_lat, alpha = total, color = rideable_type), size = 0.5, curvature = .2, arrow = arrow(length = unit(0.2,"cm"), ends="first", type = "closed")) +  
  coord_cartesian() +
  labs(title = "Most popular routes by annual members", x = NULL,y = NULL, caption = "Data by Citi Bike. Plot by Vlad Dorokhin") +
  theme(legend.position="right")

# The code above is a part of Vlad Dorokhin's portfolio research project: Citi Bike Bike-Share Analysis (03/2021-03/2022)
# More information and additional files can be found here: https://github.com/vladdorokhin/portfolio/tree/main/20220415_citi_bike_project_01

# Copyright (C) [2022] [Vlad Dorokhin]
# This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program; if not, see <https://www.gnu.org/licenses>.
