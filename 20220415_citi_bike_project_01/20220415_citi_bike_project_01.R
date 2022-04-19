# The code is a part of Vlad Dorokhin's portfolio research project: Citi Bike Bike-Share Analysis (03/2021-03/2022)
# More information and additional files can be found here: https://github.com/vladdorokhin/portfolio/tree/main/20220415_citi_bike_project_01

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
