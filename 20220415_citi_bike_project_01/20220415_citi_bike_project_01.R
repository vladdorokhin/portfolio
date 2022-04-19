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
colnames(tripdata)
## See first 6 rows
head(tripdata)
## See a list of columns and data types (numeric, character, etc)
str(tripdata)
## Glimpse of data
print("Glimpse:")
glimpse(tripdata)
## Statistical summary of data
print("Summary:")
summary(tripdata)
