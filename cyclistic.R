#Installing the packages
install.packages('tidyverse')
install.packages('readr')
install.packages('lubridate')
install.packages("dplyr")
install.packages("magrittr")

#Loading the packages
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(magrittr)
#Adding a name <- Importing the csv(file_location)
Jan2022 <- read_csv("Divvy_Tripdata/2022_01.csv")
Feb2022 <- read_csv("Divvy_Tripdata/2022_02.csv")
Mar2022 <- read_csv("Divvy_Tripdata/2022_03.csv")
Apr2022 <- read_csv("Divvy_Tripdata/2022_04.csv")
May2022 <- read_csv("Divvy_Tripdata/2022_05.csv")
Jun2022 <- read_csv("Divvy_Tripdata/2022_06.csv")
Jul2022 <- read_csv("Divvy_Tripdata/2022_07.csv")
Aug2022 <- read_csv("Divvy_Tripdata/2022_08.csv")

#str(dataset_name)
colnames(Jan2022)
colnames(Feb2022)
colnames(Mar2022)
colnames(Apr2022)
colnames(May2022)
colnames(Jun2022)
colnames(Jul2022)
colnames(Aug2022)

#Creating new dataset name <- binding rows(all_your_datasets)

merged_data <- bind_rows(Jan2022, Feb2022, Mar2022, Apr2022, May2022, Jun2022, Jul2022, Aug2022)

head(merged_data)

#Cleaning the Dataset

#Fixing Column Types
str(merged_data)

merged_data$started_at <- as.POSIXct(merged_data$started_at, format = "%Y-%m-%d %H:%M:%S")
merged_data$ended_at <- as.POSIXct(merged_data$ended_at, format = "%Y-%m-%d %H:%M:%S")
str(merged_data)


#Removing NA/null values
cleaned_df_1 <- na.omit(merged_data)

#Adding Ride Length Column
cleaned_df_2 <- mutate(cleaned_df_1, ride_length = difftime(ended_at, started_at, units = "mins"))
str(cleaned_df_2)

#To calculate the number of observations with negative ride length
nrow(cleaned_df_2[cleaned_df_2$ride_length < 0,])


## We use the ! to NOT show observations where ride_length < 0.
cleaned_df_3 <- cleaned_df_2[!cleaned_df_2$ride_length < 0,]
head(cleaned_df_3, 5)

#Adding Month and Day and Hour Columns


cleaned_df_4 <- mutate(cleaned_df_3, trip_month = format(as.POSIXct(started_at), "%B-%y"), trip_day = format(as.POSIXct(started_at),"%A"), trip_hour = format(as.POSIXct(started_at),"%H"))
head(cleaned_df_4,3)

#write.csv(cleaned_df_4, file = "cleaned_df_4.csv")

#Analysing the Data
summary_stats <- cleaned_df_4 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length), median_ride_length = median(ride_length), min_ride_length = min(ride_length), max_ride_length = max(ride_length))
head(summary_stats)

#how many members are keeping the bikes for more than 24 hours
## We will filter for ride length > 1440 minutes, which is 24 hours.
## We will also group by member_casual field to compare between casuals and members
summary_stats_long_usage <- cleaned_df_4 %>% 
  filter(cleaned_df_4$ride_length > 1440) %>% 
  group_by(member_casual) %>% 
  summarise(number_of_trips_over_24h = n())
head(summary_stats_long_usage)

#Top 30 Frequent Start Stations - Members vs. Casuals
## To extract the station list we will extract columns 5, 9 and 10 from out main dataset. Then we will remove the duplicated station names to end up with a our station list.
stations_list <- cleaned_df_4[,c(5, 9, 10)]
stations_list <- stations_list[!duplicated(stations_list$start_station_name),]
nrow(stations_list)
head(stations_list, 5)

## This code creates the dataset for members

total_start_stations_member <- cleaned_df_4 %>% 
  filter(member_casual == 'member') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

## Then we create a top 30 list and merge with stations list to get lat and long info.
total_start_stations_member_top30 <- (merge(head(total_start_stations_member, 30), stations_list)) %>% 
  arrange(- number_of_starts)

head(total_start_stations_member_top30, 5)

#We will repeat the same process for casuals.

total_start_stations_casual <- cleaned_df_4 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

total_start_stations_casual_top30 <- merge(head(total_start_stations_casual, 30), stations_list) %>% 
  arrange(- number_of_starts)

head(total_start_stations_casual_top30, 5)

#saving files for tableau
write.csv(total_start_stations_casual_top30, file = "casual_stations.csv")

write.csv(total_start_stations_member_top30, file = "member_stations.csv")


#Weekly Comparison of Trip Frequency and Average Trip Length


## First, we will order the dataset by the specified vector. This is because in default R arranges by alphabet. 
cleaned_df_4$trip_day <- ordered(cleaned_df_4$trip_day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

## Then we will create a new dataset which will summarise our total trips and average trip length information.
weekly_usage <- cleaned_df_4 %>% 
  group_by(member_casual, trip_day) %>% 
  summarise(total_trips = n(), avg_trip_length = mean(ride_length))

head(weekly_usage, 14)

write.csv(weekly_usage, file = "weekly_usage.csv")

ggplot(data = weekly_usage)+
  geom_col(mapping = aes(x = trip_day, y = total_trips, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = 'Total Trips per Weekday', subtitle = 'Members vs. Casuals', x = 'Day of the Week', y = 'Trips', fill = 'Member Type')+
  facet_wrap(~member_casual)

#plot our findings on average trip length
ggplot(data = weekly_usage)+
geom_col(mapping = aes(x = trip_day, y = avg_trip_length, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = 'Average Ride Length per Weekday', subtitle = 'Members vs. Casuals', x = 'Day of the Week', y = 'Avg Ride Length in Minutes', fill = 'Member Type')+
  facet_wrap(~member_casual)

#Monthly Comparison of Trip Frequency and Average Trip Length

## First, we will order the dataset by the specified vector. This is because in default R arranges by alphabet.
cleaned_df_4$trip_month <- ordered(cleaned_df_4$trip_month, levels = c("January-22", "February-22", "March-22","April-22", "May-22", "June-22", "July-22", "August-22"))

monthly_usage <- cleaned_df_4 %>% 
  group_by(member_casual, trip_month) %>% 
  summarise(total_trips = n(), avg_trip_length = mean(ride_length))

head(monthly_usage)


write.csv(monthly_usage, file = "monthly_usage.csv")

#plot our findings on monthly trip frequency
ggplot(data = monthly_usage)+
  geom_col(mapping = aes(x = trip_month, y = total_trips, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = 'Total Trips per Month', subtitle = 'Members vs. Casuals', x = 'Month-Year', y = 'Trips', fill = 'Member Type')+
  facet_wrap(~member_casual)

#average monthly trip

ggplot(data = monthly_usage)+
  geom_col(mapping = aes(x = trip_month, y = avg_trip_length, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = 'Average Trip Length per Month', subtitle = 'Members vs. Casuals', x = 'Month-Year', y = 'Average Trip Length in Minutes', fill = 'Member Type')+
  facet_wrap(~member_casual)


#Hourly Comparison of Trip Frequency and Average Trip Length


## First, we will order the dataset by the specified vector. This is because in default R arranges by alphabet. 
cleaned_df_4$trip_hour <- ordered(cleaned_df_4$trip_hour, levels = c("01", "02", "03", "04", "05", "06", "07","08", "09", "10", "11", "12", "13", "14","15", "16", "17", "18", "19", "20", "21","22", "23","24"))

## Then we will create a new dataset which will summarise our total trips and average trip length information.
hourly_usage <- cleaned_df_4 %>% 
  group_by(member_casual, trip_hour) %>% 
  summarise(total_trips = n(), avg_trip_length = mean(ride_length))

head(hourly_usage, 30)

write.csv(hourly_usage, file = "hourly_usage.csv")

ggplot(data = hourly_usage)+
  geom_col(mapping = aes(x = trip_hour, y = total_trips, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = 'Total Trips per hour', subtitle = 'Members vs. Casuals', x = 'Hour of the Week', y = 'Trips', fill = 'Member Type')+
  facet_wrap(~member_casual)

#plot our findings on average trip length
ggplot(data = hourly_usage)+
  geom_col(mapping = aes(x = trip_hour, y = avg_trip_length, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 45))+
  labs(title = 'Average Ride Length per hour', subtitle = 'Members vs. Casuals', x = 'hour of the Week', y = 'Avg Ride Length in Minutes', fill = 'Member Type')+
  facet_wrap(~member_casual)

no_of_rides <-
cleaned_df_4 %>% 
  group_by(member_casual) %>% 
  summarise(ride_id=n())
 
write.csv(no_of_rides, file = "no_of_rides.csv")

length_of_rides <-
  cleaned_df_4 %>% 
  group_by(member_casual) %>% 
  summarise(sum(ride_length))

write.csv(length_of_rides, file = "length_of_rides.csv")