#loading packages
library(tidyverse)
library(ggplot2)
library(lubridate)
library(janitor)
library(readr)

#importing the data
September <- read.csv("202109-divvy-tripdata.csv")
October <- read.csv("202110-divvy-tripdata.csv")
November <- read.csv("202111-divvy-tripdata.csv")
December <- read.csv("202112-divvy-tripdata.csv")
January <- read.csv("202201-divvy-tripdata.csv")
February <- read.csv("202202-divvy-tripdata.csv")
March <- read.csv("202203-divvy-tripdata.csv")
April <- read.csv("202204-divvy-tripdata.csv")
May <- read.csv("202205-divvy-tripdata.csv")
June <- read.csv("202206-divvy-tripdata.csv")
July <- read.csv("202207-divvy-tripdata.csv")
August <- read.csv("202208-divvy-tripdata.csv")

#merging all the data into one dataframe
alldata <- rbind(September,October,November,December,January,February,March,April,May,June,July,August)

#removing the data frames we have just merged and just leaving that one. Just to clean the environment
rm(January,February,March,May,April,June,July,August,September,October,November,December)

#checking the structure of the data
str(alldata)

# adding columns for: year, month, day and hour of the ride 
clean_data <- alldata %>%
  mutate(ride_hour = hour(started_at)) %>%
  mutate(ride_month = month(started_at, label = TRUE)) %>%
  mutate(ride_day = wday(started_at,label=TRUE, abbr=FALSE)) %>%
  mutate(ride_year = year(started_at))

#changing data type for the dates since they are characters
clean_data$started_at <- ymd_hms(clean_data$started_at)
clean_data$ended_at <- ymd_hms(clean_data$ended_at)

#adding column of ride duration
clean_data$ride_duration <- as.numeric(difftime(clean_data$ended_at, clean_data$started_at, units = "mins"))
clean_data$ride_duration <- round(clean_data$ride_duration,2)

#checking how many inputs are before and after removing and filtering (this will be reused)
dim(clean_data)

#filtering ride duration leaving just positive results
clean_data <- filter(clean_data, ride_duration > 0)

#checking the rows with NA values to see if there can be a pattern or reason for them to exist
sum(is.na(clean_data))
View(clean_data %>% filter(!complete.cases(.)))

#removing NA values since I could not find any reason for them to be null
clean_data <- na.omit(clean_data)

#looking for blanks and leaving rows without them
clean_data %>% 
  filter(member_casual == ""|member_casual ==" ")
clean_data %>% 
  filter(start_station_name == ""|start_station_name ==" ")
clean_data <- filter(clean_data, start_station_name != "")
clean_data <- filter(clean_data, end_station_name != "")

#looking for duplicated values
sum(duplicated(clean_data$ride_id))

#analyzing the data

#getting a general idea of the numeric variables
summary(clean_data$ride_duration)

#amount of each membership in %
prop.table(table(clean_data$member_casual))*100

#amount of each bicycle type and usage by membership + plot
table(clean_data$rideable_type)
round(prop.table(table(clean_data$member_casual, clean_data$rideable_type),2)*100)

ggplot(clean_data) +
  geom_bar(aes (x= rideable_type,)) + facet_wrap(~member_casual) +
  labs(title="Use of each type of bicycle by member type")

#percentage of each membership by month, day and hour of the ride
round(prop.table(table(clean_data$member_casual, clean_data$ride_month),2)*100)
round(prop.table(table(clean_data$member_casual, clean_data$ride_day),2)*100)
round(prop.table(table(clean_data$member_casual, clean_data$ride_hour),2)*100)

#sum of rides and average of its duration for each membership by weekday + plot for both 
clean_data %>% 
  group_by(member_casual, ride_day) %>% 
  summarise(total_rides = n()) %>% 
  arrange(ride_day) %>% 
  ggplot(aes(x=ride_day,y=total_rides,fill=member_casual))+geom_col(position = "dodge")+
  labs(title="Rides per weekday by member type")

clean_data %>% 
  group_by(member_casual, ride_day) %>% 
  summarise(avg_duration = mean(ride_duration)) %>% 
  ggplot(aes(x=ride_day,y=avg_duration,color=member_casual))+geom_point()+
  labs(title="Average ride lenght for each weekday by member type")

#total of rides for  each membership by month + plot
clean_data %>% 
  group_by(member_casual, ride_month) %>% 
  summarise(total_rides = n()) %>% 
  ggplot(aes(x=ride_month,y=total_rides,fill=member_casual))+geom_col(position = "dodge")+
  labs(title="Rides per month by member type")

# total of rides for  each membership by hour of the day + plot
clean_data %>% 
  group_by(member_casual, ride_hour) %>% 
  summarise(total_rides = n()) %>% 
  ggplot(aes(x=ride_hour,y=total_rides,fill=member_casual))+geom_col(position = "dodge")+
  labs(title="Rides by hour of the day for each member type")

#looking which station is the most frequented one
clean_data %>% 
  group_by(start_station_name) %>% 
  summarise(most_used_stations = n()) %>% 
  arrange(-most_used_stations)

clean_data %>% 
  group_by(end_station_name) %>% 
  summarise(most_used_stations = n()) %>% 
  arrange(-most_used_stations)

#exporting to a .CSV file the data now that it is clean and analyzed
write.csv(clean_data, "Clean_data.csv")