# Library
library(tidyverse)
library(RColorBrewer)
library(scales)

# To read raw data and combine into one file name "tripdata_2021"

tripdata_202101 <- read.csv("raw_data/202101-divvy-tripdata.csv")
tripdata_202102 <- read.csv("raw_data/202102-divvy-tripdata.csv")
tripdata_202103 <- read.csv("raw_data/202103-divvy-tripdata.csv")
tripdata_202104 <- read.csv("raw_data/202104-divvy-tripdata.csv")
tripdata_202105 <- read.csv("raw_data/202105-divvy-tripdata.csv")
tripdata_202106 <- read.csv("raw_data/202106-divvy-tripdata.csv")
tripdata_202107 <- read.csv("raw_data/202107-divvy-tripdata.csv")
tripdata_202108 <- read.csv("raw_data/202108-divvy-tripdata.csv")
tripdata_202109 <- read.csv("raw_data/202109-divvy-tripdata.csv")
tripdata_202110 <- read.csv("raw_data/202110-divvy-tripdata.csv")
tripdata_202111 <- read.csv("raw_data/202111-divvy-tripdata.csv")
tripdata_202112 <- read.csv("raw_data/202112-divvy-tripdata.csv")
tripdata_2021 <- bind_rows(tripdata_202101, tripdata_202102, tripdata_202103, tripdata_202104, tripdata_202105, tripdata_202106, 
                           tripdata_202107, tripdata_202108, tripdata_202109, tripdata_202110, tripdata_202111, tripdata_202112)


# To add column "date", "month", "day", "year", "day_of_week" of each ride

tripdata_2021$date <- as.Date(tripdata_2021$started_at)
tripdata_2021$month <- format(as.Date(tripdata_2021$date), "%m")
tripdata_2021$day <- format(as.Date(tripdata_2021$date), "%d")
tripdata_2021$year <- format(as.Date(tripdata_2021$date), "%Y")
tripdata_2021$day_of_week <- format(as.Date(tripdata_2021$date), "%A")

# To calculate ride length, calculate different of time of each ride

tripdata_2021$ride_length <- difftime(tripdata_2021$ended_at, tripdata_2021$started_at)

# By now the columns date, month, day, year, day_of_week and ride_length have been added
# To order date starting with Sunday

tripdata_2021$day_of_week <- ordered(tripdata_2021$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# To analyse ridership data with weekday

tripdata_2021 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration_min = mean(ride_length/60)) %>%
  arrange(member_casual, weekday)%>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(labels = comma)+  theme(legend.title=element_blank())

# To plot the average duration

tripdata_2021 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n(), average_duration_min = mean(ride_length/60)) %>%
  arrange(member_casual, weekday) %>%  
  ggplot(aes(x = weekday, y = average_duration_min, fill = member_casual)) + 	
  geom_col(position =   "dodge")+  
  scale_fill_brewer(palette = "Pastel1") +    	
  labs(title = "How Do They Ride?")+  
  scale_y_continuous(labels = comma)+
  theme(legend.title=element_blank())

# Number of rides with rideable type and casual/member

tripdata_2021 %>%  
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, rideable_type, weekday) %>%  
  summarise(number_of_rides = n(), average_duration_min = mean(ride_length/60)) %>%  
  ggplot(aes(x = weekday, y = number_of_rides, fill = rideable_type)) + 
  geom_col(position = "dodge")+  scale_fill_brewer(palette = "Set2") +   
  scale_y_continuous(labels = comma)+  
  facet_wrap(~member_casual)

# Duration with rideable type and casual/member

tripdata_2021 %>%  
  mutate(weekday = wday(started_at, label = TRUE)) %>%  
  group_by(member_casual, rideable_type, weekday) %>%  
  summarise(number_of_rides = n(), average_duration_min = mean(ride_length/60)) %>%  
  ggplot(aes(x = weekday, y = average_duration_min, fill = rideable_type)) + 
  geom_col(position = "dodge") +  
  scale_fill_brewer(palette = "Set2") +   
  scale_y_continuous(labels = comma)+  
  facet_wrap(~member_casual)

# At what time do they ride?

tripdata_2021 %>%  
  mutate(start_time = hour(started_at)) %>%  
  group_by(member_casual, rideable_type, start_time) %>%  
  summarise(number_of_rides = n(), average_duration_min = mean(ride_length/60)) %>%  
  ggplot(aes(x = start_time, y = number_of_rides, fill = rideable_type)) + 
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = comma)+  
  facet_wrap(~member_casual)

# Usage by month

tripdata_2021 %>%  
  mutate(start_month = month(started_at)) %>%  
  group_by(member_casual, rideable_type, start_month) %>%  
  summarise(number_of_rides = n(), average_duration_min = mean(ride_length/60)) %>% 
  ggplot(aes(x = start_month, y = number_of_rides, fill = member_casual)) + 
  geom_col(position = "stack") +   scale_fill_brewer(palette = "Pastel1") +   
  scale_x_continuous(limits = c(0,13), breaks = breaks_width(1)) + 
  scale_y_continuous(labels = comma) + 
  labs(title = "Usage By Month") +  
  theme(legend.title=element_blank())



  
  
  
