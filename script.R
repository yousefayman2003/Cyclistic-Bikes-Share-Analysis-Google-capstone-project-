library(tidyverse)
library(lubridate)
library(ggplot2)
library(hydroTSM)
library(scales)


rm(list = ls()) #clearing environment

#importing data

a1 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202108-divvy-tripdata.csv")
a2 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202109-divvy-tripdata.csv")
a3 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202110-divvy-tripdata.csv")
a4 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202111-divvy-tripdata.csv")
a5 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202112-divvy-tripdata.csv")
a6 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202201-divvy-tripdata.csv")
a7 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202202-divvy-tripdata.csv")
a8 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202203-divvy-tripdata.csv")
a9 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202204-divvy-tripdata.csv")
a10 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202205-divvy-tripdata.csv")
a11 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202206-divvy-tripdata.csv")
a12 <- read.csv("C:/Users/My PC/Desktop/google capstone/divvy-dataset/202207-divvy-tripdata.csv")


data <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) #combining all data




     
View(data)
glimpse(data)
str(data)
summary(data)
#cleaning the data

data <- data %>% 
            select(2,3,4,13) #selecting the date i need

unique(data$rideable_type) #seeing the unique values of the ride type
unique(data$member_casual) #seeing the unique values of riders

#transforming the data
data <- data %>% 
              mutate(ride_length = difftime(data$ended_at,data$started_at))#calculate the duration of the ride
            
sapply(data , class) #checking of data types of my columns

data$date <- as.Date(data$started_at) #adding date column

data$year <- format(as.Date(data$date), "%Y") #adding year column

data$month <-  months(data$date) #adding month column

data$day_of_week <- format(as.Date(data$date), "%A") #adding day  column

data <- data %>% 
  mutate(season = time2season(date,                
                              out.fmt = "seasons")) # Convert dates to seasons

data <- data %>% 
  arrange(date) #sorting the data by date

data$day_of_week <- ordered(data$day_of_week, levels=c("Sunday", "Monday", "Tuesday",
                                                       "Wednesday", "Thursday", "Friday", "Saturday")) # ordering day of the week
data$month <- ordered(data$month , level=c("January", "February", "March",
                                           "April", "May", "June", "July",
                                           "August", "September", "October", "November", "December")) # ordering months 
View(data)

str(data)

data$ride_length <- as.numeric(as.character(data$ride_length)) #converting column data type to numeric

data$ride_length <- data$ride_length/60 #converting ride length from sec to mins

data <- data %>% 
        filter(!(ride_length < 0))#filtering data

#analyzing the data


aggregate(data$ride_length ~ data$member_casual, FUN = max)# Comparing members and casual users max
aggregate(data$ride_length ~ data$member_casual, FUN = min)# Comparing members and casual users min
aggregate(data$ride_length ~ data$member_casual, FUN = median)# Comparing members and casual users median
aggregate(data$ride_length ~ data$member_casual, FUN = mean) # Comparing members and casual users mean

# analyze ridership data by type and weekday
data %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)	# sorts

#calculating total number of rides for each season
num_of_rides_season <- data %>% 
  group_by(member_casual, data$season) %>% 
  summarise(number_of_rides = n()) 

#calculating total number of rides for each month
num_of_rides_month <- data %>% 
  group_by(member_casual, data$month) %>% 
  summarise(number_of_rides = n())

#calculating total number of rides for each day
num_of_rides_day <- data %>% 
  group_by(member_casual, data$day_of_week) %>% 
  summarise(number_of_rides = n())

#calculating total number of ride type 
num_of_rideable_type <- data %>% 
  group_by(member_casual, data$rideable_type) %>% 
  summarise(number_of_rides = n())

#calculating average  time of rides for each day
avg_day <- aggregate(data$ride_length ~ data$member_casual + data$day_of_week, FUN = mean)
#calculating average  time of rides for each month
avg_month <- aggregate(data$ride_length ~ data$member_casual + data$month, FUN = mean)
#calculating average  time of rides for each season
avg_season <- aggregate(data$ride_length ~ data$member_casual + data$season, FUN = mean)
#calculating average  time of rides for each ride type
avg_rideable_type <- aggregate(data$ride_length ~ data$rideable_type + data$member_casual, FUN = mean)

#Visualize the data

#visualize the number of rides per day
data %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  rename(user_type = member_casual) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#0000ff", "#ff8c00")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Number Of Rides Per Day" , x = "Day / User Type", 
       y = "Number Of Rides") 


#Creating a visualization for average duration per day
data %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  rename(user_type = member_casual) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#0000ff", "#ff8c00")) +
  labs(title = "Avg Ride Duration Per Day" ,x = "Day / User Type", 
       y = "Time(min)")

#Creating a visualization for number of rides per month
data %>% 
  mutate(months= month(started_at, label = TRUE)) %>% 
  rename(user_type = member_casual) %>% 
  group_by(months, user_type) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user_type, months)  %>% 
  ggplot(aes(x = months, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#0000ff", "#ff8c00")) +
  scale_y_continuous(labels = comma) +
 labs(title = "Number Of Rides Per Month", x = "Month / User Type", 
      y = "Number Of Rides") 

#Creating a visualization for average duration per month
data %>% 
  mutate(months= month(started_at, label = TRUE )) %>% 
  rename(user_type = member_casual) %>% 
  group_by(months, user_type) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, months)  %>% 
  ggplot(aes(x = months, y = average_duration, fill = user_type)) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("#0000ff", "#ff8c00")) +
  labs(title = "Average Ride Duration Per Month", x = "Month / User Type", 
       y = "Time(min)") 

#Creating a visualization for number of rides per season
data %>% 
  rename(user_type = member_casual) %>% 
  group_by(season,user_type) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user_type,season) %>% 
  ggplot(aes(x = season , y = number_of_rides , fill = user_type)) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("#0000ff", "#ff8c00")) +
  labs(title = "Number Of Rides Per Season" , x = "Season / User Type"
       , y = "Number Of Rides")

#Creating a visualization for which type of bikes is used the most
data %>% 
  rename(user_type = member_casual) %>% 
  group_by(rideable_type,user_type) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(user_type,rideable_type) %>% 
  ggplot(aes(x = rideable_type , y = number_of_rides , fill = user_type )) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values = c("#0000ff", "#ff8c00")) +
  labs(title = "Which Type Of Bikes Is Used The Most" , x = "Bike Type / User type"
       , y = "Number Of Rides")

#Export the data for data vz or future analysis
write.csv(data , file = "C:\\Google_Capstone_Project\\Cyclistic_bike_share_cleaned.csv")
write.csv(avg_season , file = "C:\\Google_Capstone_Project\\ride_season_avg_length.csv")
write.csv(avg_rideable_type , file = "C:\\Google_Capstone_Project\\rideable_type_avg_length.csv")
write.csv(avg_month , file = "C:\\Google_Capstone_Project\\ride_month_avg_length.csv")
write.csv(avg_day, file = "C:\\Google_Capstone_Project\\ride_day_avg_length.csv")
write.csv(num_of_rides_season, file = "C:\\Google_Capstone_Project\\ride_season_total_length.csv")
write.csv(num_of_rides_month, file = "C:\\Google_Capstone_Project\\ride_month_total_length.csv")
write.csv(num_of_rides_day, file = "C:\\Google_Capstone_Project\\ride_day_total_length.csv")
write.csv(num_of_rideable_type, file = "C:\\Google_Capstone_Project\\ride_type_total_length.csv")






















