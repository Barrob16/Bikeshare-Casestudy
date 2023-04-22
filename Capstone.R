#Download the packages we will need to perform data analysis
library("tidyverse")
library("ggplot2")
library("dplyr")
library("lubridate")


## Read the CSV files into R
q1 <- read_csv("2019_Q1.csv")
q2 <- read_csv("2019_Q2.csv")
q3 <- read_csv("2019_Q3.csv")
q4 <- read_csv("2019_Q4.csv")

# need to standardize column names in q2 Data Frame
# so I can eventually combine the databases into one csv
# Do I need to Cast any data types for better functionality?


## Rename Columns in q2 dataframe to be standardized with all other data frames
q2 <- q2 %>% 
  rename("trip_id" = "01 - Rental Details Rental ID",
        "start_time"  = "01 - Rental Details Local Start Time",
        "end_time" = "01 - Rental Details Local End Time",
        "bikeid" = "01 - Rental Details Bike ID",
        "tripduration" = "01 - Rental Details Duration In Seconds Uncapped",
        "from_station_id" = "03 - Rental Start Station ID",
        "from_station_name" = "03 - Rental Start Station Name",
        "to_station_id" = "02 - Rental End Station ID",
        "to_station_name" = "02 - Rental End Station Name",
        "usertype" = "User Type",
        "gender" = "Member Gender",
        "birthyear" = "05 - Member Details Member Birthday Year")


## combine the four quarterly datasets into one year long data set
yeardata <- bind_rows(q1, q2, q3, q4)


## Change names of values in Usertype for easier understanding
yeardata$usertype[yeardata$usertype == 'Subscriber'] <- 'Annual Membership'
yeardata$usertype[yeardata$usertype == 'Customer'] <- 'Single Use'


## Create a new column that extracts just the month from start_time in 1-12 format
yeardata <- yeardata %>% 
  mutate(trip_month=month(yeardata$start_time))


## Casts the data type from drtn to hh/mm/ss format 
yeardata$ride_length <- hms::hms(seconds_to_period(yeardata$ride_length))


##trip length. Ride length with difftime function
yeardata$ride_length <- difftime(yeardata$end_time,yeardata$start_time)


# Aggregate the date and time into seperate categories of Day, month, Day of week for analysis
yeardata$date <- as.Date(yeardata$start_time) 
yeardata$month <- format(as.Date(yeardata$date), "%m")
yeardata$day <- format(as.Date(yeardata$date), "%d")
yeardata$day_of_week <- format(as.Date(yeardata$date), "%A")


# Rearrange the display order of day_of_week for context of future charts.
yeardata$day_of_week <- ordered(yeardata$day_of_week, levels=c( "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


## Plots a bar graph of usertypes
ggplot(data=yeardata)+
  geom_bar(mapping=aes(x=usertype,fill=usertype))


## Plots a bar graph of trips per month
ggplot(data=yeardata)+
  geom_bar(mapping=aes(x=trip_month,fill=usertype))


## Plots a bar graph of trips per month split by usertype
ggplot(data=yeardata)+
  geom_bar(mapping=aes(x=trip_month,fill=usertype))+
  facet_wrap(~usertype)


## Plot bar graph with difference in genders in terms of users 
ggplot(data=yeardata)+
  geom_bar(mapping=aes(x=gender,fill=gender)) +
  facet_wrap(~usertype) +
  labs(title="Gender breakdown of riders", subtitle="Graphs split with usertype")


## Stations most departed from graphed, un-ordered
ggplot(data=yeardata)+
  geom_bar(mapping=aes(x=from_station_name))+
  labs(title="Stations departed from", subtitle = "unordered")



## Stations most departed from graphed, Ordered in Desc order (not working)
ggplot(data=yeardata)+
  geom_bar(mapping=aes(x=reorder(from_station_name, function(x)-length(x))))

  
## Users by birthyear 
## uses coord_cartesian(xlim) to remove front and back end of the graph with unnecesary decades i.e. 1890, 2030
ggplot(data=yeardata)+
  geom_bar(mapping=aes(x=birthyear,fill=gender))+
  coord_cartesian(xlim = c(1945, 2005))+
  labs("Users by birthyear")


## Graphs day of week usage over year averages split by usertype
## fix days of week to start with Mon>sun
## How to graph them stacked?
ggplot(data=yeardata)+
  geom_bar(mapping=aes(x=day_of_week,fill=usertype))+
  facet_wrap(~usertype)+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))


### Remove negative value trip lengths
yeardata %>% 
  select(ride_length,from_station_name,to_station_name, usertype) %>% 
  filter(ride_length <= 0.0) %>% 
  view()

yeardata <- yeardata[yeardata$ride_length >= 0, ]

yeardata %>% 
  select(ride_length,from_station_name,to_station_name, usertype) %>% 
  filter(ride_length <= 0.0) %>% 
  view()

###
yeardata %>% 
  select(mean(yeardata$ride_length)) %>% 
  filter(yeardata$usertype == 'Single Use') %>% 
  view()

yeardata$ride_length <-as.numeric(yeardata$ride_length)
view(summarize(yeardata$ride_length))
view(filter(yeardata$tripduration <= 0))
view(head(yeardata,n=10))
summarize(yeardata$tripduration)


yeardata %>% 
  select(yeardata$ride_length) %>% 
  filter(yeardata$usertype == 'Single Use') %>% 
  mean()



!duplicated(yeardata$trip_id)
table
yeardata %>% 
  distinct()



