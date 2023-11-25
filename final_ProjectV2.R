
# Case Study: Cyclistic
##Google Data Analytics Capstone Project
### Qian Li
### 2022-05-17
#=====================



#load libraries 
library(tidyverse) #data management and visualization
library(lubridate) # working with dates
library(dplyr) # data manipulation, mutate() creates new variables
library(janitor) # # data cleaning and tables
library(plotly) # used for creating interactive web-based graphs via plotly's JavaScript graphing library
library(scales)

# Set theme
theme_set(theme_minimal())
theme_update(
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  plot.title.position = "plot"
)



# STEP 1. IMPORT DATA
#=====================

# Read CSV files into tibbles
X202204_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202204-divvy-tripdata.csv")
X202203_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202203-divvy-tripdata.csv")
X202202_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202202-divvy-tripdata.csv")
X202201_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202201-divvy-tripdata.csv")
X202112_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202112-divvy-tripdata.csv")
X202111_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202111-divvy-tripdata.csv")
X202110_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202110-divvy-tripdata.csv")
X202109_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202109-divvy-tripdata.csv")
X202108_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202108-divvy-tripdata.csv")
X202107_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202107-divvy-tripdata.csv")
X202106_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202106-divvy-tripdata.csv")
X202105_divvy_tripdata <- read_csv("C:/Users/Antti-Pekka/Desktop/google stoneProject/csv/202105-divvy-tripdata.csv")

# STEP 2:COMBINE INTO A SINGLE FILE
#====================================================


# Bind all tibbles into one
all_trips <-  rbind (X202105_divvy_tripdata,
                     X202106_divvy_tripdata,
                     X202107_divvy_tripdata,
                     X202108_divvy_tripdata,
                     X202109_divvy_tripdata,
                     X202110_divvy_tripdata,
                     X202111_divvy_tripdata,
                     X202112_divvy_tripdata,
                     X202201_divvy_tripdata,
                     X202202_divvy_tripdata,
                     X202203_divvy_tripdata,
                     X202204_divvy_tripdata)

# Remove individual month data frames to clear up space in the environment 
remove(X202105_divvy_tripdata,
       X202106_divvy_tripdata,
       X202107_divvy_tripdata,
       X202108_divvy_tripdata,
       X202109_divvy_tripdata,
       X202110_divvy_tripdata,
       X202111_divvy_tripdata,
       X202112_divvy_tripdata,
       X202201_divvy_tripdata,
       X202202_divvy_tripdata,
       X202203_divvy_tripdata,
       X202204_divvy_tripdata)

# Create new data frame to contain new columns
all_trips_V1 <- all_trips 

# Verify data by returning first part of the data
head(all_trips_V1)

# Get a glimpse of the data
glimpse(all_trips_V1)

# STEP 3: PROCESS DATA
#==============================================


#Clean data
# Removes all rows with NAs .In this case, where missing values represent trip duration less than 60 sec ,they could be false starts or users trying to re-dock a bike to ensure it was secure. 
# limit trips longer than 24 hours, no overnight stay

all_trips_V1.1<- all_trips %>% 
  drop_na() %>% 
  mutate(trip_duration = interval(started_at, ended_at) %>% 
           as.duration(),
         .after = ended_at) %>% 
  filter(trip_duration >= 60 & trip_duration <= (60 * 60 * 24))

# View  the data
colnames(all_trips_V1.1)
View(all_trips_V1.1)

# Add columns for date, year, month, day, day of week, and hour of trip
all_trips_v2 <- all_trips_V1.1 %>% 
  mutate(date = date(started_at),
         year = year(started_at),
         month = month(started_at, label = TRUE, abbr = FALSE),
         day = day(started_at),
         day_of_week = wday(started_at, label = TRUE, abbr = FALSE),
         hour = hour(started_at),.after = ended_at # move  year, month, day, day of week, and hour columns to specific position using .after
         )
View(all_trips_v2)


# Create a column for season based on the "month" column using case_when() and mutate() function
all_trips_v2 <- all_trips_v2 %>% mutate(season = 
                                         case_when(
                                           month == "December" | month == "January" | month == "February" ~ "winter",
                                           month == "March" | month == "April" | month == "May" ~ "spring",
                                           month == "June" | month == "July" | month == "August" ~ "summer",
                                           month == "September" | month == "October" | month == "November" ~ "autumn"), 
                                        )
all_trips_v2 <-all_trips_v2 %>% relocate(season, .after = year )# relocate season column after year column
                                         
                                            

View(all_trips_v2)
# Create column for different time_of_day: Night, Morning, Afternoon, Evening
all_trips_v2 <-all_trips_v2 %>% mutate(time_of_day = 
                                             case_when(hour == "0" ~ "Night",
                                                       hour == "1" ~ "Night",
                                                       hour == "2" ~ "Night",
                                                       hour == "3" ~ "Night",
                                                       hour == "4" ~ "Night",
                                                       hour == "5" ~ "Night",
                                                       hour == "6" ~ "Morning",
                                                       hour == "7" ~ "Morning",
                                                       hour == "8" ~ "Morning",
                                                       hour == "9" ~ "Morning",
                                                       hour == "10" ~ "Morning",
                                                       hour == "11" ~ "Morning",
                                                       hour == "12" ~ "Afternoon",
                                                       hour == "13" ~ "Afternoon",
                                                       hour == "14" ~ "Afternoon",
                                                       hour == "15" ~ "Afternoon",
                                                       hour == "16" ~ "Afternoon",
                                                       hour == "17" ~ "Afternoon",
                                                       hour == "18" ~ "Evening",
                                                       hour == "19" ~ "Evening",
                                                       hour == "20" ~ "Evening",
                                                       hour == "21" ~ "Evening",
                                                       hour == "22" ~ "Evening",
                                                       hour == "23" ~ "Evening"))
                                                        



all_trips_v2 <-all_trips_v2 %>% relocate(time_of_day, .after = day_of_week)# relocate time_of_day column after day_of_week column
View(all_trips_v2)

#Create columns for route and type of trip based on the "start_station_name" and "end_station_name" columns.

all_trips_v2 <- all_trips_v2 %>%  
  mutate(trip_route = paste(start_station_name,"to",end_station_name),
         .after = end_station_id) %>% 
  mutate(trip_type = case_when(
    start_station_name == end_station_name ~ "round_trip", 
    start_station_name != end_station_name ~ "one_way"), 
    .after = trip_route)

colnames(all_trips_v2)
View(all_trips_v2)


# Remove unwanted column(s): ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng by name of the dataset
all_trips_v2 <- all_trips_v2 %>%  
  select(-c(ride_id, start_station_id, end_station_id,start_lat,start_lng,end_lat,end_lng)) 


View(all_trips_v2  )

#======================================================
# Inspect the modified dataframe
colnames(all_trips_v2  )  #List of column names
nrow(all_trips_v2  )  #total number of rides
dim(all_trips_v2 )  #Dimensions
head(all_trips_v2  )  #overview the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips_v2 )  #List of columns and data types (numeric, character, etc)
summary(all_trips_v2 )  #Statistical summary of data

# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
#  Summary statistics for trip_duration  (all figures in seconds)

mean(all_trips_v2$trip_duration ) #straight average (total ride length / rides)
median(all_trips_v2$trip_duration ) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$trip_duration ) #longest ride
min(all_trips_v2$trip_duration ) #shortest ride
summary(all_trips_v2$trip_duration )  #Statistical summary on the specific column


# Frequency tables for user(rider) type 
all_trips_v2 %>% 
  tabyl(member_casual)


# Frequency tables for bike type(classic_bike. docked_bike, electric_bike)
all_trips_v2 %>% 
  tabyl(rideable_type)

# Frequency tables for trip type(one_way, round_trip)
all_trips_v2 %>% 
  tabyl(trip_type)

# Frequency tables for season
all_trips_v2 %>% 
  tabyl(season)



#Plot user types( member VS casual)
all_trips_v2 %>% 
  group_by(member_casual) %>%  # using group_by() to divide the data into subgroups based on rider type 
  count() %>%  # HOw many member/ casual riders do all_trips_v2 has?
  mutate(percent = n / nrow(all_trips_v2)) %>%    # create a new column percentage
  ggplot(aes(x = member_casual, 
             y = percent, 
             fill = member_casual)) + 
  geom_col(width = 0.75) + 
  guides(fill = "none")+
  scale_y_continuous(labels = percent_format()) +    #set values for continuous y-axis ,Format Number as Percentage
  labs(title = "Annual Members vs. Casual Riders", 
       subtitle = "January to December 2021", 
       x = NULL, 
       y = "% of trips") 
  geom_text(aes(label = paste0(round(percent * 100), "%")), 
          color = "white", 
          vjust = 1.5)  
 
  
  # Answer business question
  #=================================================
  #  In which month are the most bikes used?
  # July is the overall most popular month for the most casual user ( per type)
  # August is the overall most popular month for the most member user 
  
  
  # Plot trips per month
 all_trips_v2 %>%       
    group_by(member_casual, month) %>%  
    count() %>%  # aggregated count how many member/ casual riders do Jan/Feb/.../ have?
    ggplot(aes(x =month, #x=date
               y =n,     # y= value
               fill = member_casual))+  
    geom_col(position ="dodge",width =0.75) +  
    scale_y_continuous(labels = comma) + 
    labs(title = "Trips per Month",  
         subtitle ="May2021 to April2022",
         x=NULL,  
         y=NULL,
         fill = NULL) +
    theme(axis.text.x = element_text(angle =90)) 
  
  #=================================================
  # What season has the most member user /casual user?
  # Summer for both member and casual user
  all_trips_v2 %>%
    group_by(member_casual, season) %>% 
    count() %>%   #How many member/ casual riders do Spring/ Summer /.../ have?
    ggplot(aes(x=season,
               y=n,
               fill= member_casual))+
    geom_col(position="dodge", width =0.75)+
    scale_y_continuous(labels = comma)+
    labs(title = "Trips per Season",
         subtitle = "May2021 to April2022",    
         x=NULL,
         y=NULL,
         fill = NULL)
   
  #which day of the week has the most users?
  # Casual user  Sunday, member Wednesday
  all_trips_v2 %>%
    group_by(member_casual, day_of_week)%>% 
    count()%>%
    ggplot(aes(x=day_of_week,
               y=n,
               fill=member_casual))+
    geom_col(position="dodge", width=0.75)+
    scale_y_continuous(labels = comma)+
    labs(title="Trips per weekday",
         subtitle ="May2021 to April2022",
         x=NULL,
         y=NULL,
         fill = NULL)
  #whatpart of the day has the most users?
  #afternoon
  all_trips_v2 %>%
    group_by(member_casual, time_of_day)%>% 
    count()%>%
    ggplot(aes(x=time_of_day,
               y=n,
               fill=member_casual))+
    geom_col(position="dodge", width=0.75)+
    scale_y_continuous(labels = comma)+
    labs(title="Trips per hour",
         subtitle ="May2021 to April2022",
         x=NULL,
         y=NULL,
         fill = NULL)
  
  # what time has the most trips per hour
  #17:00 is highest
  all_trips_v2 %>% 
    group_by(member_casual, hour) %>%     
    count() %>% 
    ggplot(aes(x = hour, 
               y = n, 
               fill = member_casual)) +
    geom_col(position = "dodge",
             width = 0.75) +
    scale_y_continuous(labels = comma) +
    labs(title = "Trips per Hour", 
         subtitle = "May2021 to April2022", 
         x = NULL, 
         y = NULL, 
         fill = NULL)
  
 #  Who take a longer trips (median)member_casual?
 # Casual riders took longer trips than annual members.
  
  all_trips_v2 %>% 
    group_by(member_casual) %>% 
    summarise(average_duration = mean(trip_duration) %>% 
                as.duration()) %>% 
    ggplot(aes(x = member_casual, 
               y = average_duration, 
               fill = member_casual)) +
    geom_col(width = 0.75) +
    guides(fill = "none") +
    labs(title = "Average Trip Duration", 
         subtitle = "May2021 to April2022",
         x = NULL, 
         y = "seconds") +
    geom_text(aes(label = round(average_duration)), 
              vjust = 1.5, # vjust  ["bottom","middle","top","inward","outward"]
              color = "white") # 
    
  
  # Plot average trip duration by season
  #Spring casual users took longer trip
  all_trips_v2 %>% 
    group_by(member_casual, season) %>% 
    summarise(average_duration = mean(trip_duration) %>% 
                as.duration()) %>% 
    ggplot(aes(x = season, 
               y = average_duration, 
               fill = member_casual)) +
    geom_col(position = "dodge",
             width = 0.75) +
    scale_y_continuous(labels = comma) +
    labs(title = "Average Trip Duration by Season", 
         subtitle = "May2021 to April2022",
         x = NULL, 
         y = "seconds", 
         fill = NULL)
    
          
# average trip duration by season
  #Spring casual user taak longer trip, summer for annual users.
  all_trips_v2 %>% 
    group_by(member_casual, season) %>% 
    summarise(average_duration = mean(trip_duration) %>% 
                as.duration()) %>% 
    ggplot(aes(x = season, 
               y = average_duration, 
               fill = member_casual)) +
    geom_col(position = "dodge",
             width = 0.75) +
    scale_y_continuous(labels = comma) +
    labs(title = "Average Trip Duration by Season", 
         subtitle = "May2021 to April2022",
         x = NULL, 
         y = "seconds", 
         fill = NULL)
    
 # Plot average trip duration by day of week
  #Sunday, Saturday
 all_trips_v2 %>% 
   group_by(member_casual,day_of_week)%>% 
   summarise(average_duration = mean(trip_duration) %>% 
               as.duration()) %>% 
   
   ggplot(aes(x=day_of_week,
              y=average_duration ,
              fill=member_casual)) +
   geom_col(position="dodge",
            width =0.75)+
   scale_y_continuous(labels = comma)+
   labs(title = "Average Trip Duration by weekday", 
        subtitle = "May2021 to April2022",
        x = NULL, 
        y = "seconds", 
        fill = NULL)
   

 # Plot average trip duration by hour
 all_trips_v2 %>% 
   group_by(member_casual, hour) %>% 
   summarise(average_duration = mean(trip_duration) %>% 
               as.duration()) %>% 
   ggplot(aes(x = hour, 
              y = average_duration, 
              fill = member_casual)) +
   geom_col(position = "dodge",
            width = 0.75) +
   scale_y_continuous(labels = comma) +
   labs(title = "Average Trip Duration by Hour", 
        subtitle = "May2021 to April2022",
        x = NULL, 
        y = "seconds", 
        fill = NULL) +
   theme(axis.text.x = element_text(angle = 90))
 
 # Plot bike types ( classica bike or eledric bike)
 # What type of bike do casual user like?
 # what kind of bike do member user like?
 #Answer:Riders preferred classic bikes over electric bikes.
 all_trips_v2 %>% 
   filter(rideable_type != "docked_bike") %>% #annual member do not have docked_bike, so filter it out. 
   group_by(member_casual, rideable_type) %>%  #data frame was splited into  "member_casual"( one value for member. another value is casual), "rideable_type"( one value is for classic_bike, another one is electrick_bike)
   count() %>% 
   group_by(member_casual) %>%  # the data frame was split into two separate little data frames (i.e., one for member and one for casual),  – and then count the rows.
   mutate(percent = n / sum(n)) %>%  #  apply our calculation separately to each smaller data frame，In this case, the proportion of [casual user ]in our data who are using classic _bike  can be calculated as the number who are casual user using classic bike(n) divided by the total number of casual user in the data(sum N) .we used dplyr’s mutate() function to create a new variable in the data frame named percent,
   ggplot(aes(x = member_casual, 
              y = percent, 
              fill = rideable_type)) +
   geom_col(position = "dodge",
            width = 0.75) +
   scale_y_continuous(labels = percent_format()) +
   labs(title = "Bike Preference", 
        subtitle = "May2021 to April2022",
        x = NULL, 
        y = "% of trips", 
        fill = NULL) +
   geom_text(aes(label = paste0(round(percent * 100), "%")), 
             position = position_dodge(0.7),
             color = "white", 
             vjust = 1.5)
 
 #Plot trip types( one way , round way)
 #What type of trip do casual user do?
 #What type of trip do member user do?
 #Answer:Riders preferred one way trips over round trips.
 
 all_trips_v2 %>% 
   group_by(member_casual, trip_type) %>%  #data frame was spited into  "member_casual"( one value for member. another value is casual), "trip_type"( one value is for one_ way, another one is round_trip)
   count() %>% 
   group_by(member_casual) %>%  # the data frame was split into two separate little data frames (i.e., one for member and one for casual),  – and then count the rows.
   mutate(percent = n / sum(n)) %>%  #  apply our calculation separately to each smaller data frame，In this case, the proportion of [casual user ]in our data who are using classic _bike  can be calculated as the number who are casual user using classic bike(n) divided by the total number of casual user in the data(sum N) .we used dplyr’s mutate() function to create a new variable in the data frame named percent,
   ggplot(aes(x = member_casual, 
              y = percent, 
              fill = trip_type)) +
   geom_col(position = "dodge",
            width = 0.75) +
   scale_y_continuous(labels = percent_format()) +
   labs(title = "Trip Preference", 
        subtitle = "May2021 to April2022",
        x = NULL, 
        y = "% of trips", 
        fill = NULL) +
   geom_text(aes(label = paste0(round(percent * 100), "%")), 
             position = position_dodge(0.7),
             color = "white", 
             vjust = 1.5)
 
 
 #Plot top 10 trips routine for members
 # What are those most popular trip routines?
 #Answer :The most popular trips with casual riders were for leisure.
 all_trips_v2 %>% 
   filter(member_casual == "member") %>%  #filter out member
   count(trip_route) %>% #count the row the casual users made
   arrange(-n) %>%  # sorting order in descending order (The default sorting order of arrange() function is ascending so the resultant dataframe will be sorted in ascending order).
   slice_head(n = 10) %>%  #chopping the head of the data set
   ggplot(aes(x = reorder(trip_route, n), #reorder() function , we need to tell the function our factor variable (“trip_route) and the values we want to reorder it by (the column corresponding to the y-axis, i.e. “n”)
              y = n, 
              fill = trip_route)) +
   geom_col(width = 0.75) +
   coord_flip() +
   guides(fill = "none") +
   scale_y_continuous(labels = comma) +
   labs(title = "Top 10 Trips for Members", 
        subtitle = "January to December 2021",
        x = NULL, 
        y = "Trips") +
   geom_text(aes(label = n), 
             color = "white", 
             hjust = 1.5)
 
 all_trips_v2 %>% 
   filter(member_casual == "member") %>%  #filter out member
   count(trip_route) %>%     #count the row the casual users made
   arrange(-n) %>%         #sorting order in descending order 
   slice_head(n = 10) %>% 
   ggplot(aes(x = reorder(trip_route, n),  #x,y reorder
              y = n, 
              fill = trip_route)) +
   geom_col(width = 0.75) +
   coord_flip() +
   guides(fill = "none") +
   scale_y_continuous(labels = comma) +
   labs(title = "Top 10 Trips for Members", 
        subtitle = "January to December 2021",
        x = NULL, 
        y = "Trips") +
   geom_text(aes(label = n), 
             color = "white", 
             hjust = 1.5)
    
# STEP 5: SUMMARY FOR FURTHER ANALYSIS
#=================================================
Cyclistic <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

write.csv(Cyclistic, "Cyclistic.csv")