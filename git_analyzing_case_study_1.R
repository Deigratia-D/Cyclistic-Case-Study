# Attempting to work on the CSV file for Case_Study_1

library(tidyverse)

library(scales)

getwd()

setwd("")

# I have already organized, clean, and collected the data into one file. Well at least I thought.
df <- read_csv("clean_cyclistic_data.csv", col_names = TRUE)

# Adding new column ride_length_sec
df <- df %>% 
#  select(started_at, ended_at, ride_length) %>% # Used for easy viewing of code.
  mutate(ride_length_sec = as.numeric(ended_at - started_at))

# Adding new column for Year Month Day by itself
df <- df %>% 
#  select(started_at) %>% # Used for sanity check.
  mutate(date = as.Date(started_at))

# Adding new column for the starting hour from started_at ## I'm keeping this for memory but found a better way than making an entire new column.
#df$started_hour <- format(df$started_at, format = "%H")

colnames(df) # Clean column names no rename() used.

str(df) # Reviewing column names and type.

View(df) # Column and Row view of the entire data frame.

head(df) # First 6 rows of the data frame with column names.

tail(df) # Last 6 rows of the data frame with column names.

glimpse(df) # Another view style of the first few rows of data with column names at the start of row.

nrow(df) # 5,779,444 rows in the data frame.

dim(df) # 5,779,444 rows and 19 columns/variables in the data frame.

summary(df)  # Statistical summary of data. I prefer to filter the NA's out when performing analysis. Also, error found in ride_length_sec.

table(df$member_casual) # Shows a count of the different objects within a variable. 2,224,252 Casual and 3,535,192 Members
table(df2$start_station_name)

# Finding bad data within the ride_length column
df %>% 
  select(rideable_type, started_at, ended_at, ride_length, start_station_name, start_station_id, end_station_name, end_station_id, member_casual) %>% 
  filter(ride_length < 0) %>% 
  print(n = 107)

# Drop bad data from the data frame while assigning a new variable with the data.
# This code was making 857,836 errors in the data, I'm only saving it to revisit it later. df2 <- df[!(df$start_station_name == "HQ" | df$ride_length < 0),]
df2 <- df[!(df$ride_length < 0),]

# Changing all docked_bike to classic_bike. From multiple sources docked_bike is classic_bike.
df2$rideable_type[df2$rideable_type == "docked_bike"] <- "classic_bike"

# Counts for all the different values in the column.
df2 %>% 
  count(rideable_type)

# Descriptive analysis on ride_length
summary(df2$ride_length_sec)

# Different style of finding mean, median, max and min.
summarize(df2, ride_length_sec = median(ride_length_sec))

# Compare members and casual users with ride_length_sec as the dependant variable.
aggregate(df2$ride_length_sec ~ df2$member_casual, FUN = mean)
aggregate(df2$ride_length_sec ~ df2$member_casual, FUN = median)
aggregate(df2$ride_length_sec ~ df2$member_casual, FUN = max)
aggregate(df2$ride_length_sec ~ df2$member_casual, FUN = min)

# Testing to see if this works
summary(aggregate(df2$ride_length_sec ~ df2$member_casual, FUN = mean))

# Testing the member vs casual ride length in seconds per the day of the weeks.
aggregate(df2$ride_length_sec ~ df2$member_casual + df2$day_of_week, FUN = mean)

# Making the day of the weeks have levels(order).
df2$day_of_week <- ordered(df2$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Making the months of the year have levels(order).
df2$month_of_year <- ordered(df2$month_of_year, levels = c("July", "August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June"))

# Random testing.
mean(df$start_lat)
## 41.90288
mean(df$start_lng)
## -87.6477
mean(df$end_lat, na.rm = TRUE)
## 41.90308
mean(df$end_lng, na.rm = TRUE)
## -87.64773

# More questions for this data set.

## What is the percents of rides per? Member vs Casual
df %>% group_by(Group) %>% summarise(Percentage=n()/nrow(.)) # This is the code to get a percent.

# Total rides. 5,779,337
df2 %>% 
  summarise(number_of_rides = n())

# All Member vs Casual.
df2 %>% 
  group_by(member_casual) %>% 
  summarise(percentage = (n()/nrow(.)) * 100)

# Ride types.
df2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(percentage = (n()/nrow(.)) * 100)

# Day of week.
df2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(percentage = (n()/nrow(.)) * 100)

# Month of year.
df2 %>% 
  group_by(member_casual, month_of_year) %>% 
  summarise(percentage = (n()/nrow(.)) * 100) %>% 
  print(n = 24)

# Length of rides.

# Lengths of every ride turned into a percent.
df2 %>% 
  group_by(member_casual) %>% 
  summarise(percentage = (sum(ride_length_sec) / sum(df2$ride_length_sec)) * 100)

# Mean of both member and casual ride length then added together = 2407
df2 %>% 
  group_by(member_casual) %>% 
  summarise(percentage = mean(ride_length_sec)) %>% 
  summarise(per = sum(percentage))

# Mean of both member and casual ride length then divided by total 2407.
df2 %>% 
  group_by(member_casual) %>% 
  summarise(percentage = mean(ride_length_sec) / 2407 * 100)

## Who has more rides? Member
df2 %>%
  count(member_casual)

ggplot(df2, aes(x = member_casual, fill = member_casual)) +
  geom_bar(width = 0.75, color = "black") +
  labs(title = "Total Rides",
       subtitle = "Member vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Total Rides",
       x = "Rider Types",) +
  scale_y_continuous(labels = comma,
                     breaks = c(0, 1000000, 2250000, 3500000)) +
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(legend.position = "top")

## Who has longer rides? Casual
  
df2 %>% 
  group_by(member_casual) %>%
  summarise(average_duration = mean(ride_length_sec)) %>%
  ggplot(aes(x = member_casual, y = average_duration / 60, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.75, color = "black") +
  labs(title = "Average Length of Total Rides",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Length of Rides in Minutes",
       x = "Rider Type") +
  scale_y_continuous(labels = comma,
                     breaks = c(0, 5, 10, 15, 20, 25)) +
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(legend.position = "top")

## Week casual vs member ##

# Which day of the week have the most and least rides?
df2 %>% 
  count(day_of_week)  # Saturday is the most used day and Monday is the least.

# Graph for the most popular day.
ggplot(df2, aes(x = day_of_week)) +
  geom_bar()

# Table of average duration and number of rides per day of the week for the year.
df2 %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length_sec)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)  # Puts the data in order by member_casual and day_of_week

# Graph for rides per day of week for the year.
df2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.75, color = "black") +
  labs(title = "Rides Per Day of Week",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Rides",
       x = "Day of Week") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 13)),
        legend.position = "top")

# Graph for average length of the rides per day of the week for the year.
df2 %>% 
  group_by(member_casual, day_of_week) %>%
  summarise(average_duration = mean(ride_length_sec / 60)) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.75, color = "black") +
  labs(title = "Average Duration of Rides Per Day of Week",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Average Duration in Minutes",
       x = "Day of Week") +
#  scale_y_datetime(labels = date_format("%H:%M:%S")) + # Trying to change the seconds to time format.
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 13)),
        legend.position = "top")
                       
# Which type of ride did they use? 
df2 %>% 
  group_by(member_casual, rideable_type) %>% # Groups members and casuals by type of ride.
  summarise(number_of_rides = n(), average_duration = mean(ride_length_sec)) %>% # Calculate the number of rides and average duration.
  arrange(member_casual, rideable_type)

# Graph for number of rides for rideable_type.
df2 %>% 
  group_by(member_casual, rideable_type) %>%
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.75, color = "black") +
  labs(title = "Total Rides Per Bike Type",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Rides",
       x = "Bike Type") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(legend.position = "top")
  
# Graph for duration of rideable_types.
df2 %>% 
  group_by(member_casual, rideable_type) %>%
  summarise(average_length = mean(ride_length_sec / 60)) %>% 
  ggplot(aes(x = rideable_type, y = average_length, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.75, color = "black") +
  labs(title = "Average Duration of Rides Per Bike Type",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Duration of Rides in Minutes",
       x = "Bike Type") +
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(legend.position = "top")

########
# Does distance have any bearing on the analysis? No, because the data is not accurate for analysis.
# Unfortunately the bikes did not have gps distance data for the ride,
# the data for distance is from one starting point to an ending point and that is unreliable because
# a rider can start and end at the same location causing the data to apply a distance of 0.00000.
########

# Is there a favorite drop of or pick up station between the two?
df2 %>%
  group_by(start_station_id, member_casual) %>%  
  count(start_station_id) %>% 
  arrange(desc(n)) %>% 
  print(n = 100)
########
# Casual riders used the 13022 start_station_id the most and members use KA1503000043
########

## Within the month casual vs member ##

# Is there a pattern of rides and length of rides within the month?
df2 %>% 
  group_by(member_casual, date, day_of_week) %>% # The day_of_week was added to see the day of the date.
  filter(between(date, as.Date('2022-08-01'), as.Date('2022-08-31'))) %>% # Used to select month to view.
  summarise(number_of_rides = n(),
            ride_length_mean = mean(ride_length_sec)) %>% # Change between these two summaries to see, number of rides or length of rides.
  arrange(date)
  
#######
# It seems that the pattern is Casual riders take longer ride lengths and are most active on the weekends
# and member rides are typically shorter in length and are mostly used on the weekdays.
#######

# Graph for Number of rides per month.
df2 %>% 
  group_by(member_casual, month_of_year) %>%
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = month_of_year, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.75, color = "black") +
  labs(title = "Number of Rides Per Month",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Rides",
       x = "Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 13)),
        legend.position = "top")

# Graph for duration of rides per month.
df2 %>% 
  group_by(member_casual, month_of_year) %>% 
  summarise(ride_length_mean = mean(ride_length_sec / 60)) %>% 
  ggplot(aes(x = month_of_year, y = ride_length_mean, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.75, color = "black") +
  labs(title = "Average Duration of Rides Per Month",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Duration of Rides in Minutes",
       x = "Month") +
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 13)),
        legend.position = "top")

# Do different parts of the months have different ride lengths or ride amounts? It doesn't seem like per month there is a certain change.

# Graph for number of rides per dates of month.
df2 %>% 
  group_by(member_casual, date, month_of_year) %>%
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = date, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge", width = 0.75, color = "black") +
  labs(title = "Number of Rides Per Day of Month",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Rides",
       x = "Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 45, margin = margin(t = 13)),
        legend.position = "top") +
  facet_wrap(~month_of_year) # Also could use member_casual here to have a okay view.

# Is there a pattern for ride type? No pattern. *See next question for a more complete question.
df2 %>% 
  group_by(member_casual, date, rideable_type) %>%
  filter(between(date, as.Date('2022-07-01'), as.Date('2022-07-31'))) %>% # Filter for time period.
  filter(member_casual == "member") %>% # Use to filter out member and/or casual and/or which ride type.
  summarise(number_of_rides = n()) %>%
  arrange(date) %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, color = rideable_type, shape = rideable_type)) + # Change color and shape to help identify member_casual and/or rideable_type
  geom_point()
######
# Casual riders prefer the electric_bike over the classic unless rides are longer in duration.
# Member riders prefer the electric over the classic but there isn't a huge difference.
# These vary by month.
######

# When do electric bikes become the least used based on duration? Anything past eight hours and two minutes is going to be a classic bike.
df2 %>% 
  group_by(member_casual, ride_length_sec, rideable_type) %>% 
  filter(rideable_type == "electric_bike") %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = ride_length_sec / 60, fill = member_casual)) +
  geom_histogram(position = "dodge") +
  labs(title = "Number of Electric Bike Rides Per Minute",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Rides",
       x = "Duration in Minutes") +
  scale_x_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 0, margin = margin(t = 5)),
        legend.position = "top")

# Classic bike graph
df2 %>% 
  group_by(member_casual, ride_length_sec, rideable_type) %>% 
  filter(rideable_type == "classic_bike") %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = ride_length_sec / 60, fill = member_casual)) +
  geom_histogram(position = "dodge", binwidth = 1000) +
  labs(title = "Number of Classic Bike Rides Per Minute",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Rides",
       x = "Duration in Minutes") +
  scale_x_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 0, margin = margin(t = 5)),
        legend.position = "top")

# Graph of classic bike usage per minutes and filtered for below 1,500 mins.
df2 %>% 
  group_by(member_casual, ride_length_sec, rideable_type) %>% 
  filter(rideable_type == "classic_bike" & ride_length_sec < 90000) %>% 
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = ride_length_sec / 60, fill = member_casual)) +
  geom_histogram(position = "dodge", bins = 50) +
  labs(title = "Number of Classic Bike Rides Per Minute Under 1,500",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Rides",
       x = "Duration in Minutes") +
  scale_x_continuous(labels = comma,
                     breaks = c(0, 125, 250, 375, 500, 625, 750, 1000, 1500)) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 0, margin = margin(t = 5)),
        legend.position = "top")

# More test on when rideable types are used.
df2 %>% 
  filter(ride_length_sec > 10000 & member_casual == "casual" & rideable_type == "classic_bike") %>% 
  arrange(desc(ride_length_sec))

# Does distance mean anything? Not on this analysis.

## Year casual vs member ##

# Which months have the most rides?
df2 %>% 
  group_by(member_casual, month_of_year) %>%
#  filter(month_of_year == "July") %>% # Use if you want to see specific months.
  summarise(number_of_rides = n()) %>%
  arrange(month_of_year) %>% 
  ggplot(aes(x = month_of_year, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
######
# Casual and member riders rode the most in summer then in spring/fall and least in winter.
######

# Which months have the longest and shortest rides?
df2 %>% 
  group_by(member_casual, month_of_year) %>%
  #  filter(month_of_year == "July") %>% # Use if you want to see specific months.
  summarise(ride_length_mean = mean(ride_length_sec)) %>% 
  arrange(month_of_year) %>% 
  ggplot(aes(x = month_of_year, y = ride_length_mean, fill = member_casual)) +
  geom_col(position = "dodge")
######
# The lengths of both riders were higher in summer then spring/fall and least in winter.
######

# Is there a pattern for the year? Yes, Summer has the highest length and number of rides and
# it falls towards winter and rises towards summer.

# How are the rideable types used throughout the year?
df2 %>% 
  group_by(member_casual, month_of_year, rideable_type) %>%
#  filter(month_of_year == "July") %>% # Use if you want to see specific months.
#  filter(member_casual == "member") %>% # Use to filter out member and/or casual and/or which ride type.
  summarise(number_of_rides = n(),
            ride_length_mean = mean(ride_length_sec)) %>% 
  arrange(month_of_year) %>% 
  ggplot(aes(x = month_of_year, y = ride_length_mean, fill = member_casual)) +
  geom_col(position = "dodge") +
  facet_wrap(~rideable_type)
######
# Members use electric rides more often in sring/fall and is a mixture in summer and winter.
# Casual riders use electric bikes more year round then classic and docked.
######

# What about distance? Not gonna happen.

## Average start times Members vs Casuals
df2 %>% 
  group_by(member_casual, format(started_at, format = "%H")) %>% 
  summarise(numbe_of_start = n()) %>%
  arrange(member_casual)

# Per day
df2 %>% 
  group_by(member_casual, day_of_week, started_at) %>% 
  ggplot(aes(x = format(started_at, format = "%H"), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Starts Per Hour Per Day",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Starts",
       x = "Starting Hour") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 90, margin = margin(t = 0)),
        legend.position = "top") +
  facet_wrap(~day_of_week)

# Per Month
df2 %>% 
  group_by(member_casual, day_of_week, started_at) %>% 
  ggplot(aes(x = format(started_at, format = "%H"), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Starts Per Hour Per Month",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Starts",
       x = "Starting Hour") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 90, margin = margin(t = 0)),
        legend.position = "top") +
  facet_wrap(~month_of_year)

# Per Year
df2 %>% 
  group_by(member_casual, day_of_week, started_at) %>% 
  ggplot(aes(x = format(started_at, format = "%H"), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Total Number of Starts Per Hour",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Starts",
       x = "Starting Hour") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 90, margin = margin(t = 0)),
        legend.position = "top")

# Per Bike Type
df2 %>% 
  group_by(member_casual, day_of_week, started_at) %>% 
  ggplot(aes(x = format(started_at, format = "%H"), fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Starts Per Hour Per Ride Type for Year.",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Number of Starts",
       x = "Starting Hour") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 90, margin = margin(t = 0)),
        legend.position = "top") +
  facet_wrap(~rideable_type)

# Average distance per start hour. None of this worked the way I wanted and I don't think it's worth the time to get correct.

# Per day.
df2 %>% 
  group_by(member_casual, day_of_week, started_at) %>% 
  ggplot(aes(x = format(started_at, format = "%H"), y = mean(ride_length_sec) / 60, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Distance Per Hour Per Day",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Average Distance in Minutes",
       x = "Starting Hour") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 0, margin = margin(t = 5)),
        legend.position = "top") +
  facet_wrap(~day_of_week)

# Per Month.
df2 %>% 
  group_by(member_casual, day_of_week, started_at) %>% 
  ggplot(aes(x = format(started_at, format = "%H"), y = mean(ride_length_sec) / 60, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Distance Per Hour Per Month",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Average Distance in Minutes",
       x = "Starting Hour") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 0, margin = margin(t = 5)),
        legend.position = "top") +
  facet_wrap(~month_of_year)

# Per Year.
df2 %>% 
  group_by(member_casual, day_of_week, started_at) %>% 
  ggplot(aes(x = format(started_at, format = "%H"), y = mean(ride_length_sec) / 60, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Distance Per Hour for Year",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Average Distance in Minutes",
       x = "Starting Hour") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 0, margin = margin(t = 5)),
        legend.position = "top")

# Per rideable_type.
df2 %>% 
  group_by(member_casual, day_of_week, started_at) %>% 
  ggplot(aes(x = format(started_at, format = "%H"), y = mean(ride_length_sec) / 60, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Distance Per Hour Per Ride Type for Year",
       subtitle = "Members vs Casual",
       fill = "",
       caption = "Data From: 2022/07/01 - 2023/06/30",
       y = "Average Distance in Minutes",
       x = "Starting Hour") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("#7CA1CC", "#1F449C"),
                    breaks = c("casual", "member")) +
  theme(axis.text.x = element_text(angle = 0, margin = margin(t = 5)),
        legend.position = "top") +
  facet_wrap(~rideable_type)
