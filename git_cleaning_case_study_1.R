## Cleaning Case Study 1

library(tidyverse)

df <- read_csv("divvy_tripdata_202207_through_202306.csv", col_names = TRUE)

head(df)

str(df)

mean(df$start_lat)
## 41.90288

mean(df$start_lng)
## -87.6477

mean(df$end_lat, na.rm = TRUE)
## 41.90308

mean(df$end_lng, na.rm = TRUE)
## -87.64773

summary(df)

end_lat <- df$end_lat

end_lng <- df$end_lng

boxplot(end_lat, end_lng)

## Creating a new variable with altered data from the original data set by taking the NA out of specific columns.
New_df = df

New_df$end_lat = ifelse(is.na(New_df$end_lat),
                      median(New_df$end_lat,
                             na.rm = TRUE),
                      New_df$end_lat)

New_df$end_lng = ifelse(is.na(New_df$end_lng),
                      median(New_df$end_lng,
                             na.rm = TRUE),
                      New_df$end_lng)

summary(New_df)

new_end_lat <- New_df$end_lat

new_end_lng <- New_df$end_lng

boxplot(new_end_lat, new_end_lng)

## Creating a new variable with the original data with new columns ride_length, day_of_week, and distance.

## ride_length testing. Used seconds_to_period() which is a lubridate function.
df <- df %>% 
##  select(started_at, ended_at) %>% I used this line for easy viewing when testing the code.
  mutate(ride_length = seconds_to_period(ended_at - started_at))

## day_of_week testing. Used the {base} function weekdays() to achieve finding what day of the week the start of the ride was.
df <- df %>% 
##  select(started_at) %>%  Used this for easier viewing when testing the code.
  mutate(day_of_week = weekdays(started_at))

## month_of_year testing.
df <- df %>% 
##  select(started_at) %>% Used this for easier viewing when testing the code.
  mutate(month_of_year = months(started_at)) 

## Got the new column to be positive numbers no matter what, now need to figure out how to judge distance with longitude and latitude coordinates.
## This will not be used for the analysis because the below code is the correct way to judge distance with Lat and Lng values.
df_distance <- df %>% 
  select(start_lat, start_lng, end_lat, end_lng) %>% 
  mutate(distance = abs((start_lat - end_lat) + (start_lng - end_lng)))

## Distance, d = 3963.0 * arccos[(sin(lat1) * sin(lat2)) + cos(lat1) * cos(lat2) * cos(long2 – long1)]
## Find the value of the latitude in radians:
## Value of Latitude or Longitude in Radians, lat = Latitude / (180/pi) OR
## Value of Latitude or Longitude in Radians, lat = Latitude / 57.29577951
## Lat and Lng need to be turned into a radians before putting into the Haversine formula.
## You can multiple the miles number by 1.609344 to get the distance in Kilometers or start with 6378 in the formula. Also, multiply kilometers by 0.621371 to get miles.
df <- df %>% 
  mutate(distance_miles = 3963 * acos((sin(start_lat/57.29577951) * sin(end_lat/57.29577951)) + cos(start_lat/57.29577951) * 
                                  cos(end_lat/57.29577951) * cos(end_lng/57.29577951 - start_lng/57.29577951)))

## Move columns for better orginization.
df <- df %>% 
  relocate(distance_miles, .after = ended_at) %>% 
  relocate(day_of_week, .after = started_at) %>% 
  relocate(ride_length, .after = ended_at) %>% 
  relocate(month_of_year, .after = day_of_week)

## Write the file to a new .csv file and or a .db file
write_csv(df, "clean_cyclistic_data.csv", col_names = TRUE,)

## Attempt to put all the new and old columns into a new variable. I don't need to do this because adjusting the...
## original variable won't save over the file that was read in. 
df_v2 <- df
  
## This is the formula for finding the distance between two points on a 2d plane.
sqrt(((7 - 2)^2) + ((17 - 5)^2))