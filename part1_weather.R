library(tidyverse)
library(reshape)

data <- read_csv("combined_bicycle_weather_data.csv")

# What is the relationship between the number of bike users in a given day and weather conditions such as temperature,
# wind speed and humidity?
# - Group by day.
# - Explore relationship between (1) number of bikes in a day and (2) total distance travelled and (3)
# usage duration by all users and other variables.
# - Explore normalizing weather variables - is it more predictive?

# Group by day and add summary columns (count registered and casual users, total distance, total duration)
by_day <- data %>% 
  group_by(Date) %>% 
  summarise(Bike.Count = n(),
            Registered.Users = sum(Member.Type=="Registered"),
            Casual.Users = sum(Member.Type=="Casual"),
            Total.Dist = sum(traveldist),
            Total.Duration = sum(as.numeric(Duration))
            )

# Did registered and casual users tally up correctly?
sum((by_day$Registered.Users+by_day$Casual.Users)==by_day$Bike.Count)
# Fine

# Re-combine with weather
weather <- read_csv("2017_Q1_weather.csv")
byday_data <- merge(x = by_day, y = weather, by = "Date", all.x=TRUE)

# Run this line twice:
byday_data$Condition <- sub(",", " ", byday_data$Condition, fixed=TRUE)

# Make conditions indicators (0 or 1)
conditions <- byday_data %>% 
  select(Date, Condition)

conditions$Rain <- 0
conditions$Fog <- 0
conditions$Snow <- 0
conditions$Thunderstorm <- 0

# Code Rain, Fog, Snow, Thunderstorm as 0 or 1 as needed
fill_conditions <- function(df) {
  df[grepl("Rain", df[,"Condition"]),"Rain"] <- 1
  df[grepl("Fog", df[,"Condition"]),"Fog"] <- 1
  df[grepl("Snow", df[,"Condition"]),"Snow"] <- 1
  df[grepl("Thunderstorm", df[,"Condition"]),"Thunderstorm"] <- 1
  return(df)
}

conditions <- fill_conditions(conditions)

# Final dataframe
byday_final <- byday_data %>% 
  mutate(Rain_Inches = Rain) %>% 
  select(-Rain, -Condition) %>% 
  merge(conditions, by="Date") %>% 
  select(-Condition)

# Write to CSV for safekeeping
write_csv(byday_final, "byday_final.csv")

