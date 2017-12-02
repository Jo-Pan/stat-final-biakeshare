library(tidyverse)
library(reshape)

bicycles <- read_csv("2017Q1_w_dist.csv")
weather <- read_csv("2017_Q1_weather.csv")

# Split up start date, start time, end date, end time
bicycles <- bicycles %>% 
  transform(Start.date = colsplit(Start.date, split = " ", names = c('Date', 'Start.Time'))) %>% 
  transform(End.date = colsplit(End.date, split = " ", names = c('End.Date', 'End.Time')))

bicycles$Date = bicycles$Start.date$Date %>% as.character()
bicycles$Start.Time <- bicycles$Start.date$Start.Time %>% as.character()
bicycles$End.Time <- bicycles$End.date$End.Time %>% as.character()

bicycles <- bicycles %>% 
  select(-End.date, -Start.date)

# Add hour of the day variable
bicycles$Start.Hour <- gsub(":.+", "", bicycles$Start.Time)

# Convert date to date type
bicycles$Date <- as.Date(bicycles$Date, format = "%m/%d/%Y")

# Group by day and add summary columns (count registered and casual users, total distance, total duration)
by_day <- bicycles %>% 
  group_by(Date) %>% 
  summarise(Bike.Count = n(),
            Registered.Users = sum(Member.Type=="Registered"),
            Casual.Users = sum(Member.Type=="Casual"),
            Total.Dist = sum(traveldist),
            Total.Duration = sum(as.numeric(Duration))
  )

# Combine with weather
weather$Date <- as.Date(weather$Date, format = "%m/%d/%Y")
by_day <- merge(x = by_day, y = weather, by = "Date", all.x=TRUE)

# Remove commas in condition
by_day$Condition <- gsub(",", " ", by_day$Condition, fixed=TRUE)

# Make conditions indicators (0 or 1)
conditions <- by_day %>% 
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

# Change Rain to Rain_Inches and merge by_day with Conditions
by_day <- by_day %>% 
  mutate(Rain_Inches = Rain) %>% 
  select(-Rain, -Condition) %>% 
  merge(conditions, by="Date") %>% 
  select(-Condition)

# Add day of week and Is.Weekend variables
by_day$Weekday <- weekdays(by_day$Date)
by_day$Is.Weekend <- by_day$Weekday=="Sunday"||by_day$Weekday=="Saturday"

# In Rain_Inches, T stands for "Trace", which means that some moisture was detected, but not enough to be
# measurable. We will impute T as 0.

by_day$Rain_Inches[by_day$Rain_Inches=="T"] <- 0
by_day$Rain_Inches <- as.numeric(by_day$Rain_Inches)

# Upon inspecting Wind_High, Wind_Avg and Wind_Low, we find that these variables are confusing
# because Wind_High is not always the largest value and Wind_Low is not always the smallest value,
# so we'll just drop Wind_High and Wind_Low.

by_day %>% select(Wind_High, Wind_Avg, Wind_Low) %>% head(8)

# Wind_High Wind_Avg Wind_Low
# 13        5        -
# 12        7        -
# 10        7        -
# 16        8       30
# 12        7        -
# 16       12       22
# 23       15       30
# 29       14       38

by_day <- by_day %>% 
  select(-Wind_High, -Wind_Low)

# Washington D.C. federal holidays Jan through March 2017:
# Monday, January 2, 2017  New Year's Day**
# Monday, January 16, 2017  Martin Luther King Jr. Day
# Friday, January 20, 2017 Inauguration Day***
# Monday, February 20, 2017 Washington's Birthday
# Source: https://dchr.dc.gov/page/holiday-schedules

# Add Is.Holiday variable
by_day$Is.Holiday <- 0
dc.holidays <- c("1/2/2017", "1/16/2017", "1/20/2017", "2/20/2017") %>% as.Date(format = "%m/%d/%Y")
by_day$Is.Holiday[by_day$Date %in% dc.holidays] <- 1

# Write to csv
write_csv(by_day, "by_day.csv")
write_csv(bicycles, "by_hour.csv")
