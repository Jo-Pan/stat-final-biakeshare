library(tidyverse)
library(readr)

bicycle= read_csv("2017Q1_w_dist.csv")
head(bicycle)
glimpse(bicycle)
dim(bicycle)


weather= read_csv("2017_Q1_weather.csv")
head(weather)
glimpse(weather)
dim(weather)

install.packages("reshape")
require(reshape)
bicycle$End.date.temp<-bicycle$End.date
bicycle = transform(bicycle, End.date = colsplit(End.date, split = " ", names = c('Date', 'Time')))

glimpse(bicycle)
head(bicycle)

bicycle$Date<-bicycle$End.date$Date
bicycle$End.date<-NULL
bicycle$End.date<-bicycle$End.date.temp
bicycle$Date<-as.character(bicycle$Date)

typeof(bicycle$Date)

combined<-merge(x = bicycle, y = weather, by = "Date", all.x=TRUE)

head(combined)

sum(is.na(combined))

combined$Condition<-sub(",", " ", combined$Condition, fixed=TRUE)
unique(combined$Condition)

#Writing clean data

write.csv(combined, file = "combined_bicycle_weather_data.csv",row.names=FALSE)

