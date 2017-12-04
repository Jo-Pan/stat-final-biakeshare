library(car) #step
library(MASS) #box-cox
library(DAAG) #cv
library(tidyverse)

###########################################
## LINEAR MODELING: PREDICTING BIKE COUNT
###########################################

# Data Preprocessing ----------------------------------------------------
day<-read.csv("by_day.csv")
hour<-read.csv("by_hour.csv")

# Treat categorical variables as factors
day$Is.Weekend<-as.factor(day$Is.Weekend)
day$Month<-as.factor(day$Month)
day$Weekday<-as.factor(day$Weekday)
day$Rain <- as.factor(day$Rain)
day$Fog <- as.factor(day$Fog)
day$Snow <- as.factor(day$Snow)
day$Thunderstorm <- as.factor(day$Thunderstorm)

# Subset variables we're interested in
bikedata <- day[, c(2, 8:33)]

# Investigate NAs
colSums(is.na(bikedata))
# There are no NAs.

# Linear Model (full) ----------------------------------------------------

lm.full<-lm(Bike.Count~.,data=bikedata)

summary(lm.full) 

# Residual standard error: 1206 on 59 degrees of freedom
# Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8092 
# F-statistic: 13.59 on 30 and 59 DF,  p-value: < 2.2e-16

# Model is significant.

# The model identified Day, Wind_Avg, Rain_Inches, Snow, Weekday(Sunday) and Is.Holiday
# as significant variables at the 5% significance level.
# Is.Weekend and Vis_High were removed from the model because of singularities.

# We do not know if the beta estimates above are being affected by multicollinearity.
# We need to investigate multicollinearity in the data and consider transforming
# and removing variables.

# Investigate Multicollinearity --------------------------------------------------------------------------

# Look at correlation matrix between numeric variables
is.num <- sapply(bikedata, is.numeric)
num.df <- bikedata[, is.num]
cor(num.df)

# There are severe near-linear dependencies (corr > 0.9) between Temp_High and Temp_Avg,
# Temp_Low and Temp_Avg, Dew_High and Dew_Avg, Dew_Low and Dew_Avg, Hum_High and Hum_Avg,
# Pres_High and Pres_Avg, and Pres_Low and Pres_Avg.

# Look at Variance Inflation Factors
lm.full<-lm(Bike.Count~.-Is.Weekend -Vis_High,data=bikedata)
vif(lm.full)
# Again, many of weather variables have VIF > 5, so again, we need to deal with them.

# Dealing With Multicollinearity --------------------------------------------------------------------------

# Collapse High/Low Variables into Range Variables.
# This way, we keep the information contained in these variables and hopefully avoid multicollinearity
# with the Avg.

bikedata$Temp_Range <- bikedata$Temp_High-bikedata$Temp_Low
bikedata$Dew_Range <- bikedata$Dew_High-bikedata$Dew_Low
bikedata$Hum_Range <- bikedata$Hum_High-bikedata$Hum_Low
bikedata$Pres_Range <- bikedata$Pres_High-bikedata$Pres_Low
bikedata$Vis_Range <- bikedata$Vis_High-bikedata$Vis_Low

# Drop and High and Low variables. We will also drop Is.Weekend because it is perfectly linearly
# depend with Weekday==Saturday and Weekday==Sunday.

bikedata <- bikedata %>% 
  select(-Is.Weekend, -Temp_High, -Temp_Low, -Dew_High, -Dew_Low, -Hum_High, -Hum_Low,
         -Pres_High, -Pres_Low, -Vis_High, -Vis_Low)

# Look at correlation matrix between numeric variables
is.num <- sapply(bikedata, is.numeric)
num.df <- bikedata[, is.num]
cor(num.df)

# No more correlations above > 0.9

lm.full2 <- lm(Bike.Count~. ,data=bikedata)
summary(lm.full2)

# Residual standard error: 1174 on 63 degrees of freedom
# Multiple R-squared:  0.872,	Adjusted R-squared:  0.8192 
# F-statistic: 16.51 on 26 and 63 DF,  p-value: < 2.2e-16

# Model is significant.
# Our Adj-R^2 improved from 0.8092 to 0.8192.
# Variables identified by the model as significant at the 5% significance level:
# Month(Jan), Day, Temp_Avg, Wind_Avg, Rain_Inches, Snow, Weekday(Sunday), Is.Holiday.

vif(lm.full2)
# VIF values are much lower now. However, Temp_Avg, Dew_Avg, Hum_Avg, Vis_Avg have VIF > 10.
# Vis_Range has VIF > 5 and < 10.
# According to the correlation matrix, Hum_Avg and Temp_Avg are both correlated with Dew_Avg
# by 0.85317738 and 0.80249561 respectively.

# What happens if we drop Dew_Avg and Vis_Range?
lm.full3 <- lm(Bike.Count~. -Dew_Avg -Vis_Range,data=bikedata)
summary(lm.full3)

# Snow is no longer significant.

# Residual standard error: 1179 on 65 degrees of freedom
# Multiple R-squared:  0.8669,	Adjusted R-squared:  0.8178 
# F-statistic: 17.65 on 24 and 65 DF,  p-value: < 2.2e-16

# Model is significant.
vif(lm.full3)
# Vis_Avg and Hum_Avg have GVIF ~ 5 or 6, but overall, looks much better.

# Drop Dew_Avg and Vis_Range from dataframe.

bikedata <- bikedata %>% 
  select(-Dew_Avg, -Vis_Range)

lm.full4 <- lm(Bike.Count~.,data=bikedata)

# Step function -----------------------------------------------------------------

## Begin by defining the models with no variables (null) and all variables (full)
lm.null <- lm(Bike.Count~1,data=bikedata)

## step selection
lm.step<-step(lm.null, scope=list(lower=lm.null, upper=lm.full4), direction="both")

# Step:  AIC=1281.19
# Bike.Count ~ Temp_Avg + Hum_Avg + Rain_Inches + Weekday + Is.Holiday + 
#   Wind_Avg + Month + Day + Snow + Rain

summary(lm.step)
# Residual standard error: 1134 on 73 degrees of freedom
# Multiple R-squared:  0.8617,	Adjusted R-squared:  0.8314 
# F-statistic: 28.43 on 16 and 73 DF,  p-value: < 2.2e-16

# Model is significant.

# Residual Analysis/Influential Points ----------------------------------------------

## R-student residuals
ti<-rstudent(lm.step)

## Normal probabilty plot
qqnorm(ti)
qqline(ti)
# Looks normal except for two points on the right tail.

## Residual vs. fitted values plot
plot(fitted(lm.step),ti)
# Seems normal with the exception of 2 points.

summary(influence.measures(lm.step))
# Pt 84 is an influential point, with a COVRATIO of 0.07, a DFFIT of 1.94 and a DFBETA of 1.03 for Temp_Avg.
# This suggests that point 84 has unusually high influence over the beta estimate of Temp_Avg,
# and that the fitted values are being affected by the presence of point 84.

# Pt 50 has a DFFIT of 1.56 and a COVRATIO of 0.1, suggesting that this point also exerts an unusual amount of
# influence on the fitted values.

bikedata[c(50, 84),]
# These points correspond to Feb 19, 2017 and Mar 25, 2017.
# Bike.Count Month Day Temp_Avg Hum_Avg Pres_Avg Vis_Avg Wind_Avg Rain_Inches Rain Fog Snow Thunderstorm  Weekday Is.Holiday
# 50      12350   Feb  19       60      60    29.91      10        9        0.01    1   0    0            0   Sunday          0
# 84      16191 March  25       66      58    30.17      10        9        0.00    0   0    0            0 Saturday          0

summary(bikedata$Bike.Count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1583    5382    7237    7183    8608   16190

# In terms of bike count, both these days are in the topmost quartile and the bike count for Pt 84 corresponds
# to the max bike count.

# Does removing these two points change the model's beta coefficients?
lm.full5 <- lm(Bike.Count ~ Temp_Avg + Hum_Avg + Rain_Inches + 
                 Weekday + Is.Holiday + Wind_Avg + Month + Day + Snow + Rain, 
               data = bikedata[-c(50, 84),])
summary(lm.full5)

# Residual standard error: 946.7 on 71 degrees of freedom
# Multiple R-squared:  0.8881,	Adjusted R-squared:  0.8629 
# F-statistic: 35.22 on 16 and 71 DF,  p-value: < 2.2e-16

# An improvement in Adjusted R^2 compared to lm.step (up from 0.8314).

lm.full5$coefficients
# (Intercept)         Temp_Avg          Hum_Avg      Rain_Inches    WeekdayMonday  WeekdaySaturday    WeekdaySunday 
# 1540.024182       174.092476       -13.834248     -4194.415747       146.885718      -801.287525     -2075.045727 
# WeekdayThursday   WeekdayTuesday WeekdayWednesday       Is.Holiday         Wind_Avg         MonthJan       MonthMarch 
# 12.829004        -7.907761       -92.695427     -1789.780420       -92.307566      -850.845000       324.486204 
# Day            Snow1            Rain1 
# 30.187008       491.145747      -547.949283 

lm.step$coefficients
# (Intercept)         Temp_Avg          Hum_Avg      Rain_Inches    WeekdayMonday  WeekdaySaturday    WeekdaySunday 
# 742.37329        199.83904        -20.72061      -4095.33739        193.89938       -387.05692      -1709.29852 
# WeekdayThursday   WeekdayTuesday WeekdayWednesday       Is.Holiday         Wind_Avg         MonthJan       MonthMarch 
# -86.16931        -40.64184       -198.33614      -1733.17718        -93.83794       -813.51719        321.05561 
# Day            Snow1            Rain1 
# 31.76259        679.38475       -570.10086 

# Yes, there are large differences in these coefficient estimates.

# However, we cannot justify removing these points in our model. An internet search reveals that
# nothing special happened on Feb 19, 2017 or Mar 25, 2017 in Washington D.C. We have no evidence that
# these two observations are bad data points. It's possible that they seem influential in this model
# because we do not have enough data points (n = 90).

# Plotting residuals against explanatory variables.

plot(bikedata$Temp_Avg, ti) # Looks ok except the two points in the upper right hand corner.
plot(bikedata$Hum_Avg, ti) # Looks fine except for two points
plot(bikedata$Rain_Inches, ti) # Highly abnormal residuals - !!!!!
plot(bikedata$Wind_Avg, ti) # Some irregularities, but mostly ok

# Looking At Rain_Inches ----------------------------------------------- 

plot(bikedata$Rain_Inches, bikedata$Bike.Count)
# There are many days where Rain_Inches == 0, leading to the strange residual plot found above.

# There seems to be no good way to transform this variable. Furthermore, it is
# a significant variable, so we will leave it in as is.

# Cross-Validate -----------------------------------------------
x.cv <- cv.lm(data=bikedata, form.lm=lm.step, m=2, plotit=T)

# Sum of squares = 1.21e+08    Mean square = 2698332    n = 45 
# 
# Overall (Sum over all 45 folds) 
# ms 
# 2352489 

# Plot shows our final model is pretty good.

#######################################################
## LINEAR MODELING: PREDICTING TOTAL TRAVEL DISTANCE
#######################################################

bikedata2 <- bikedata %>% select(-Bike.Count)
bikedata2$Total.Dist <- day$Total.Dist

# Linear Model (full) ----------------------------------------------------

lm.full<-lm(Total.Dist~.,data=bikedata2)
summary(lm.full) 

# Residual standard error: 2590000 on 64 degrees of freedom
# Multiple R-squared:  0.866,	Adjusted R-squared:  0.813 
# F-statistic: 16.5 on 25 and 64 DF,  p-value: <2e-16

# Model is significant.
# Month, Day, Temp_Avg, Wind_Avg, Rain_Inches, Snow, Weekday, Is.Holiday 
# appear to be statistically significant predictors.

# Step function -----------------------------------------------------------------
## Begin by defining the models with no variables (null) and all variables (full)
lm.null <- lm(Total.Dist~1,data=bikedata2)

#### step selection
lm.step<-step(lm.null, scope=list(lower=lm.null, upper=lm.full), direction="both")
# Step:  AIC=2664
# Total.Dist ~ Temp_Avg + Rain_Inches + Weekday + Is.Holiday + 
#   Wind_Avg + Day + Month + Snow + Rain

summary(lm.step)
# Residual standard error: 2470000 on 74 degrees of freedom
# Multiple R-squared:  0.859,	Adjusted R-squared:  0.83 
# F-statistic:   30 on 15 and 74 DF,  p-value: <2e-16

vif(lm.step)
# All VIF < 2.

# Residual Analysis ----------------------------------------------

## Find the R-student residuals
ti<-rstudent(lm.step)

## Normal probabilty plot
qqnorm(ti)
qqline(ti)
# Looks good apart from 2 points in the right tail.

## Residual plot vs. fitted values
plot(fitted(lm.step), ti)
# Mostly fine except for 2 outliers.

## Plot against individual explanatory variable
plot(bikedata2$Temp_Avg, ti)
plot(bikedata2$Rain_Inches, ti)
plot(bikedata2$Wind_Avg, ti)

# The plots are all similar to the ones in the Bike.Count model.

# Cross-Validate -----------------------------------------------
s.cv <- cv.lm(data=bikedata2, form.lm=lm.step, m=2, plotit=T) 

# Sum of squares = 5.72e+14    Mean square = 1.27e+13    n = 45 
# 
# Overall (Sum over all 45 folds) 
# ms 
# 1.1e+13