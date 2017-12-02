#setwd("/Users/Pan/Google Drive/Data Science/STAT 6021/stat-final-bikeshare")
library(car) #step
library(MASS) #box-cox
library(DAAG) #cv

# Data Preprocess for models ----------------------------------------------------
day<-read.csv("by_day.csv")
hour<-read.csv("by_hour.csv")

day$Is.Weekend<-as.numeric(grepl("S(at|un)", day$Weekday))
day$Month<-as.factor(day$Month)
day$Weekday<-as.factor(day$Weekday)

outcomeName<-"Bike.Count" 
predictorName<-names(day)[8:33]
data=data.frame(day[,predictorName],day[,outcomeName])
names(data)[27]<-outcomeName

colSums(is.na(data))



# Linear Model (full) ----------------------------------------------------
lm.full<-lm(Bike.Count~.,data=data)
summary(lm.full) 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1921.7  -638.7  -233.7   586.8  3268.1 
# 
# Coefficients: (2 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)      -14975.941  22626.341  -0.662  0.51062   
# MonthJan           -687.628    414.665  -1.658  0.10257   
# MonthMarch          243.167    423.296   0.574  0.56784   
# Day                  33.981     18.783   1.809  0.07553 . 
# Temp_High            49.199    271.897   0.181  0.85703   
# Temp_Avg            159.389    550.114   0.290  0.77303   
# Temp_Low              8.991    279.063   0.032  0.97441   
# Dew_High             12.634     61.294   0.206  0.83740   
# Dew_Avg             -84.069     90.286  -0.931  0.35557   
# Dew_Low              47.188     54.868   0.860  0.39326   
# Hum_High            -31.431     40.213  -0.782  0.43757   
# Hum_Avg              56.827     71.452   0.795  0.42961   
# Hum_Low             -22.751     32.650  -0.697  0.48866   
# Pres_High           579.132   3756.365   0.154  0.87800   
# Pres_Avg          -1339.090   6818.076  -0.196  0.84497   
# Pres_Low           1175.121   3943.695   0.298  0.76677   
# Vis_High                 NA         NA      NA       NA   
# Vis_Avg              88.952    283.269   0.314  0.75461   
# Vis_Low              97.284    102.611   0.948  0.34696   
# Wind_Avg            -86.376     40.112  -2.153  0.03539 * 
#   Rain_Inches       -3252.357   1193.619  -2.725  0.00845 **
#   Rain               -432.363    468.566  -0.923  0.35990   
# Fog                 607.560    762.410   0.797  0.42871   
# Snow               1201.443    612.428   1.962  0.05451 . 
# Thunderstorm        540.906   1365.051   0.396  0.69335   
# WeekdayMonday       147.423    552.271   0.267  0.79045   
# WeekdaySaturday    -268.785    551.962  -0.487  0.62809   
# WeekdaySunday     -1748.884    519.112  -3.369  0.00133 **
# WeekdayThursday    -220.331    533.677  -0.413  0.68121   
# WeekdayTuesday     -248.911    550.648  -0.452  0.65290   
# WeekdayWednesday   -129.384    558.070  -0.232  0.81746   
# Is.Weekend               NA         NA      NA       NA   
# Is.Holiday        -1743.461    786.724  -2.216  0.03055 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1206 on 59 degrees of freedom
# Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8092 
# F-statistic: 13.59 on 30 and 59 DF,  p-value: < 2.2e-16

#remove is.weekend and Vis_High
predictorName<-predictorName[-c(15,25)]
data=data.frame(day[,predictorName],day[,outcomeName])
names(data)[25]<-outcomeName

lm.full2<-lm(Bike.Count~.,data=data)
summary(lm.full2) 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)      -14975.941  22626.341  -0.662  0.51062   
# MonthJan           -687.628    414.665  -1.658  0.10257   
# MonthMarch          243.167    423.296   0.574  0.56784   
# Day                  33.981     18.783   1.809  0.07553 . 
# Temp_High            49.199    271.897   0.181  0.85703   
# Temp_Avg            159.389    550.114   0.290  0.77303   
# Temp_Low              8.991    279.063   0.032  0.97441   
# Dew_High             12.634     61.294   0.206  0.83740   
# Dew_Avg             -84.069     90.286  -0.931  0.35557   
# Dew_Low              47.188     54.868   0.860  0.39326   
# Hum_High            -31.431     40.213  -0.782  0.43757   
# Hum_Avg              56.827     71.452   0.795  0.42961   
# Hum_Low             -22.751     32.650  -0.697  0.48866   
# Pres_High           579.132   3756.365   0.154  0.87800   
# Pres_Avg          -1339.090   6818.076  -0.196  0.84497   
# Pres_Low           1175.121   3943.695   0.298  0.76677   
# Vis_Avg              88.952    283.269   0.314  0.75461   
# Vis_Low              97.284    102.611   0.948  0.34696   
# Wind_Avg            -86.376     40.112  -2.153  0.03539 * 
#   Rain_Inches       -3252.357   1193.619  -2.725  0.00845 **
#   Rain               -432.363    468.566  -0.923  0.35990   
# Fog                 607.560    762.410   0.797  0.42871   
# Snow               1201.443    612.428   1.962  0.05451 . 
# Thunderstorm        540.906   1365.051   0.396  0.69335   
# WeekdayMonday       147.423    552.271   0.267  0.79045   
# WeekdaySaturday    -268.785    551.962  -0.487  0.62809   
# WeekdaySunday     -1748.884    519.112  -3.369  0.00133 **
#   WeekdayThursday    -220.331    533.677  -0.413  0.68121   
# WeekdayTuesday     -248.911    550.648  -0.452  0.65290   
# WeekdayWednesday   -129.384    558.070  -0.232  0.81746   
# Is.Holiday        -1743.461    786.724  -2.216  0.03055 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1206 on 59 degrees of freedom
# Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8092 
# F-statistic: 13.59 on 30 and 59 DF,  p-value: < 2.2e-16


# Step function -----------------------------------------------------------------
## Begin by defining the models with no variables (null) and all variables (full)
lm.null <- lm(Bike.Count~0,data=data)

## step selection
lm.step<-step(lm.null, scope=list(lower=lm.null, upper=lm.full2), direction="both")

# Step:  AIC=1278.94
# Bike.Count ~ Temp_High + Rain_Inches + Month + Day + Rain + Weekday + 
#   Wind_Avg + Is.Holiday + Temp_Low + Snow + Vis_Low - 1

# Investigate correlation --------------------------------------------------------------------------
is.num <- sapply(data, is.numeric)
num.df <- data[, is.num]
cor(num.df)

vif(lm.full2) # a lot of them above 5
vif(lm.step) #temphigh and templow >5


# Step function without high vif variable ---------------------------------------
lm.full3<-lm(Bike.Count~.-Temp_High - Hum_Avg - Pres_Avg,data=data)
summary(lm.full3)
lm.step2<-step(lm.null, scope=list(lower=lm.null, upper=lm.full3), direction="both")
# Step:  AIC=1278.24
# Bike.Count ~ Temp_Avg + Rain_Inches + Month + Rain + Weekday + 
#   Day + Is.Holiday + Wind_Avg + Snow + Vis_Low - 1
summary(lm.step2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# Temp_Avg           189.57      14.99  12.646  < 2e-16 ***
#   Rain_Inches      -3897.44     832.03  -4.684 1.27e-05 ***
#   MonthFeb         -1034.73     986.17  -1.049 0.297529    
# MonthJan         -1797.95     880.97  -2.041 0.044880 *  
#   MonthMarch        -590.28     988.71  -0.597 0.552341    
# Rain              -533.19     338.49  -1.575 0.119532    
# WeekdayMonday      220.51     453.86   0.486 0.628525    
# WeekdaySaturday   -265.18     461.09  -0.575 0.566978    
# WeekdaySunday    -1687.49     449.79  -3.752 0.000349 ***
#   WeekdayThursday   -112.93     451.04  -0.250 0.803001    
# WeekdayTuesday    -195.96     465.26  -0.421 0.674856    
# WeekdayWednesday  -155.20     468.08  -0.332 0.741175    
# Day                 31.66      14.80   2.138 0.035836 *  
#   Is.Holiday       -1798.76     672.32  -2.675 0.009207 ** 
#   Wind_Avg           -88.44      31.73  -2.787 0.006772 ** 
#   Snow              1010.29     437.79   2.308 0.023852 *  
#   Vis_Low            110.37      48.85   2.259 0.026859 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1116 on 73 degrees of freedom
# Multiple R-squared:  0.9829,	Adjusted R-squared:  0.979 
# F-statistic: 247.3 on 17 and 73 DF,  p-value: < 2.2e-16
vif(lm.step2) #temp_avg: 5.993089
              #not remove since high pvalue
# > vif(lm.step2)
#                 GVIF Df GVIF^(1/(2*Df))
# Temp_Avg    35.917120  1        5.993089
# Rain_Inches  2.018002  1        1.420564
# Month       82.648773  3        2.087081
# Rain         3.406187  1        1.845586
# Weekday     14.411328  6        1.248989
# Day          5.026538  1        2.241994
# Is.Holiday   1.452777  1        1.205312
# Wind_Avg     8.890154  1        2.981636
# Snow         1.847969  1        1.359400
# Vis_Low     11.789934  1        3.433647

# step without month ----------------------------------------------------
# since month has a high gvif
lm.full4<-lm(Bike.Count~.-Temp_High - Hum_Avg - Pres_Avg - Month ,data=data)
lm.step3<-step(lm.null, scope=list(lower=lm.null, upper=lm.full4), direction="both")
# Step:  AIC=1277.08
# Bike.Count ~ Temp_Avg + trans_rain + Weekday + Is.Holiday + Day + 
#   Pres_Low + Temp_Low + Snow + Wind_Avg + Hum_High + Vis_Low - 
#   1

summary(lm.step3)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# Temp_Avg            296.79      38.89   7.631 6.94e-11 ***
#   trans_rain        -3181.63     700.55  -4.542 2.16e-05 ***
#   WeekdayFriday    -28529.82   16507.13  -1.728  0.08816 .  
# WeekdayMonday    -28235.98   16558.93  -1.705  0.09241 .  
# WeekdaySaturday  -28775.91   16519.77  -1.742  0.08574 .  
# WeekdaySunday    -30103.22   16515.49  -1.823  0.07244 .  
# WeekdayThursday  -28594.69   16476.23  -1.736  0.08687 .  
# WeekdayTuesday   -28788.95   16447.45  -1.750  0.08426 .  
# WeekdayWednesday -28533.31   16436.83  -1.736  0.08679 .  
# Is.Holiday        -1975.07     653.60  -3.022  0.00346 ** 
#   Day                  33.53      14.90   2.251  0.02739 *  
#   Pres_Low            904.57     539.08   1.678  0.09763 .  
# Temp_Low            -83.53      44.92  -1.860  0.06696 .  
# Snow               1386.15     445.15   3.114  0.00264 ** 
#   Wind_Avg            -79.71      33.67  -2.367  0.02058 *  
#   Hum_High            -19.15      11.37  -1.685  0.09631 .  
# Vis_Low              68.88      52.43   1.314  0.19306    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1108 on 73 degrees of freedom
# Multiple R-squared:  0.9832,	Adjusted R-squared:  0.9792 
# F-statistic: 250.6 on 17 and 73 DF,  p-value: < 2.2e-16

# residuals analysis ----------------------------------------------
## Find the residuals
ei<-resid(lm.step3)

## Find the studentized residuals
ri<-rstandard(lm.step3)

## Find the R-student residuals
ti<-rstudent(lm.step3)

## Normal probabilty plot : most points look normal. there are two exception
qqnorm(rstudent(lm.step3))
qqline(rstudent(lm.step3))

## Residual plot vs. fitted values : most points look normal.  there are two exception
yhat <- fitted(lm.step3)
plot(yhat,ti)

summary(influence.measures(lm.step3))
#84 is an influential points.

## plot against individual explainatory variable
# Bike.Count ~ Temp_Avg + Rain_Inches + Month + Rain + Weekday + 
#   Day + Is.Holiday + Wind_Avg + Snow + Vis_Low - 1
plot(data$Temp_Avg,ti) #mostly normal beside two pts
plot(data$Rain_Inches,ti) #not normal!
plot(data$Wind_Avg,ti) #kind of normal 
plot(data$Vis_Low,ti) #kind of normal 

#box-cox
boxcox(lm.step2)$lambda  #center around -0.75

# transform variable ----------------------------------------------- 
data$trans_rain<-data$Rain_Inches^0.5
lm.4<-lm(Bike.Count ~ Temp_Avg + trans_rain + Rain + Weekday + 
     Day + Is.Holiday + Wind_Avg + Snow + Vis_Low ,data=data)
summary(lm.4) 
#Adjusted R-squared:  0.8202  
#trans_rain       trans_rain       -3541.99     797.56  -4.441 3.03e-05 ***
ti<-rstudent(lm.4)
plot(data$Rain_Inches,ti) #still not normal


# cross validate -----------------------------------------------
s.cv <- cv.lm(data=data, form.lm=lm.step3, m=2, plotit=T) 
# Sum of squares = 96930568    Mean square = 2154013    n = 45 
# Overall (Sum over all 45 folds) 
# ms 2033546 

# plot shows our final model (lm.step3) is pretty good.
