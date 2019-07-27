#install.packages("sqldf")
#install.packages("geosphere")
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("sp")
#install.packages("rgdal")
#install.packages("rms")

#library(readr)
#library(sqldf)
#library(ggplot2)
#library(sp)
#library(rgdal)
#library(geosphere)
library(rms)

library(quantmod)

#Use getSymbol to grab data from yahoo finance by mentioning the start and end date
getSymbols("AAPL", src = 'yahoo', 
           from = "2000-01-31", 
           to = "2019-12-31",
           auto.assign = TRUE, 
           warnings = FALSE) 

#We can then transform the daily data into weekly data using to.weekly()
prices_weekly <- to.weekly(AAPL, indexAt = "lastof", OHLC = FALSE)

#Data Screening to make sure we don't have any missing values
summary(prices_weekly)
apply(prices_weekly,2,function(x) sum(is.na(x)))


###Data to begin, create a copy
dat0 = AAPL
colnames(dat0) = c("open", "high", "low", "close", "volume", "adjusted")
K = 6
N = nrow(dat0)
model1 = lm(adjusted ~ . ,data=dat0)

## laverage cutoff
leverage = hatvalues(model1)
cutleverage = (2*K + 2) / N
badlaverage = as.numeric(leverage > cutleverage)
table(badlaverage)

## cook cutoff
cooks = cooks.distance(model1)
cutcook = 4 / (N-K-1)
badcooks = as.numeric(cooks > cutcook)
table(badcooks)

## remove outlier
total = badcooks + badlaverage
noout = subset(dat0, total < 1)


## Model 2 -- revised model after cleaning data
model2 = lm(adjusted ~ . ,data=noout)
summary(model2, correlation = TRUE)

### Model 2 -- linearity
standardized2 = rstudent(model2)
fitted2 = scale(model2$fitted.values)
qqnorm(standardized2)
abline(0,1)

### Model 2 -- normality
hist(standardized2, breaks = 1000)

### Model 2 -- homogeneous
plot(fitted2)
abline(0,0)
abline(v = 0)


###Model 3 -- Model Selection
dat1 = noout

###Manuel Step-wise regression
null=lm(adjusted ~ 1, data=dat1)
StepF0=null
summary(StepF0)

add1(StepF0, scope = ~open + high + low + close + volume, test='F', data=dat1)
StepF1=update(StepF0, ~ low)
drop1(StepF1, test='F',data=dat1)

add1(StepF1, scope = ~open + high + low + close + volume, test='F', data=dat1)
StepF2=update(StepF1, ~ low + open)
summary(StepF2)
drop1(StepF2, test='F',data=dat1)

add1(StepF2, scope = ~open + high + low + close + volume, test='F', data=dat1)
StepF3=update(StepF2, ~ low + open + volume)
summary(StepF3)
drop1(StepF3, test='F',data=dat1)

add1(StepF3, scope = ~open + high + low + close + volume, test='F', data=dat1)

model3 = lm(adjusted~low + open + volume,data=dat1)
summary(model3)
anova(model3)



### Model 4 -- Automatic Step-wise regression, to confirm Model 3
model4.full = ols(adjusted ~ open
                  + high + low + close + volume, data=dat1)
fastbw(model4.full, rule="p",sls=0.001)
