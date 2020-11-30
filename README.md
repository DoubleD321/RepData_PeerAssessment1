## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

## Set working directory and load library
setwd(choose.dir())
library(ggplot2)

Note that the data file already downloaded in GitHub and extracted from zip

## load data into R
activity_data<- read.csv("activity.csv")
str(activity_data)
summary(activity_data)

## aggregate steps data by date
activity_stepsPD<- with(activity_data,aggregate(steps, by=list(date),sum,na.rm=TRUE,header=TRUE))
names(activity_stepsPD)<- c("date","steps")

## determine the mean and median steps per day
mean(activity_stepsPD$steps)
median(activity_stepsPD$steps)

## aggregate steps date by interval and determine mean
activity_stepsPT<- with(activity_data,aggregate(steps, by=list(interval),mean,na.rm=TRUE))
names(activity_stepsPT)<- c("interval","MeanSteps")


## plot steps by interval
library(ggplot2) 
ggplot(activity_stepsPT, aes(interval,MeanSteps))+
geom_line(col="blue")+
        ggtitle("Average steps per interval")+
        xlab("Time interval")+ylab("Steps")+
        ggtitle("Steps per Time Interval")

## determine max number of steps in an interval (id interval)
max_stepsI<- activity_stepsPT[which.max(activity_stepsPT$MeanSteps),]
max_stepsI

## Identify missing data
missingVals<- !complete.cases(activity_data)
sum(missingVals==TRUE)

## Replace missing data with interval mean data
library(dplyr)
imp_data <- activity_data %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ activity_stepsPT$MeanSteps[match(activity_data$interval, activity_stepsPT$interval)],      
      TRUE ~ as.numeric(steps)
    ))

## Aggregate the step by interval for the "complete" dataset (imputed)
imp_data_stepsPT<- with(imp_data,aggregate(steps, by=list(interval),mean,na.rm=TRUE))
names(imp_data_stepsPT)<- c("interval","MeanSteps")

## Aggregate the step per day by date
imp_data_stepsPD<- with(imp_data,aggregate(steps, by=list(date),sum,header=TRUE))
str(imp_data_stepsPD)
summary(imp_data_stepsPD)
names(imp_data_stepsPD)<- c("date","steps")

## Calculate mean and median of the complete dataset interval
imp_mean<- mean(imp_data_stepsPT$MeanSteps)
imp_mean
imp_median<- median(imp_data_stepsPT$MeanSteps)
imp_median

## Calculate mean and median of the complete dataset steps per day
mean(imp_data_stepsPD$steps)
names(imp_data_stepsPD)<- c("date","steps")
median(imp_data_stepsPD$steps)

## Histogram of complete dataset - steps per day
hist(imp_data_stepsPD$steps,
     main="Total Steps Taken per day",
     xlab="Steps",col="green")

## Comparing mean and median of original dataset with the "complete' dataset
imp_mean<-mean(imp_data_stepsPD$steps)
imp_median<-median(imp_data_stepsPD$steps)

## Original mean and median
omean<-mean(activity_stepsPD$steps)
omedian<- median(activity_stepsPD$steps)

## Determine the difference between original and imputed
dif_mean<- imp_mean - omean
dif_mean
dif_median<- imp_median - omedian
dif_median

## Calculate a percentage change
mean_percent_chg<-(imp_mean - omean)/omean
mean_percent_chg
median_percent_chg<-(imp_median-omedian)/omedian
median_percent_chg


## Add a duplicate copy of date column and call it WD_WE
imp_data_WD_WE<- imp_data %>%
  mutate(WD_WE = date)

## Change data in WD_WE to indicate if the date falls on a weekday or weekend
imp_data_WD_WE$WD_WE <- ifelse(weekdays(as.Date(imp_data_WD_WE$WD_WE)) == "Saturday" | weekdays(as.Date(imp_data_WD_WE$WD_WE)) == "Sunday", "weekend", "weekday")

## Aggregate steps by WD_WE and interval
week_end_day <- aggregate(steps ~ WD_WE+interval, data=imp_data_WD_WE, sum)

## Plot comparing the weekday and weekend steps
ggplot(week_end_day, aes(interval, steps)) + 
  geom_line() + 
  facet_wrap(~WD_WE, nrow = 2) +
  xlab("Intervals (5 minute)") + 
  ylab("Number of steps")

