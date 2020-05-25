---
title: 'Reproducible Research: Peer Assessment 1'
author: "Lan Mu"
date: "5/22/2020"
output: 
  html_document:
    keep_md: true
---
# Introduction 
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## Loading and preprocessing the data
Loading the data:

```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
activity <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
```
Convert date variable to calender dates:

```r
activity$date<-as.POSIXct(activity$date,format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?
Calculate the total numbers of steps taken per day:

```r
library(dplyr)
library(tidyverse)
sum<-activity %>% drop_na(steps) %>% group_by(date) %>% summarise(TotalStep=sum(steps))
```
Make a histogram of the total number of steps taken each day

```r
png("Histogram of Total Steps.png")
hist(sum$TotalStep,main="Histogram of Total Number of Steps Taken Each Day",xlab="Total Number of Steps")
dev.off()
```

```
## png 
##   2
```

Calculate and report the mean and median of the total number of steps taken per day

```r
MeanSteps<-mean(sum$TotalStep)
MedianSteps<-median(sum$TotalStep)
```
The mean of total number of steps taken per day is 1.0766189\times 10^{4}; The median of total number of steps taken per day is 10765.

## What is the average daily activity pattern?
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
png("Time serious plot of average steps of each interval.png")
IntervalMean<-activity %>% drop_na(steps) %>% group_by(interval) %>% summarise(MeanSteps=mean(steps))
library(lattice)
xyplot(IntervalMean$MeanSteps~seq_along(IntervalMean$interval),type=c("l"),ylab="Average of Steps Taken",xlab="Time Interval",main="Time Series Plot of Interval and Average Steps")
dev.off()
```

```
## png 
##   2
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MaxInterval<-IntervalMean[which.max(IntervalMean$MeanSteps),1]
```
The 5-minute interval which contains the maximum number of steps is 835.

## Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
nmissing<-sum(is.na(activity$steps))
```
Total number of missing values in the dataset is 2304. 

Devise a strategy for filling in all of the missing values in the dataset. I used 
the mean of each interval across all days to fill in the missing values.

```r
NAstep<-activity[is.na(activity$steps),]
impute<-merge(NAstep,IntervalMean,by="interval",all.x=TRUE)
impute<-impute[,!names(impute) %in% c("steps"),drop=F]
colnames(impute)<-c("interval","date","steps")
NoNAstep<-activity[!is.na(activity$steps),]
ImputedAct<-rbind(NoNAstep,impute)
```
Make a histogram of the total number of steps taken each day

```r
png("Histogram of Imputed Total Steps.png")
nonasum<-ImputedAct  %>% group_by(date) %>% summarise(TotalStep=sum(steps))
hist(nonasum$TotalStep,main="Histogram of Total Number of Steps with NA Filled by Imputation",xlab="Total Number of Steps")
```

Calculate and report the mean and median total number of steps taken per day.

```r
NoNAMeanSteps<-mean(nonasum$TotalStep)
NoNAMedianSteps<-median(nonasum$TotalStep)
diffmean<-MeanSteps-NoNAMeanSteps
diffmedian<-MedianSteps-NoNAMedianSteps
```
The new mean of the imputed data is 1.0766189\times 10^{4}. The new mean is the same as the unimputed mean. The new median of the imputed data is 1.0766189\times 10^{4}. The new median increases -1.1886792 from the unimputed median.

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library(chron)
ImputedAct$week<-ifelse(is.weekend(ImputedAct$date)=="FALSE","Weekday","Weekend")
```
Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
IntervalMeanWk<-ImputedAct  %>% group_by(interval,week) %>% summarise(MeanSteps=mean(steps))
png("Time Series Plot of imputed average steps in each interval.png")
xyplot(IntervalMeanWk$MeanSteps~seq_along(IntervalMeanWk$interval)|IntervalMeanWk$week,type=c("l"),ylab="Average of Steps Taken",xlab="Time Interval",main="Time Series Plot of Interval and Average Steps by Weekday or Weekend",layout=c(1,2))
dev.off()
```

```
## png 
##   2
```
