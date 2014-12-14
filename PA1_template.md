---
#title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
# Title: "Reproducible Research: Peer Assessment 1"


## Load required packages


library(plyr)


library(ggplot2)


library(knitr)



## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Process/transform the data into a format suitable for your analysis


```r
setwd("C:/Users/Mandeep/Documents/Rcourse")
activitydata<-read.csv("./repdata-data-activity/activity.csv")
```



## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

2. Calculate and report the mean and median total number of steps taken/day



```r
totalStepsEachDay<-tapply(activitydata$steps,activitydata$date,sum)
hist(totalStepsEachDay,breaks=6,main="Frequency of steps each day",
     xlab="Number of steps each day",ylab="Frequency",col="red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
meanStepsPerDay<-mean(totalStepsEachDay,na.rm=TRUE)
medianStepsPerDay<-median(totalStepsEachDay,na.rm=TRUE)
```
Mean of total number of steps taken/day 1.0766189 &times; 10<sup>4</sup>.
Median of total number of steps taken/day 10765.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


Average number of steps taken per 5 minute of interval.


```r
totalStepsEachFiveMinInterval<-tapply(activitydata$steps,activitydata$interval,mean,na.rm=T)
```

Time Series Plot


```r
plot(totalStepsEachFiveMinInterval,type="l",main="Time Series Interval vs Average steps/Interval",
     xlab="Interval",ylab="Average Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

 


```r
intervalMaxSteps<-seq(along=totalStepsEachFiveMinInterval)[totalStepsEachFiveMinInterval == 
                                                             max(totalStepsEachFiveMinInterval)]
```
5-min interval with maximum number of steps 104

## Imputing missing values

Number of missing values in the dataset


```r
totalNA<-sum(is.na(activitydata$steps))
```
Total number of missing values is 2304.


A function replacing NA's in steps by the mean of 5-minute interval averaged across all days.


```r
na.replace <- function(act) {
  ddply(act, ~interval, function(dd) {
    steps <- dd$steps
    dd$steps[is.na(steps)] <- mean(steps, na.rm = TRUE)
    return(dd)
  })
}
```

Dataset with missing values filled in.


```r
imputedActivityData <- na.replace(activitydata)
```

Total number of steps taken each day


```r
imputedStepsPerDay<-ddply(imputedActivityData,~date,summarise,steps=sum(steps))
```

Histogram of total number of steps taken each day


```r
p <- ggplot(imputedStepsPerDay, aes(steps))
p <- p + geom_histogram(fill = "white", color = "blue")
p <- p + ggtitle("Total number of steps per day")
p + xlab("Steps per day")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

Mean and Median of total number of steps taken per day


```r
imputedMeanStepsPerDay <- mean(imputedStepsPerDay$steps)
imputedMedianStepsPerDay <- median(imputedStepsPerDay$steps)
```
Mean of total number of steps taken per day 1.0766189 &times; 10<sup>4</sup>.Median of total number of steps taken per day 1.0766189 &times; 10<sup>4</sup>

Imputation has slight affect on Median of number of steps taken per day while on Mean it does not have any affect. Filling in NA's have only slight impact on total daily number of steps.



## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 

```r
activitydata$date <- as.Date(activitydata$date, "%Y-%m-%d")
 
activitydata$day <- weekdays(activitydata$date)

activitydata$day_type <- c("weekday")

for (i in 1:nrow(activitydata)){
  if (activitydata$day[i] == "Saturday" || activitydata$day[i] == "Sunday"){
    activitydata$day_type[i] <- "weekend"
  }
}

activitydata$day_type <- as.factor(activitydata$day_type)

table_interval_steps_imputed <- aggregate(steps ~ interval+day_type, activitydata, mean)
```

Make a panel plot containing a time series plot of the 5-minute interval vs the average number of steps taken


```r
qplot(interval, steps, data=table_interval_steps_imputed, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ day_type, ncol=1)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
