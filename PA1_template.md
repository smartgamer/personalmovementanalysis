---
title: "Personal Movement Analysis"
author: "Jerry J"
date: "August 20, 2017"
output: html_document
---



## Background
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

The dataset address : https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
The variables included in this dataset are:

    * steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
    * date: The date on which the measurement was taken in YYYY-MM-DD format
    * interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Q1. What is mean total number of steps taken per day?
    Calculate the total number of steps taken per day
    If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
    Calculate and report the mean and median of the total number of steps taken per day

```r
data = read.csv("activity.csv")
#library(tidyverse)

aggdata = aggregate(data$steps, by = list(data$date), FUN = sum, na.rm=T)
meanStepsday= mean(aggdata[,2])
meanStepsday
```

```
## [1] 9354.23
```

```r
hist(aggdata[,2])
```

![plot of chunk calculate steps per day](figure/calculate steps per day-1.png)

```r
medianStepsday = median(aggdata[,2])
medianStepsday
```

```
## [1] 10395
```
The mean total number of steps taken per day is 9354

The median total number of steps taken per day is 10395

### Q2. What is the average daily activity pattern?

    - Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    

```r
aggdata2 = aggregate(data$steps, by = list(data$interval), FUN = mean, na.rm=T)
colnames(aggdata2) = c("interval", "steps")
plot(aggdata2$interval, aggdata2$steps,type="l", xlab= "interval", ylab= "steps", col="green" , lwd=2 )
lines(aggdata2$interval, aggdata2[,2], type="l", col="red", lwd=2)
```

![plot of chunk average daily activity pattern](figure/average daily activity pattern-1.png)

```r
maxstepInterval = aggdata2[aggdata2$steps==max(aggdata2[,2]),]
maxstepInterval$interval
```

```
## [1] 835
```
### Q3. Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

    - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    - Create a new dataset that is equal to the original dataset but with the missing data filled in.
    - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
#check total number of missing values 
NAdata = subset(data, is.na(steps) == T)
nrow(NAdata)
```

```
## [1] 2304
```

```r
NAdata2=data[which(is.na(data)),]
nrow(NAdata2)
```

```
## [1] 2304
```

```r
# fill missing value with median for that day, or the mean for that 5-minute interval
dataImpute=data
narowIndex = which(is.na(dataImpute))
for (i in 1:length(narowIndex)) {
    dataImpute[i,]$steps = aggdata2[which(aggdata2$interval == dataImpute[i,]$interval),]$steps
    
}

library(tidyverse)
library(ggplot2)
dataImpAgg = aggregate(dataImpute$steps, by=list(dataImpute$date), FUN = sum, na.rm=T)
colnames(dataImpAgg) = c("date", "steps")
ggplot(data=dataImpAgg) + 
    geom_bar(mapping = aes(x=date, y=steps), stat="identity")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
meanSteps = mean(dataImpAgg$steps)
meanSteps
```

```
## [1] 9728.107
```

```r
medianSteps = median(dataImpAgg$steps)
medianSteps
```

```
## [1] 10600
```
### Q4. Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

    - Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
    - Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
dataImpDay = mutate(dataImpute, weekd=weekdays(as.Date(date)))
weekendlist = c("Saturday", "Sunday")
dataImpDay2 = mutate(dataImpDay, weekdy=ifelse(weekd %in% weekendlist, yes = "weekend", no = "weekday"))

dataImpday2agg = aggregate(steps ~ weekdy+interval, dataImpDay2, FUN = mean, na.rm=T)

library("lattice")
xyplot(steps ~ interval | weekdy,
       data = dataImpday2agg,
       type = "l",
       lty = c(1, 2, 2, 1),
       lwd = c(1, 1, 1, 3),
       col.line = c(rep("black",3), "red"))
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
    
