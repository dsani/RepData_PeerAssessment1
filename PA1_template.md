# Reproducible Research: Peer Assessment 1

```r
library(lattice)
```
## Loading and preprocessing the data
* Load the data (i.e. read.csv())

```r
actdata=read.csv("activity.csv")
```
* Process/transform the data (if necessary) into a format suitable for your analysis

```r
totsteps<-aggregate(steps~date,data=actdata,sum,na.rm=TRUE)
```

## What is mean total number of steps taken per day?
* Make a histogram of the total number of steps taken each day

```r
hist(totsteps$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

* Calculate and report the mean and median total number of steps taken per day


```r
mean(totsteps$steps)
```

```
## [1] 10766
```

```r
median(totsteps$steps)
```

```
## [1] 10765
```
* The mean is 1.0766 &times; 10<sup>4</sup> steps/day.
* The median is 10765 steps/day.

## What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsInterval<-aggregate(steps~interval,data=actdata,mean,na.rm=TRUE)
plot(steps~interval,data=stepsInterval,type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepsInterval[which.max(stepsInterval$steps),]$interval
```

```
## [1] 835
```

It is  **835** interval.


## Imputing missing values
* Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(actdata$steps))
```

```
## [1] 2304
```
Total missing rows are 2304.

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

:Strategy - Fill in all of the missing values with the mean for that 5-minute interval. Create a function **"interval2steps"** to get the mean steps for particular 5-minute interval. 

```r
interval2steps<-function(interval){
    stepsInterval[stepsInterval$interval==interval,]$steps
}
```

* Create a new dataset that is equal to the original dataset but with the missing data filled in


```r
actdataFilled<-actdata
count=0
for(i in 1:nrow(actdataFilled)){
    if(is.na(actdataFilled[i,]$steps)){
        actdataFilled[i,]$steps<-interval2steps(actdataFilled[i,]$interval)
        count=count+1
    }
}
cat("Total ",count, "NA values filled.\n\r")  
```

```
## Total  2304 NA values filled.
## 
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
totsteps2<-aggregate(steps~date,data=actdataFilled,sum)
hist(totsteps2$steps)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
mean(totsteps2$steps)
```

```
## [1] 10766
```

```r
median(totsteps2$steps)
```

```
## [1] 10766
```
* The **mean** total number of steps taken is 1.0766 &times; 10<sup>4</sup> steps/day.
* The **median** total number of steps taken is 1.0766 &times; 10<sup>4</sup> steps/day.


: The **mean** value is the **same** as the value before imputing missing data. The median value shows **a little** difference.


## Are there differences in activity patterns between weekdays and weekends?
* Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
actdataFilled$day=ifelse(as.POSIXlt(as.Date(actdataFilled$date))$wday%%6==0,"weekend","weekday")
actdataFilled$day=factor(actdataFilled$day,levels=c("weekday","weekend"))
```


* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
stepsInterval2=aggregate(steps~interval+day,actdataFilled,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=stepsInterval2,aspect=1/2,type="l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
