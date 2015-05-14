# Reproducible Research: Peer Assessment 1

  
### Loading and preprocessing the data  

Read in the data and make a back-up

```r
activity <- read.csv("D:/BI/R/Course5/activity.csv", stringsAsFactors=FALSE)
activity_bu <- activity
```
  
### What is mean total number of steps taken per day?  
summarize the data   


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity.aggr.pday <- aggregate(steps~date,activity,FUN=mean )
hist(activity.aggr.pday$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
#summary with mean and median 
summary(activity.aggr.pday$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1424 30.7000 37.3800 37.3800 46.1600 73.5900
```

```r
boxplot(activity.aggr.pday$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-2.png) 
  
## What is the average daily activity pattern?

```r
#time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
activity.aggr.pint <- aggregate(steps~interval,activity,FUN=mean )
attach(activity.aggr.pint)
plot(interval,steps,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
activity.aggr.pint[steps==max(steps),]      
```

```
##     interval    steps
## 104      835 206.1698
```

```r
detach(activity.aggr.pint)
```
  
### Imputing missing values

```r
# total number of missing values in the dataset (i.e. the total number of rows with NAs)
nrow(activity[!complete.cases(activity),])
```

```
## [1] 2304
```

```r
#fill NAs with the mean of the corresponding interval
# new dataset that is equal to the original dataset but with the missing data filled in
activity.aggr.pint <- aggregate(steps~interval,activity,FUN=mean)
activity.complete.cases <- activity
for (i in 1:dim(activity.complete.cases)[1]) {
  if (is.na(activity.complete.cases$steps[i])) {activity.complete.cases$steps[i] <- activity.aggr.pint[ activity.aggr.pint$interval==activity.complete.cases$interval[i]    ,"steps"] }
}
activity.aggr.pday2 <- aggregate(steps~date,activity.complete.cases,FUN=mean )
hist(activity.aggr.pday2$steps) 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#comparison Df with NAs and without NAs
summary(activity.aggr.pday$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1424 30.7000 37.3800 37.3800 46.1600 73.5900
```

```r
summary(activity.aggr.pday2$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.1424 34.0900 37.3800 37.3800 44.4800 73.5900
```

```r
par(mfrow=c(1,2))
boxplot(activity.aggr.pday$steps,  main="With NAs")
boxplot(activity.aggr.pday2$steps,  main="Without NAs")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-2.png) 
  
## Are there differences in activity patterns between weekdays and weekends?

```r
#weekend vs week
library("lubridate")
activity$weekend <- "NA"
str(activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekend : chr  "NA" "NA" "NA" "NA" ...
```

```r
is.weekend <- function(x) {
if (wday(x) %in% c(0,7)) {"Y"} else {"N"}
}
activity$weekend <- lapply(activity$date, is.weekend)

activity.weekend <- activity[activity$weekend=="Y", ] 
activity.week <- activity[activity$weekend=="N", ]
activity.aggr.weekend <- aggregate(steps~interval,activity.weekend,FUN=mean )
activity.aggr.week <- aggregate(steps~interval,activity.week,FUN=mean )
par(mfrow=c(1,1))
plot(activity.aggr.weekend$interval,activity.aggr.weekend$steps,type="l", col="blue", main="Weekend vs week")
lines(activity.aggr.week$interval,activity.aggr.week$steps, col="orange")
legend("topright",c("weekend","week") ,lty=c(1,1), lwd=c(2.5,2.5),col=c("blue","orange"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Conclusions: During the weekend the activity peaks are higher
