# Reproducible Research: Peer Assessment 1

##Loading and preprocessing the data

Loading from activity.csv + transform date to Date format:

```r
activity <- read.csv("activity.csv", header=TRUE)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

##What is mean total number of steps taken per day?

Calculate the total number of steps taken per day: 

```r
activitycomp <- subset(activity, complete.cases(activity) == TRUE)
dailysteps <- as.numeric(tapply(activitycomp$steps, activitycomp$date, sum))
```

Histogram of the total number of steps taken each day:

```r
daylist <- unique(activitycomp$date)
plotdata <- data.frame(daylist, dailysteps)
hist(plotdata$dailysteps, col = "red", main = "Total number of steps taken per day", xlab = "Daily steps")
```

![](PA1_template_files/figure-html/histogram-1.png) 

Mean/Median calculation:

```r
mean(plotdata$dailysteps)
```

```
## [1] 10766.19
```

```r
median(plotdata$dailysteps)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

Average number of steps taken by 5-minute interval:

```r
avgstepsbyint <- as.numeric(tapply(activitycomp$steps, activitycomp$interval, mean))
intlist <- unique(activitycomp$interval)
plotdata2 <- data.frame(intlist, avgstepsbyint)
library(lattice)
xyplot(plotdata2$avgstepsbyint ~ plotdata2$intlist, type = "l", xlab = "Interval", ylab = "Number of steps", main = "Average daily activity pattern")
```

![](PA1_template_files/figure-html/avg activity-1.png) 

Most active 5-minute interval based on avg. number of steps taken:

```r
which.max(plotdata2$avgstepsbyint)
```

```
## [1] 104
```

```r
plotdata2$intlist[which.max(plotdata2$avgstepsbyint)] 
```

```
## [1] 835
```

```r
x <- which.max(plotdata2$avgstepsbyint)
y <- plotdata2$intlist[which.max(plotdata2$avgstepsbyint)] 
z <- round(max(plotdata2$avgstepsbyint))
```

It means, that the Nr. 104 5-minute interval is the most active, name of this interval: 835 (HMM), avg. steps taken: 206. 

##Imputing missing values

Total number of rows with NA:

```r
nrow(activity) - nrow(na.omit(activity))
```

```
## [1] 2304
```

Impute missing steps value with average:

```r
stepsimp <- activity$steps
z <- round(mean(stepsimp, na.rm=TRUE))
stepsimp[ is.na(stepsimp) ] <- z
```

Create new dataset with imputed data:

```r
date <- activity$date
interval <- activity$interval
activityimp <- data.frame(stepsimp, date, interval)
```

New historgram, using imputed data:

```r
dailysteps2 <- as.numeric(tapply(activityimp$steps, activityimp$date, sum))
daylist2 <- unique(activityimp$date)
plotdata3 <- data.frame(daylist2, dailysteps2)
hist(plotdata3$dailysteps2, col = "grey", main = "Total number of steps taken per day (after imputation)", xlab = "Daily steps")
```

![](PA1_template_files/figure-html/Imp histogram-1.png) 

New Mean/Median calculation:

```r
mean(plotdata3$dailysteps2)
```

```
## [1] 10751.74
```

```r
median(plotdata3$dailysteps2)
```

```
## [1] 10656
```

What is the difference between original and new dataset (using histograms):

```r
par(mfrow=c(1,2))
hist(plotdata$dailysteps, ylim = c(1,35), col = "red", main = "Original", xlab = "Daily steps")
hist(plotdata3$dailysteps2, col = "grey", main = "New (after imputation)", xlab = "Daily steps")
```

![](PA1_template_files/figure-html/Hist diff-1.png) 

The impact: frequency in category including mean value increased due to the selected imputing strategy. 

##Are there differences in activity patterns between weekdays and weekends?

Create new factor variable with "weekday"/"weekend" levels in the dataset:

```r
Sys.setlocale("LC_ALL", "C") #To be sure, that the weekday will be the expected
```

```
## [1] "C"
```

```r
daycateg <- weekdays(activityimp$date)
daycateg[ daycateg == "Sunday" | daycateg == "Saturday" ] <- "weekend"
daycateg[ daycateg != "weekend" ] <- "weekday"
daycatfac <- as.factor(daycateg)
activityimp2 <- cbind(activityimp, daycatfac)
```

Panel plot with avg. steps by 5-minute interval for weekdays/weekends:

```r
plotdata4 <- as.data.frame(aggregate(activityimp2$steps ~ activityimp2$interval + activityimp2$daycatfac, activityimp2, mean))
names(plotdata4) <- c("interval", "daycatfac", "steps")
library(lattice)
xyplot(plotdata4$steps ~ plotdata4$interval | plotdata4$daycatfac,layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps") 
```

![](PA1_template_files/figure-html/Weekday/Weekend plot-1.png) 

Based on the plot, it seems there is difference between weekdays and weekends. I mean at the weekends the given user is more active, while on weekdays his/her activity is focused on morning hours. 
