## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
“quantified self” movement – a group of enthusiasts who take
measurements about themselves regularly to improve their health, to find
patterns in their behavior, or because they are tech geeks. But these
data remain under-utilized both because the raw data are hard to obtain
and there is a lack of statistical methods and software for processing
and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

-   Dataset: [Activity monitoring
    data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are
coded as 𝙽𝙰) </br> date: The date on which the measurement was taken in
YYYY-MM-DD format </br> interval: Identifier for the 5-minute interval
in which measurement was taken </br> The dataset is stored in a
comma-separated-value (CSV) file and there are a total of 17,568
observations in this dataset.

## Loading and preprocessing the data

``` r
#Load necessary packages
library(ggplot2)

#Unzip filr and Read csv Data into Data.Table. 
activityData <- read.csv(file="activity.csv", header=TRUE)
activityData$date <-as.Date(activityData$date, format="%Y-%m-%d")
```

## What is mean total number of steps taken per day?

``` r
#Calculate the total number of steps taken per day

AggregatedData<-aggregate(activityData$steps, list(activityData$date), sum, na.rm=TRUE)
colnames(AggregatedData) <- c("date", "steps")

#Make a histogram of the total number of steps taken each day

hist(AggregatedData$steps,breaks=10, main="Histogram of total steps per day", xlab="Total steps per day", ylab="Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
#Calculate and report the mean and median of the total number of steps taken per day

mean_per_day<-mean(AggregatedData$steps, na.rm=TRUE)
median_per_day<-median(AggregatedData$steps, na.rm=TRUE)
```

## What is the average daily activity pattern?

``` r
#Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

timeseries_steps<-aggregate(activityData$steps, list(activityData$interval), mean, na.rm=TRUE)
colnames(timeseries_steps) <- c("interval", "steps")
plot(timeseries_steps$interval,timeseries_steps$steps, type="l", xlab="5-minute Interval", ylab="Mean of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
      
timeseries_steps$interval[which.max(timeseries_steps$steps)]
```

    ## [1] 835

## Imputing missing values

``` r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

sum(is.na(activityData$steps) == TRUE)
```

    ## [1] 2304

``` r
#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Replacing missing values with the 5-day average of that respective interval.

# Create a new dataset that is equal to the original dataset but with 
# the missing data filled in.
meanStepsByInt <- aggregate(steps ~ interval, activityData, mean)
imp_activityData <- transform(activityData,
                              steps = ifelse(is.na(activityData$steps),
                                             meanStepsByInt$steps[match(activityData$interval, 
                                                                        meanStepsByInt$interval)],
                                             activityData$steps))

# Make a histogram of the total number of steps taken each day and and report the mean and median.
impStepsByInt <- aggregate(steps ~ date, imp_activityData, FUN=sum)
hist(impStepsByInt$steps,
     main = "Imputed Number of Steps Per Day",
     xlab = "Number of Steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
# Calculate the total steps taken per day
totalSteps <- aggregate(steps ~ date, activityData, FUN=sum)

#Calculating Mean and Median
meanSteps <- mean(totalSteps$steps, na.rm = TRUE)
medSteps <- median(totalSteps$steps, na.rm = TRUE)
impMeanSteps <- mean(impStepsByInt$steps, na.rm = TRUE)
impMedSteps <- median(impStepsByInt$steps, na.rm = TRUE)
diffMean = impMeanSteps - meanSteps
diffMed = impMedSteps - medSteps
diffTotal = sum(impStepsByInt$steps) - sum(totalSteps$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

``` r
# Create a new factor variable in the dataset with two levels - "weekend" and "weekday"
DayType <- function(date) {day <- weekdays(date)
            if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
                  return ("weekeday")
            else if (day %in% c('Saturday', 'Sunday'))
                  return ("weekend")
            else
                  stop ("Invalid Date Format.")}
imp_activityData$date <- as.Date(imp_activityData$date)
imp_activityData$day <- sapply(imp_activityData$date, FUN = DayType)

# Make a panel plot containnig a time-series plot of the 5-minute interval
# and the average number of steps taken across all weekdays or weekends
meanStepsByDay <- aggregate(steps ~ interval + day, imp_activityData, mean)
ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + geom_line() + facet_grid(day ~ .) +
      ggtitle("Average Daily Activity Pattern") + xlab("5-minute Interval") + ylab("Average Number of        Steps") + theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)
