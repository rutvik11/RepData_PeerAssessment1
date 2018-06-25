Introduction
------------

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a Fitbit, Nike
Fuelband, or Jawbone Up. These type of devices are part of the
"quantified self" movement – a group of enthusiasts who take
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

Loading and Preprocessing
-------------------------

Using **read.csv()** load the activity dataset.

    activity <- read.csv("activity.csv", header = T, stringsAsFactors = F)

    # View first few rows
    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    # Inspect the columns of data
    str(activity)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    # Transform the date attribute to an actual date format
    activity$date <- as.POSIXct(activity$date, format="%Y-%m-%d")

    ## Warning in strptime(x, format, tz = tz): unknown timezone 'zone/tz/2018c.
    ## 1.0/zoneinfo/Asia/Kolkata'

What is mean total number of steps taken per day ?
--------------------------------------------------

### 1. Calculate the total number of steps taken per day

    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    total_num_steps <- activity %>% select("steps", "date") %>% group_by(date) %>% summarise(steps = sum(steps, na.rm = T))
    total_num_steps

    ## # A tibble: 61 x 2
    ##          date steps
    ##        <dttm> <int>
    ##  1 2012-10-01     0
    ##  2 2012-10-02   126
    ##  3 2012-10-03 11352
    ##  4 2012-10-04 12116
    ##  5 2012-10-05 13294
    ##  6 2012-10-06 15420
    ##  7 2012-10-07 11015
    ##  8 2012-10-08     0
    ##  9 2012-10-09 12811
    ## 10 2012-10-10  9900
    ## # ... with 51 more rows

### 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

    library(ggplot2)

    ggplot(total_num_steps, aes(x = steps)) +
        geom_histogram(fill = "Purple", binwidth = 2000) +
        labs(title = "Daily Steps", x = "Steps", y = "Frequency")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### 3. Calculate and report the mean and median of the total number of steps taken per day.

    paste("Mean of the total number of steps taken per day -", mean(total_num_steps$steps))

    ## [1] "Mean of the total number of steps taken per day - 9354.22950819672"

    paste("Median of the total number of steps taken per day -", median(total_num_steps$steps))

    ## [1] "Median of the total number of steps taken per day - 10395"

What is the average daily activity pattern ?
--------------------------------------------

### 1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

    activity_pattern <- activity %>% select("steps", "interval") %>% group_by(interval) %>% summarise(mean_steps = mean(steps, na.rm = T))

    # Compute the time series plot
    plot(activity_pattern$interval, 
         activity_pattern$mean_steps, 
         type="l", 
         col="Purple", 
         lwd=2, 
         xlab="Interval [minutes]", 
         ylab="Average number of steps", 
         main="Time-series of the average number of steps per intervals - (NA removed)")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps ?

    activity_pattern[which.max(activity_pattern$mean_steps), 1]

    ## # A tibble: 1 x 1
    ##   interval
    ##      <int>
    ## 1      835

The 5-minute interval that contains the maximum of steps, on average
across all days, is **835**.

Imputing missing values
-----------------------

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

    sum(is.na(activity$steps))

    ## [1] 2304

The number of NA’s is 2304.

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

    for(i in 1:ncol(activity)){
      activity[is.na(activity[,i]), i] <- mean(activity[,i], na.rm = TRUE)
    }

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

    write.csv(x = activity, file = "tidy_data.csv", row.names = F)

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

    # read tidy data
    cleaned_activity <- read.csv("tidy_data.csv", header = T, stringsAsFactors = F)

    total_num_steps <- cleaned_activity %>% select("steps", "date") %>% group_by(date) %>% summarise(steps = sum(steps))
    total_num_steps

    ## # A tibble: 61 x 2
    ##          date    steps
    ##         <chr>    <dbl>
    ##  1 2012-10-01 10766.19
    ##  2 2012-10-02   126.00
    ##  3 2012-10-03 11352.00
    ##  4 2012-10-04 12116.00
    ##  5 2012-10-05 13294.00
    ##  6 2012-10-06 15420.00
    ##  7 2012-10-07 11015.00
    ##  8 2012-10-08 10766.19
    ##  9 2012-10-09 12811.00
    ## 10 2012-10-10  9900.00
    ## # ... with 51 more rows

    ggplot(total_num_steps, aes(x = steps)) +
        geom_histogram(fill = "Purple", binwidth = 2000) +
        labs(title = "Daily Steps", x = "Steps", y = "Frequency")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

    paste("Mean of the total number of steps taken per day -", mean(total_num_steps$steps))

    ## [1] "Mean of the total number of steps taken per day - 10766.1886792453"

    paste("Median of the total number of steps taken per day -", median(total_num_steps$steps))

    ## [1] "Median of the total number of steps taken per day - 10766.1886792453"

These formulas gives a mean and median of 10766 and 10766 respectively.

These values differ greatly from the estimates from the first part of
the assignment. The impact of imputing the missing values is to have
more data, hence to obtain a bigger mean and median value.

Are there differences in activity patterns between weekdays and weekends ?
--------------------------------------------------------------------------

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    activity$weekday <- weekdays(activity$date)
    activity[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = activity$weekday), "daytype"] <- "weekday"
    activity[grepl(pattern = "Saturday|Sunday", x = activity$weekday), "daytype"] <- "weekend"

    activity$daytype <- as.factor(activity$daytype)

    str(activity)

    ## 'data.frame':    17568 obs. of  5 variables:
    ##  $ steps   : num  37.4 37.4 37.4 37.4 37.4 ...
    ##  $ date    : POSIXct, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ weekday : chr  "Monday" "Monday" "Monday" "Monday" ...
    ##  $ daytype : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...

### 2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

    avg <- activity %>% select("steps", "interval", "daytype") %>% group_by(interval, daytype) %>% summarise(steps = mean(steps))

    ggplot(avg, aes(interval, steps)) + geom_line() + facet_grid(daytype ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)
