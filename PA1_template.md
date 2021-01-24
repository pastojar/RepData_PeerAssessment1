---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

<!-- indents the code chunks -->






## Loading and preprocessing the data
    
    ```r
    data <- read.csv("activity.csv")
    data$date <- lubridate::as_date(data$date)
    ```




## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day
    
    ```r
    data_daily <- data.frame( day = unique(data$date), steps = NA )
    data_daily$steps <- tapply( data$steps, data$date, sum, na.rm = T )
    hist(x = data_daily$steps, breaks = 50, xlab = "steps", main = "Steps per Day - Histogram")
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2. Calculate and report the mean and median total number of steps taken per day

- The mean 
    
    ```r
    mean(data_daily$steps, na.rm = T)
    ```
    
    ```
    ## [1] 9354.23
    ```

- The median
    
    ```r
    median(data_daily$steps, na.rm = T)
    ```
    
    ```
    ## [1] 10395
    ```





## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    
    ```r
    data_pattern <- data.frame( interval = unique(data$interval), steps = NA )
    data_pattern$steps <- tapply( data$steps, data$interval, mean, na.rm = T )
    plot( x = data_pattern$interval, y = data_pattern$steps, type = "l", xlab = "interval", ylab = "steps" )
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    
    ```r
    data_pattern$interval[ order(data_pattern$steps, decreasing = T) [1] ]
    ```
    
    ```
    ## [1] 835
    ```




## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    
    ```r
    NAsteps <- which(is.na(data$steps))
    length(NAsteps)
    ```
    
    ```
    ## [1] 2304
    ```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    
    ```r
    # I choose to use the mean for the given 5-minute interval. See below.
    ```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
    
    ```r
    data_new <- data
    data_new$steps[ NAsteps ] <- data_pattern$steps[ match( data$interval[ NAsteps ] , data_pattern$interval ) ]
    ```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

- The histogram
    
    ```r
    data_daily_new <- data.frame( day = unique(data_new$date), steps = NA )
    data_daily_new$steps <- tapply( data_new$steps, data_new$date, sum, na.rm = T )
    hist(x = data_daily_new$steps, breaks = 50, xlab = "steps", main = "Steps per Day - Histogram - Corrected Data")
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

- The mean 
    
    ```r
    mean(data_daily_new$steps, na.rm = T)
    ```
    
    ```
    ## [1] 10766.19
    ```

- The median
    
    ```r
    median(data_daily_new$steps, na.rm = T)
    ```
    
    ```
    ## [1] 10766.19
    ```

- The newly calculates values differ form the ones calculated in the beginning, in which the missing data shifted the statistics towards lower values.




## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
    
    ```r
    weekend <- function( date ) {
            out <- weekdays(date)
            out[ out %in% c("pondělí", "úterý", "středa", "čtvrtek", "pátek") ] <- "weekday"
            out[ out != "weekday" ] <- "weekend"
            return( as.factor(out) )
    }
    
    data_new$weekend <- weekend( data_new$date )
    ```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data

    
    ```r
    data_new_weekday <- subset(data_new, weekend == "weekday" )
    data_new_weekend <- subset(data_new, weekend != "weekday" )
    data_pattern_weekday  <- data.frame( interval = unique(  data_new_weekday$interval), steps = NA )
    data_pattern_weekend  <- data.frame( interval = unique(  data_new_weekend$interval), steps = NA )
    data_pattern_weekday$steps <- tapply( data_new_weekday$steps, data_new_weekday$interval, mean, na.rm = T )
    data_pattern_weekend$steps <- tapply( data_new_weekend$steps, data_new_weekend$interval, mean, na.rm = T )
    
    par(mfrow = c(2,1),  oma = c(0, 0, 2, 0))
    plot( x = data_pattern$interval, y = data_pattern_weekday$steps, type = "l", xlab = "interval", ylab = "steps", main = "weekday" )
    plot( x = data_pattern$interval, y = data_pattern_weekend$steps, type = "l", xlab = "interval", ylab = "steps", main = "weekend" )
    mtext( "The average number of steps taken during a day", outer = T )  
    ```
    
    ![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->





