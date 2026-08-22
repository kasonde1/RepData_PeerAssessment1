---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data


``` r
unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("numeric", "Date", "numeric"))
```

## What is mean total number of steps taken per day?


``` r
steps_per_day <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

hist(steps_per_day$steps, 
     main = "Total Steps Taken Each Day", 
     xlab = "Number of Steps", 
     col = "lightblue", 
     breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

``` r
mean_steps <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)
```

- **Mean steps per day:** 1.0766189\times 10^{4}
- **Median steps per day:** 1.0765\times 10^{4}

## What is the average daily activity pattern?


``` r
avg_steps_interval <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

plot(avg_steps_interval$interval, avg_steps_interval$steps, 
     type = "l", 
     col = "blue", 
     xlab = "5-minute Interval", 
     ylab = "Average Number of Steps", 
     main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

``` r
max_interval <- avg_steps_interval$interval[which.max(avg_steps_interval$steps)]
```

- **5-minute interval with maximum average steps:** 835

## Imputing missing values


``` r
total_na <- sum(is.na(data$steps))
```

- **Total number of missing values:** 2304


``` r
# Imputing missing values using the mean for that 5-minute interval
data_imputed <- data
for (i in 1:nrow(data_imputed)) {
    if (is.na(data_imputed$steps[i])) {
        interval_val <- data_imputed$interval[i]
        data_imputed$steps[i] <- avg_steps_interval$steps[avg_steps_interval$interval == interval_val]
    }
}

steps_per_day_imputed <- aggregate(steps ~ date, data_imputed, sum)

hist(steps_per_day_imputed$steps, 
     main = "Total Steps Taken Each Day (Imputed Data)", 
     xlab = "Number of Steps", 
     col = "lightgreen", 
     breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
mean_steps_imputed <- mean(steps_per_day_imputed$steps)
median_steps_imputed <- median(steps_per_day_imputed$steps)
```

- **Mean steps per day (imputed):** 1.0766189\times 10^{4}
- **Median steps per day (imputed):** 1.0766189\times 10^{4}

## Are there differences in activity patterns between weekdays and weekends?


``` r
data_imputed$day_type <- ifelse(weekdays(data_imputed$date) %in% c("Saturday", "Sunday"), 
                                "weekend", "weekday")
data_imputed$day_type <- as.factor(data_imputed$day_type)

avg_steps_daytype <- aggregate(steps ~ interval + day_type, data_imputed, mean)

library(lattice)
xyplot(steps ~ interval | day_type, 
       data = avg_steps_daytype, 
       type = "l", 
       layout = c(1, 2), 
       xlab = "Interval", 
       ylab = "Number of steps", 
       main = "Activity Patterns: Weekdays vs. Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
