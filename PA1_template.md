---
title: "Reproducible Research Peer Assessment 1"
output:
  html_document: default
  'html_document:': default
keep_md: yes
---

## Loading and preprocessing the data

```r
#load packages:
library(ggplot2)
library(ggthemes)
activity <- read.csv("activity.csv")
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
activity$date <- as.POSIXct(activity$date, "%Y%m%d")
```

```
## Error in as.POSIXct(activity$date, "%Y%m%d"): object 'activity' not found
```

```r
day <- weekdays(activity$date)
```

```
## Error in weekdays(activity$date): object 'activity' not found
```

```r
activity <- cbind(activity, day)
```

```
## Error in cbind(activity, day): object 'activity' not found
```

```r
#looking at processed data
summary(activity)
```

```
## Error in summary(activity): object 'activity' not found
```
## What is mean total number of steps taken per day?

```r
activityTotalSteps <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
```

```
## Error in with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE)): object 'activity' not found
```

```r
names(activityTotalSteps) <- c("Date", "Steps")
```

```
## Error in names(activityTotalSteps) <- c("Date", "Steps"): object 'activityTotalSteps' not found
```

```r
totalStepsdf <- data.frame(activityTotalSteps)
```

```
## Error in data.frame(activityTotalSteps): object 'activityTotalSteps' not found
```

```r
# Plot the histogram using ggplot2
g <- ggplot(totalStepsdf, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#909800", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_calc(base_family = "sans")
```

```
## Error in ggplot(totalStepsdf, aes(x = Steps)): object 'totalStepsdf' not found
```

```r
print(g)
```

```
## Error in print(g): object 'g' not found
```

```r
mean(activityTotalSteps$Steps)
```

```
## Error in mean(activityTotalSteps$Steps): object 'activityTotalSteps' not found
```

```r
median(activityTotalSteps$Steps)
```

```
## Error in median(activityTotalSteps$Steps): object 'activityTotalSteps' not found
```
## What is the average daily activity pattern?

```r
# Calculating the average number of steps taken, 
#averaged across all days by 5-min intervals.
averageDailyActivity <- aggregate(activity$steps, by = list(activity$interval), 
FUN = mean, na.rm = TRUE)
```

```
## Error in aggregate(activity$steps, by = list(activity$interval), FUN = mean, : object 'activity' not found
```

```r
names(averageDailyActivity) <- c("Interval", "Mean")
```

```
## Error in names(averageDailyActivity) <- c("Interval", "Mean"): object 'averageDailyActivity' not found
```

```r
averageActivitydf <- data.frame(averageDailyActivity)
```

```
## Error in data.frame(averageDailyActivity): object 'averageDailyActivity' not found
```

```r
# Plot using ggplot2
da <- ggplot(averageActivitydf, mapping = aes(Interval, Mean)) + 
  geom_line(col = "#E01A86") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval") +
  theme_calc(base_family = "sans")
```

```
## Error in ggplot(averageActivitydf, mapping = aes(Interval, Mean)): object 'averageActivitydf' not found
```

```r
print(da)
```

```
## Error in print(da): object 'da' not found
```

```r
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
averageDailyActivity[which.max(averageDailyActivity$Mean), ]$Interval
```

```
## Error in eval(expr, envir, enclos): object 'averageDailyActivity' not found
```
## Imputing missing values

```r
# report the total number of rows with NAs.
sum(is.na(activity$steps))
```

```
## Error in eval(expr, envir, enclos): object 'activity' not found
```

```r
imputedSteps <- averageDailyActivity$Mean[match(activity$interval, averageDailyActivity$Interval)]
```

```
## Error in eval(expr, envir, enclos): object 'averageDailyActivity' not found
```

```r
activityImputed <- transform(activity, steps = ifelse(is.na(activity$steps), 
yes = imputedSteps, no = activity$steps))
```

```
## Error in transform(activity, steps = ifelse(is.na(activity$steps), yes = imputedSteps, : object 'activity' not found
```

```r
# Creating new dataset having imputed missing values.
totalActivityImputed <- aggregate(steps ~ date, activityImputed, sum)
```

```
## Error in eval(m$data, parent.frame()): object 'activityImputed' not found
```

```r
names(totalActivityImputed) <- c("date", "dailySteps")
```

```
## Error in names(totalActivityImputed) <- c("date", "dailySteps"): object 'totalActivityImputed' not found
```

```r
#check if dataset still has any missing values
sum(is.na(totalActivityImputed$dailySteps))
```

```
## Error in eval(expr, envir, enclos): object 'totalActivityImputed' not found
```

```r
totalImputedStepsdf <- data.frame(totalActivityImputed)
```

```
## Error in data.frame(totalActivityImputed): object 'totalActivityImputed' not found
```

```r
# Plot the histogram using ggplot2
p <- ggplot(totalImputedStepsdf, aes(x = dailySteps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#909800", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_calc(base_family = "sans")
```

```
## Error in ggplot(totalImputedStepsdf, aes(x = dailySteps)): object 'totalImputedStepsdf' not found
```

```r
print(p)
```

```
## Error in print(p): object 'p' not found
```

```r
#mean of the total number of steps taken per day is:
mean(totalActivityImputed$dailySteps)
```

```
## Error in mean(totalActivityImputed$dailySteps): object 'totalActivityImputed' not found
```

```r
#median of the total number of steps taken per day is:
median(totalActivityImputed$dailySteps)
```

```
## Error in median(totalActivityImputed$dailySteps): object 'totalActivityImputed' not found
```
## Are there differences in activity patterns between weekdays and weekends?

```r
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
```

```
## Error in strptime(activity$date, format = "%Y-%m-%d"): object 'activity' not found
```

```r
# Making a function which differentiates weekdays from weekends
activity$dayType <- sapply(activity$date, function(x) {
  if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
  {y <- "Weekend"}
  else {y <- "Weekday"}
  y
})
```

```
## Error in lapply(X = X, FUN = FUN, ...): object 'activity' not found
```

```r
activityByDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)
```

```
## Error in eval(m$data, parent.frame()): object 'activity' not found
```

```r
# Plot using ggplot2
dayPlot <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("Average daily steps by day type") + 
  xlab("Interval") + 
  ylab("Average number of steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day type") +
  theme_calc(base_family = "sans")
```

```
## Error in ggplot(activityByDay, aes(x = interval, y = steps, color = dayType)): object 'activityByDay' not found
```

```r
print(dayPlot)
```

```
## Error in print(dayPlot): object 'dayPlot' not found
```
