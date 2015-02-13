# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# load data
steps <- read.csv('./activity.csv', header = T)

# packages
suppressPackageStartupMessages(library('dplyr'))
suppressPackageStartupMessages(library('ggplot2'))
suppressPackageStartupMessages(library('knitr'))
```

## What is mean total number of steps taken per day?

```r
# Number of steps/day
daystepcount <- steps %>% group_by(date) %>% summarise(day.count = sum(steps))

# Mean and median number of steps/day
mstepcount <- daystepcount %>% ungroup() %>% 
  summarise(mean = mean(day.count, na.rm = T), median = median(day.count, na.rm = T))

mstepcount
```

```
## Source: local data frame [1 x 2]
## 
##       mean median
## 1 10766.19  10765
```
The mean total steps taken per day was 10766.19 and the median total steps taken per day was
10765. 

## What is the average daily activity pattern?

```r
# Mean step count per inteval (misc)
misc <- steps %>% group_by(interval) %>% 
  summarise(meansteps = mean(steps, na.rm = T))

maxinterval <- misc[which(misc$meansteps == max(misc$meansteps)),]
maxinterval$label <- paste('Maximum occurs at',round(maxinterval$meansteps,0),
                           'steps, at interval',maxinterval$interval)

ggplot(misc, aes(x = interval, y = meansteps)) + geom_line() + 
  geom_point(data = maxinterval, colour = 'red', size = 2) +
  geom_text(data = maxinterval, aes(x = interval + 20, label=label),
            hjust = 0, colour = 'red', size = 4) +
  ylab('Average step count') + xlab('5-min interval during day') + theme_bw()
```

![](PA1_template_files/figure-html/meansteps_interval-1.png) 
Maximum occurs at 206 steps, at interval 835

## Imputing missing values
There are 2304 missing values in the dataset.

I will impute using the rounded mean number of steps at each interval.
These values are calculated above in the variable `misc`.

```r
#Add the meansteps at each inteval to steps
steps.imputed <- left_join(steps, misc)
```

```
## Joining by: "interval"
```

```r
# If step is NA change to meansteps
steps.imputed <- steps.imputed %>% mutate(steps = ifelse(is.na(steps),round(meansteps,0),steps))
```
The new dataset is stored in `steps.imputed` 


```r
# Number of steps/day imputed
daystepcount.imp <- steps.imputed %>% group_by(date) %>% 
  summarise(day.count = sum(steps))

# Mean and median number of steps/day imputed
mstepcount.imp <- daystepcount.imp %>% ungroup() %>% 
  summarise(mean = mean(day.count, na.rm = T), median = median(day.count, na.rm = T))

ggplot(daystepcount.imp) + geom_histogram(aes(x = day.count), binwidth = 500) + 
  ggtitle('Histogram of total number of steps taken each day') + theme_bw()
```

![](PA1_template_files/figure-html/imputed_daycount-1.png) 
1.0765639\times 10^{4}, 1.0762\times 10^{4} 

## Are there differences in activity patterns between weekdays and weekends?
