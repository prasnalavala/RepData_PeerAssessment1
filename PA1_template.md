---
title: "PA1_Template"
author: "MY"
date: "February 26, 2018"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    toc: yes
---




# Rep. Research Proj.1
-------------------------------------------------------

## Objective
The purpose of this project is to make use of data from a personal activity monitoring device. This device collects 5-minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. We will study this data by understanding the maximum number of steps taken, mean, median steps taken each day, and view the histogram before and after imputing missing values. 

## Data
The data was obtained from the course website <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>.
The variables included in the dataset are:
* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Reading data
Check if the project file exists, otherwise create a project file and download and unzip the compressed folder into the project file.
### 1. Code for downloading, unzipping and reading in the dataset.



```r
if (!file.exists("./Project")){
        dir.create("./Project")
        zipfileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(zipfileURL,destfile = "./Project/activity.zip")
        unzip("./Project/activity.zip", exdir="./Project")
}
data <- read.csv("./Project/activity.csv")
```

### 2. Process/transform the data (if necessary) into a format suitable for the analysis. For this we will 

```r
data$date <- as.Date(data$date)
```

### 3. What is the mean total numer of steps taken per day? For this part, we will ignore the missing values of the dataset.

        3a. Calculate the total number of steps taken per day.
        

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
total_steps <- data %>%
        group_by(date) %>%
        filter(!is.na(steps)) %>% 
        summarize(total_steps = sum(steps, na.rm=TRUE))
total_steps
```

```
## # A tibble: 53 x 2
##    date       total_steps
##    <date>           <int>
##  1 2012-10-02         126
##  2 2012-10-03       11352
##  3 2012-10-04       12116
##  4 2012-10-05       13294
##  5 2012-10-06       15420
##  6 2012-10-07       11015
##  7 2012-10-09       12811
##  8 2012-10-10        9900
##  9 2012-10-11       10304
## 10 2012-10-12       17382
## # ... with 43 more rows
```
   
        3b. Make a histogram of the total number of steps taken per day.


```r
library(lattice)
histogram(~total_steps, data=total_steps,
          main = "Lattice Histogram of Total Daily Steps",
          xlab = "Total Steps",
          ylab = "Frequency",
          col = "seagreen",
          breaks = 5)
```

![](PA1_template_files/figure-html/histogram1-1.png)<!-- -->

        3c. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps <- mean(total_steps$total_steps,na.rm=TRUE)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps <- median(total_steps$total_steps,na.rm=TRUE)
median_steps
```

```
## [1] 10765
```


### 4. What is the average daily activity pattern?


```r
Interval <- data %>%
        group_by(interval)%>%
        filter(!is.na(steps))%>%
        summarize(avg_steps = mean(steps, na.rm=TRUE))
Interval
```

```
## # A tibble: 288 x 2
##    interval avg_steps
##       <int>     <dbl>
##  1        0    1.72  
##  2        5    0.340 
##  3       10    0.132 
##  4       15    0.151 
##  5       20    0.0755
##  6       25    2.09  
##  7       30    0.528 
##  8       35    0.868 
##  9       40    0     
## 10       45    1.47  
## # ... with 278 more rows
```

        4a. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) 

```r
library(ggplot2)
ggplot(Interval, aes(x =interval , y=avg_steps)) +
        geom_line(color="chocolate2", size=1) +
        labs(title = "Average Daily Steps", x = "Interval", y = "Average Steps per day")
```

![](PA1_template_files/figure-html/intervalplot-1.png)<!-- -->
        
        4b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
Interval[which.max(Interval$avg_steps),]
```

```
## # A tibble: 1 x 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835       206
```

### 5. Imputing Missing Values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

        5a. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
data %>% count(is.na(steps))
```

```
## # A tibble: 2 x 2
##   `is.na(steps)`     n
##   <lgl>          <int>
## 1 F              15264
## 2 T               2304
```

        5b. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
        5c. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activitydata <- data
missingvalues <- is.na(activitydata$steps)
activitydata$steps[missingvalues] <- mean(activitydata$steps,na.rm=TRUE)
activitydata <- as.data.frame(activitydata)
activitydata <- activitydata[, c("date", "interval", "steps")]
head(activitydata)
```

```
##         date interval   steps
## 1 2012-10-01        0 37.3826
## 2 2012-10-01        5 37.3826
## 3 2012-10-01       10 37.3826
## 4 2012-10-01       15 37.3826
## 5 2012-10-01       20 37.3826
## 6 2012-10-01       25 37.3826
```

        5d. Make a histogram of the total number of steps taken each day. 
        

```r
library(ggplot2)
total_steps2 <- activitydata %>%
        group_by(date) %>%
        summarize(total_steps = sum(steps, na.rm=TRUE))
total_steps2
```

```
## # A tibble: 61 x 2
##    date       total_steps
##    <date>           <dbl>
##  1 2012-10-01       10766
##  2 2012-10-02         126
##  3 2012-10-03       11352
##  4 2012-10-04       12116
##  5 2012-10-05       13294
##  6 2012-10-06       15420
##  7 2012-10-07       11015
##  8 2012-10-08       10766
##  9 2012-10-09       12811
## 10 2012-10-10        9900
## # ... with 51 more rows
```

```r
ggplot(total_steps2, aes(x = total_steps)) +
        geom_histogram(fill = "indianred", binwidth = 1000) +
        labs(title = "Total Daily Steps including Missing values", y = "Interval", x = "Number of Steps")
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

        5d (cont'd). Calculate and report the **mean** and **median** total number of steps taken per day.Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
mean_steps2 <- mean(total_steps2$total_steps,na.rm=TRUE)
mean_steps2
```

```
## [1] 10766.19
```

```r
median_steps2 <- median(total_steps2$total_steps,na.rm=TRUE)
median_steps2
```

```
## [1] 10766.19
```

The impact of imputing missing data with the average number of steps in the same 5-min interval is that both the mean and the median are same : 10766.19

### 6. Are there differences in activity patterns between weekdays and weekends?
For this part we will use the weekdays() function and use the dataset with the filled-in missing values for this part.

        6a. Create a new factor variable in the dataset with two levels ??? ???weekday??? and ???weekend??? indicating whether a given date is a weekday or weekend day.

```r
head(activitydata)
```

```
##         date interval   steps
## 1 2012-10-01        0 37.3826
## 2 2012-10-01        5 37.3826
## 3 2012-10-01       10 37.3826
## 4 2012-10-01       15 37.3826
## 5 2012-10-01       20 37.3826
## 6 2012-10-01       25 37.3826
```

```r
activitydata$date<-as.Date(activitydata$date)
activitydata <- mutate(activitydata,weekday=weekdays(date))
activitydata$weekday[!activitydata$weekday=="Saturday" & !activitydata$weekday=="Sunday"] <- "Weekday"
activitydata$weekday[activitydata$weekday=="Saturday" | activitydata$weekday=="Sunday"] <- "Weekend"
head(activitydata)
```

```
##         date interval   steps weekday
## 1 2012-10-01        0 37.3826 Weekday
## 2 2012-10-01        5 37.3826 Weekday
## 3 2012-10-01       10 37.3826 Weekday
## 4 2012-10-01       15 37.3826 Weekday
## 5 2012-10-01       20 37.3826 Weekday
## 6 2012-10-01       25 37.3826 Weekday
```

        6b. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(ggplot2)
interval2 <- activitydata %>%
        group_by(interval, weekday) %>%
        summarize(avg_steps2 = mean(steps, na.rm=TRUE))
interval2
```

```
## # A tibble: 576 x 3
## # Groups: interval [?]
##    interval weekday avg_steps2
##       <int> <chr>        <dbl>
##  1        0 Weekday       7.01
##  2        0 Weekend       4.67
##  3        5 Weekday       5.38
##  4        5 Weekend       4.67
##  5       10 Weekday       5.14
##  6       10 Weekend       4.67
##  7       15 Weekday       5.16
##  8       15 Weekend       4.67
##  9       20 Weekday       5.07
## 10       20 Weekend       4.67
## # ... with 566 more rows
```

```r
plot <- ggplot(interval2, aes(x = interval, y=avg_steps2, color=weekday)) + geom_line() + labs(title = "Average Daily Steps By Weekday", y = "Number of Steps", x = "Interval") + facet_wrap(~weekday, ncol=1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/panelplot-1.png)<!-- -->
