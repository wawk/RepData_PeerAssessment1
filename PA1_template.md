---
title: "Reproducible Research Assignment 1"
author: "Steven Moody"
date: "Saturday, February 27, 2016"
output: html_document
---

### Loading and Processing the Data

```r
library(plyr)
library(stats)
```

```r
df <- read.table("./activity.csv",header=T,sep=",",numerals = "no.loss")
df2 <- ddply(df,~date,summarize,sum=sum(steps),mean=mean(steps),median=median(steps))
```
### **What is mean total number of steps taken per day?**

```r
hist(df2$sum, main="Histogram of Total Steps Taken Each Day", xlab = "Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

#### Results showing the sum, mean and median for steps grouped by Date (using head and tail of data.frame)


```
##          date   sum     mean median
## 1  2012-10-01    NA       NA     NA
## 2  2012-10-02   126  0.43750      0
## 3  2012-10-03 11352 39.41667      0
## 4  2012-10-04 12116 42.06944      0
## 5  2012-10-05 13294 46.15972      0
## 6  2012-10-06 15420 53.54167      0
## 7  2012-10-07 11015 38.24653      0
## 8  2012-10-08    NA       NA     NA
## 9  2012-10-09 12811 44.48264      0
## 10 2012-10-10  9900 34.37500      0
```

```
##          date   sum     mean median
## 52 2012-11-21 12787 44.39931      0
## 53 2012-11-22 20427 70.92708      0
## 54 2012-11-23 21194 73.59028      0
## 55 2012-11-24 14478 50.27083      0
## 56 2012-11-25 11834 41.09028      0
## 57 2012-11-26 11162 38.75694      0
## 58 2012-11-27 13646 47.38194      0
## 59 2012-11-28 10183 35.35764      0
## 60 2012-11-29  7047 24.46875      0
## 61 2012-11-30    NA       NA     NA
```

### **What is the average daily activity pattern?**

```r
stepinterval <- aggregate(steps ~ interval, data=df, FUN=mean)
plot(stepinterval, type="l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

### **Which 5 minute interval, on average across all the days in the dataset, contains the maximum number of steps?**

```r
stepinterval[which.max(stepinterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```
Imputing missing values
Calculate and report the total number of missing values in the dataset(i.e. the total number of rows with NAs)

```r
sum(is.na(df))
```

```
## [1] 2304
```

### **Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day or the mean for that 5 minut interval, etc.**

#### **Answer:** I will use the rounded mean value for the appropriate interval corresponding to the one with the NA in steps.

### ** Create a new dataset that is equal to the original dataset but with the missing data filled in.**

```r
df3 <- df
any(is.na(df))
```

```
## [1] TRUE
```

```r
indexes = which(is.na(df3))
for (indx in indexes) {
        intrvl <- df3[indx,]$interval
        stepVal <- stepinterval[stepinterval$interval == intrvl,]$steps
        df3[indx,]$steps <- stepVal
        
}
```
#### Proof that there are no longer any NA's in the Steps column

```r
any(is.na(df$steps))
```

```
## [1] TRUE
```
### **Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**

#### **Answer:** There are slight changes to the data as can be evidenced by the comparitive histograms but sum doesnt seem to show the change in a dramatic fasion.


```r
df4 <- ddply(df3,~date,summarize,sum=sum(steps),mean=mean(steps),median=median(steps))
par(mfrow = c(1, 2))
hist(df4$sum, main = "Histogram (With NAs)" ,xlab="Total Steps taken each Day")
hist(df2$sum, main = "Histogram (NAs filled)",xlab="Total Steps taken each Day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

#### The dataset after NAs have been filled in for Steps


```
##          date      sum     mean   median
## 1  2012-10-01 10766.19 37.38260 34.11321
## 2  2012-10-02   126.00  0.43750  0.00000
## 3  2012-10-03 11352.00 39.41667  0.00000
## 4  2012-10-04 12116.00 42.06944  0.00000
## 5  2012-10-05 13294.00 46.15972  0.00000
## 6  2012-10-06 15420.00 53.54167  0.00000
## 7  2012-10-07 11015.00 38.24653  0.00000
## 8  2012-10-08 10766.19 37.38260 34.11321
## 9  2012-10-09 12811.00 44.48264  0.00000
## 10 2012-10-10  9900.00 34.37500  0.00000
```

```
##          date      sum     mean   median
## 52 2012-11-21 12787.00 44.39931  0.00000
## 53 2012-11-22 20427.00 70.92708  0.00000
## 54 2012-11-23 21194.00 73.59028  0.00000
## 55 2012-11-24 14478.00 50.27083  0.00000
## 56 2012-11-25 11834.00 41.09028  0.00000
## 57 2012-11-26 11162.00 38.75694  0.00000
## 58 2012-11-27 13646.00 47.38194  0.00000
## 59 2012-11-28 10183.00 35.35764  0.00000
## 60 2012-11-29  7047.00 24.46875  0.00000
## 61 2012-11-30 10766.19 37.38260 34.11321
```
### **Are there differences in activity patterns between weekdays and weekends?**

#### **Answer:** Yes according to the graphs the most activity appears to occurr on the weekend, which makes
#### sense since this is likely the time when most people will have the available time for activities.


```r
df3$date <- as.Date(as.character(df3$date))
df3$dayofweek <- ifelse(weekdays(df3$date) %in% c("Saturday","Sunday"),"weekend","weekday")
df3$dayofweek <- factor(df3$dayofweek)
```


```r
par(mfrow = c(2, 1))
for (dow in c("weekend", "weekday")) {
    dowsteps <- aggregate(steps ~ interval, data = df3, subset = df3$dayofweek == 
        dow, FUN = mean)
    plot(dowsteps, type = "l", main = dow)
}
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
