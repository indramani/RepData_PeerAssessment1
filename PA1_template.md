# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Here we load activity.csv to dataframe *data*  and clean it to remove NAs.
cleaned data is stored in dataset *cleaned*. 

```r
 data <- read.csv("activity.csv")

 data$date<- as.Date(data$date)

 cleaned<-data[!is.na(data[,1]),]
```


## What is mean total number of steps taken per day?
Using cleaned data we aggregate it for each day in dataframe *aggregated_steps* 
which is used to extract vector *steps_per_day* . 
Here we plot histogram for steps per day. 
We also print summary of per day steps statistics to look at mean, median etc.

```r
 aggregated_steps<-aggregate(steps ~ date, cleaned, FUN=sum)

 steps_per_day<-aggregated_steps$steps

 hist(steps_per_day, 
	main="Steps per day using cleaned data with missing values removed", 
	xlab="steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
 summary(steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8840   10800   10800   13300   21200
```




## What is the average daily activity pattern?
Here we aggregate by 5 min time interval and take average across all the days.


```r
 aggregated_over_time<-aggregate(steps ~ interval, cleaned, FUN=mean)

 plot(aggregated_over_time$interval , aggregated_over_time$steps, type="l",
	main="Average Steps during the day", 
	xlab="5 min time interval " , ylab="Average steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


max average steps (206.1698) are taken during interval number (835) 

## Imputing missing values
We can calculate number of missing rows as follow

```r
na_count<- nrow(data[is.na(data$steps)==TRUE,])
na_count
```

```
## [1] 2304
```

I will use mean of 5 min interval to fill NA values.

```r
 completeddata<-data

 completeddata[is.na(data$steps)==TRUE,]$steps <- 
	aggregated_over_time[aggregated_over_time$interval == 
		data[is.na(data$steps)==TRUE,]$interval, ]$steps

 new_aggregated_steps<-aggregate(steps ~ date, completeddata, FUN=sum)

 hist(new_aggregated_steps$steps, 
	main="Steps per day using exprapolated data with no missing values", 
	xlab="steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
 summary(new_aggregated_steps$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8860   10800   10800   13200   21200
```

We can see that there is no change in mean and median for total number of steps per day. 
However, first and third quantile are different and very close for cleaned and
extrapolated data.


## Are there differences in activity patterns between weekdays and weekends?



```r
 wend<-as.factor(ifelse(weekdays(data$date,abbreviate=TRUE) %in% 
		c("Sat","Sun"), "weekend", "weekday"))

 data_with_wend<-cbind(data,wend)

 aggregated_over_time_for_weekend<-aggregate(steps ~ interval, 
	data_with_wend[data_with_wend$wend=="weekend",], FUN=mean)

 aggregated_over_time_for_weekday<-aggregate(steps ~ interval, 
	data_with_wend[data_with_wend$wend=="weekday",], FUN=mean)

 par(mfrow=c(2,1))

 attach(aggregated_over_time_for_weekend,warn.conflicts = FALSE)

 plot(interval,steps,main="weekend",xlab="Interval",
	ylab="Number of Steps",type="l")

 attach(aggregated_over_time_for_weekday,warn.conflicts = FALSE)

 plot(interval,steps,main="weekday",xlab="Interval",
	ylab="Number of Steps",type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


These charts clearly show that people are more active on weekdays than
on weekend.
