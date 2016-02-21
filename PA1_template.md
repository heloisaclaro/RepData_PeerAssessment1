######First Set Working Directory##########
setwd("C:/Users/DiogoNina/Desktop")

######Read data from assignment1######
peerassignment1 <- read.csv("activity.csv", as.is = TRUE)

######Process/transform the data (if necessary) into a format suitable for your analysis (remove the missing data)#####

nomissing_peerassignment1 <- peerassignment1[complete.cases(peerassignment1), ]

######1 Calculate the total number of steps taken per day#####
dailysteps <- aggregate(steps ~ date, nomissing_peerassignment1, sum)
View(dailysteps)

###### Make a histogram of the total number of steps taken each day########
plot1 <- hist(dailysteps$steps, main = "Graph number of steps taken each day", xlab = "Daily steps")
print(plot1)

###### Calculate and report the mean and median of the total number of steps taken per day#######

round(mean(dailysteps$steps))
[1] 10766

median(dailysteps$steps)
[1] 10765

######Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)######
mean_steps_interval <- aggregate(steps ~ interval, nomissing_peerassignment1, mean)
mean_daily_steps <- aggregate(steps ~ date, nomissing_peerassignment1, mean)

plot2 <- plot(mean_steps_interval$interval, mean_steps_interval$steps, type='l', col=1, main="MEan number of steps for Interval", xlab="Intervals of time", ylab="Mean number of steps")
print(plot2)

###### Answer question Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?######
which_interval <- which.max(mean_steps_interval$steps)
print (paste("This is the interval with maximun number of steps", mean_steps_interval[which_interval, ]$interval, " the number of steps for this interval: ", round(mean_steps_interval[which_interval, ]$steps, digits = 1)))

######Imputing missing values######

######Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)######

missing_values_calculation <- peerassignment1[!complete.cases(peerassignment1), ]

nrow(missing_values_calculation)
Devise a strategy for filling in all of the missing values in the dataset

######Devise a strategy for filling in all of the missing values in the dataset#######
###### Search in each row for NA in number of steps, find the interval for this specific row, find the mean for that interval in in the dataset mean_steps_interval and replace the NA value for this specific mean value#######
###### Search in each row for NA in number of steps, find the interval for this specific row, find the mean for that interval in in the dataset mean_steps_interval and replace the NA value for this specific mean value#######

for (i in 1:nrow(peerassignment1)) {
  if(is.na(peerassignment1$steps[i])) {
    val <- mean_steps_interval$steps[which(mean_steps_interval$interval == peerassignment1$interval[i])]
    peerassignment1$steps[i] <- val 
  }
}

####### Create a new dataset that is equal to the original dataset but with the missing data filled in.#######
daily_steps_missing_replaced <- aggregate(steps ~ date, peerassignment1, sum)


####### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?#####

plot3 <- hist(daily_steps_missing_replaced$steps, main = "Histogram Graph of number of daily steps with the missing data replaced", xlab = "Daily Steps")
print(plot3)

#####Mean######

round(mean(daily_steps_missing_replaced$steps))
[1] 10766

####Median####
median(daily_steps_missing_replaced$steps)
[1] 10766.19

#####Are there differences in activity patterns between weekdays and weekends?#####



#####Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.#####


week_day <- function(date_val) {
  wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
  if  (!(wd == 'Saturday' || wd == 'Sunday')) {
    x <- 'Weekday'
  } else {
    x <- 'Weekend'
  }
  x
}

peerassignment1$kind_day <- as.factor(sapply(peerassignment1$date, week_day))

#load the ggplot library
library(ggplot2)

#####Create aggragated dataframe by interval and kind of day (weekends or weekdays)####

daily_steps_missing_replaced <- aggregate(steps ~ interval+kind_day, peerassignment1, mean)

plot4 <- ggplot(daily_steps_missing_replaced, aes(interval, steps)) +    geom_line(stat = "identity", aes(colour = kind_day)) +
  theme_gray() +
  facet_grid(kind_day ~ ., scales="fixed", space="fixed") +
  labs(x="Time interval", y=expression("Steps #")) +
  ggtitle("Steps # Interval by kind of day")
print(plot4)
