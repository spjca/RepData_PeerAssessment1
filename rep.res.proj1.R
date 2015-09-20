#Reproducible Research Assignment 1

#load libraries
library(Hmisc) #to extract and impute values over NA values

#load data
temp <- unzip("repdata-data-activity.zip")
activity.data <- read.csv(temp, stringsAsFactors = FALSE)
str(activity.data)

#date data is not properly formatted

#change to date format
activity.data$date <- as.Date(activity.data$date)
str(activity.data)

#check steps for NA values
sum(is.na(activity.data$steps))

#2304 NA values

#use Hmisc to view breakdown of NA values by interval
activity.steps.Breakdown <- bystats(activity.data$steps, activity.data$interval, 
                                 fun=function(x)c(Mean=mean(x),Median=median(x)))

#appears all instances have 8 NA values and the majority
#of instances have a median of 0. Instead of removing the NA
#values, I will impute the median value of the non NA step instances
#into the NA values using a function

#create function to impute missing values based on median of comparable interval
#input function
impute.Median <- function(impute.var, filter.var, var.levels){
  #for each v variable in list of inputs var.levels
  for(v in var.levels){ 
    #impute the median steps of each interval to the respective 
    #interval with missing values in the step index
    impute.var[which(filter.var == v)] <- impute((impute.var[ 
      which(filter.var == v)]), fun=mean)
  }
  return(impute.var)
}

#impute missing steps with medians using above function for activity data
activity.data$steps <- impute.Median(activity.data$steps,             #steps
                                     activity.data$interval,          #intervals
                                     unique(activity.data$interval))  #unique interval values

#check steps for NA values
sum(is.na(activity.data$steps))

#0 NA values, success. With the NA values removed, some questions can
#be answered

#What is the mean total number of steps taken per day?

#divide up the daily data
activity.byday <- tapply(activity.data$steps, activity.data$date, sum)

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**
##Now that the NA values have been imputed, create a variable of the divided up daily data using tapply and create a histogram of data

activity.byday <- tapply(activity.data$steps, activity.data$date, sum)
hist(activity.byday, 30, main = "Imputed Total Steps/Day", xlab = "")
mean(activity.byday)
median(activity.byday)

#Are there differences in activity patterns between weekdays and weekends?

#For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.**
  
#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**

#create variable for day of week using weekdays function on date index
activity.data$day<-weekdays(activity.data$date)
#create weekend/weekday variable 
activity.data$weekday.weekend<- as.factor(c("weekend", "weekday"))
#populate weekend/weekday variable based on is or is not saturday/sunday
activity.data[activity.data$day == "Sunday" | activity.data$day == "Saturday" ,5]<- factor("weekend")
activity.data[!(activity.data$day == "Sunday" | activity.data$day == "Saturday"),5 ]<- factor("weekday")

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data

#create dataset of weekend and weekday data respectively
activity.data.weekend <- subset(activity.data, weekday.weekend == "weekend")
activity.data.weekday <- subset(activity.data, weekday.weekend == "weekday")

#create datasets for weekdays and weekend of the mean daily usage 
activity.weekend.daily <- tapply(activity.data.weekend$steps, activity.data.weekend$interval, mean)
activity.weekday.daily <- tapply(activity.data.weekday$steps, activity.data.weekday$interval, mean)

#plot the datasets
plot(y = activity.weekend.daily, x = names(activity.weekend.daily), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekdays", ylab = "Average number of steps", 
     ylim =c(0, 250))
plot(y = activity.weekday.daily, x = names(activity.weekday.daily), type = "l", xlab = "5-Minute Interval", 
     main = "Daily Activity Pattern on Weekdays", ylab = "Average number of steps", 
     ylim =c(0, 250))


