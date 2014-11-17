#Reproducible Research
#Assignment 1
#Sean Jackson

#load libraries
library('plyr')
library('reshape2')
library('data.table')

#load data
#download zip file
temp <- tempfile()
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, temp)
#copy data into variable
data <- read.csv(unz(temp, "activity.csv"))
unlink(temp)


#clean data
#calculate mean total steps taken per day
#calculate median total steps taken per day
cleanData <- ddply(data, .(date), summarize,
                   MeanDailySteps = mean(steps, na.rm=T),
                   MedianDailySteps = median(steps, na.rm=T),
                   TotalSteps = sum(steps, na.rm=T))

intervalData <- ddply(data,.(interval), summarize,
                      MeanSteps = mean(steps, na.rm=T),
                      MedianSteps = median(steps, na.rm=T),
                      TotalSteps = sum(steps, na.rm=T))

#create histogram of total steps taken per day
hist(cleanData$TotalSteps,
     col="Red",
     xlab="Total Steps per Day",
     main="Total Steps per Day")


#make time series plot of of 5 min interval and avg steps taken
#across all days
#ggplot(data, aes(interval, steps), rm.na=T) + geom_line() +
#  scale_x_continuous()

ggplot(intervalData, aes(interval, MeanSteps), rm.na=T) + geom_line()

#question: which 5 min interval across all days contains max steps
which.max(intervalData$TotalSteps)

#calculate and report total number of missing values
sum(is.na(data$steps))

#create new dataset with filled in missing values
avgData <- data
avgData[is.na(avgData)]<- 0 


#create histogram of total steps per day
cleanAvgData <- ddply(avgData, .(date), summarize,
                   MeanDailySteps = mean(steps, na.rm=T),
                   MedianDailySteps = median(steps, na.rm=T),
                   TotalSteps = sum(steps, na.rm=T))

#calculate mean total steps/day
cleanAvgData$TotalSteps

#calculate median total steps/day
cleanAvgData$MedianSteps

#question: do the imputed missing values affect the data?
#question: what is the impact of the imputed values on the data?


#question: is there a measureable difference between weekday
#and weekend activity?