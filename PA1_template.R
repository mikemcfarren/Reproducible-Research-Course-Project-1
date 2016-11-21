setwd("E:/Coursera/Reproducible-Research-Course-Project-1")

datafile <- "activity.csv"

if(!file.exists(datafile)) {
    temp <- tempfile()
    url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, temp)
    unzip(temp)
    unlink(temp)
}

stepsDataSet <- read.csv(datafile, header = T, sep = ",")

steps.bydate.sum <- aggregate(x = stepsDataSet[c("steps")], 
                              FUN = sum, 
                              by = list(date = stepsDataSet$date), 
                              na.rm = T) 

png(filename = "dailystepshistogram.png")

hist(steps.bydate.sum$steps, 
     main = "Daily Total Number of Steps", 
     xlab = "Number of Steps", 
     col = "red")

abline(v = mean(steps.bydate.sum$steps), lty = 1, lwd = 2, col = "green")

abline(v = median(steps.bydate.sum$steps), lty = 2, lwd = 2, col = "black")

legend (x = "topright", 
        c("mean", "median"), 
        col = c("green", "black"),
        lty = c(1,2), 
        lwd = c(2, 2))

dev.off()

meanSteps <- mean(steps.bydate.sum$steps)
meanSteps

medianSteps <- median(steps.bydate.sum$steps)
medianSteps


## summary(steps.bydate.sum)


steps.byinterval.mean <- aggregate(x = stepsDataSet[c("steps")], 
                                   FUN = mean, 
                                   by = list(interval = stepsDataSet$interval), 
                                   na.rm = T) 


png(filename = "dailystepsaveragelineplot.png")

plot(steps.byinterval.mean$interval, 
     steps.byinterval.mean$steps, 
     main = "Daily Average Number of Steps by Interval", 
     xlab = "Interval", 
     ylab = "Number of Steps",
     type="l")

dev.off()

intervalWithMostAvgSteps = steps.byinterval.mean[which.max(steps.byinterval.mean$steps),]
intervalWithMostAvgSteps



# Imputing missing values
# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(stepsDataSet$steps))

## use a function to return the mean for the given interval
UpdateMissingIntervals <- function(steps, interval) {
    
    filled <- NA
    
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- steps.byinterval.mean[steps.byinterval.mean$interval == interval, "steps"];
    
    return (filled);
}

## copy the original dataset to imputedDataSet and perform a multi-variate apply, applying 
##  the UpdateMissingIntervals function above
imputedDataSet <- stepsDataSet
imputedDataSet$steps <- mapply(UpdateMissingIntervals, imputedDataSet$steps, imputedDataSet$interval)


imputedSteps.bydate.sum <- aggregate(x = imputedDataSet[c("steps")], 
                              FUN = sum, 
                              by = list(date = imputedDataSet$date), 
                              na.rm = T) 


png(filename = "dailystepshistogram-imputed.png")

hist(imputedSteps.bydate.sum$steps, 
     main = "Daily Total Number of Steps", 
     xlab = "Number of Steps", 
     col = "red")

abline(v = mean(imputedSteps.bydate.sum$steps), lty = 1, lwd = 2, col = "green")

abline(v = median(imputedSteps.bydate.sum$steps), lty = 2, lwd = 2, col = "black")

legend (x = "topright", 
        c("mean", "median"), 
        col = c("green", "black"),
        lty = c(1,2), 
        lwd = c(2, 2))

dev.off()


meanImputedSteps <- mean(imputedSteps.bydate.sum$steps)
meanImputedSteps

medianImputedSteps <- median(imputedSteps.bydate.sum$steps)
medianImputedSteps



median(steps.bydate.sum$steps)

## Use a function to determine if the day of week is a 'weekday' or a 'weekend'
GetDayType <- function(date) {
    dayofweek <- weekdays(date)
    
    if (dayofweek %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else 
        return("weekend")
}

## apply the function above to the imputed dataset and add this result to our dataset
imputedDataSet$daytype <- sapply(as.Date(imputedDataSet$date), FUN=GetDayType)

summary(imputedDataSet)



library(ggplot2)

imputedSteps.bydaytype.mean  <- aggregate(steps ~ interval + daytype, data = imputedDataSet, mean)

png(filename = "dailystepsaveragelineplot-bydaytype.png")

ggplot(imputedSteps.bydaytype.mean, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(daytype ~ .) + ## Looks better stacked vertically 
    ggtitle("Daily Average Number of Steps by Interval") +
    xlab("Interval") + 
    ylab("Number of Steps")

dev.off()


rm(list=ls())
