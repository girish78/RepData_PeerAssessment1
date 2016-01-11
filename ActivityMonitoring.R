##############Personal activity monitoring Data Assignment 1##############################################
##########################################################################################################
echo = TRUE

library(dplyr)
library(gdata)
library(lattice)
library(data.table)
library(timeDate)
################ Unzip data file #########################################################################

unzip("C:/Users/girish/Documents/data/repdata-data-activity.zip", exdir = "C:/Users/girish/Documents/data")

###########################Load Unzipped Data using read.csv command #####################################

PAD <- read.csv("C:/Users/girish/Documents/data/activity.csv")

###########################Clean Data to remove NA value Rows from the data###############################
###########################Function complete.cases used to clean data#####################################
PAD1 <- PAD[complete.cases(PAD),]

###########################Convert clean data to a Data Table so as to get a sum of steps taken per day###
PAD1 <- data.table(PAD1)
PAD2 <- PAD1[,sum(steps), by = date]
###########################Covert to data frame to give proper column names
PAD3 <- data.frame(PAD2)
colnames(PAD3) <- c("date", "steps")

####### Start png device driver to save output to file.png#################################################
#######Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels##
png(file = "./data/Histogram_Steps_Taken_Per_Day.png", width = 480, height = 480)
#####################Create a Histogram for Steps taken per day ###########################################
#####################Format the Title name and the x axes name to present proper naming####################
hist(PAD3$steps, main = paste("Histogram of Steps Taken Per Day"),xlab = ("Steps Taken Per Day"))
dev.off()
hist(PAD3$steps, main = paste("Histogram of Steps Taken Per Day"),xlab = ("Steps Taken Per Day"))
#####################Calculate the Mean & median of the ##################################################
ActivityMean <- mean(PAD3$steps)
ActivityMedian <- median(PAD3$steps)
ActivityMean
ActivityMedian
##########################################################################################################

PAD4 <- group_by(PAD1,date)
PAD5 <- mutate(PAD4,average = mean(steps))
for (i in 2:15264) {
PAD5[i,3] <- 5 * (i -1)}

png(file = "./data/Average Daily Activity Pattern.png", width = 480, height = 480)
plot(PAD5$interval, PAD5$average,type = "l",col="red", ann=FALSE)
title(main="Average Daily Activity Pattern", col.main="blue", font.main = 4)
title(xlab="Interval 5 Minutes")
title(ylab="Average Steps Taken Per Day")
dev.off()

plot(PAD5$interval, PAD5$average,type = "l",col="red", ann=FALSE)
title(main="Average Daily Activity Pattern", col.main="blue", font.main = 4)
title(xlab="Interval 5 Minutes")
title(ylab="Average Steps Taken Per Day")

PAD6 <- mutate(PAD, na = is.na(steps))

Total_NA_Steps <- sum(PAD6$na)

PAD6$steps[is.na(PAD6$steps)] <- 0 

PAD7 <- data.table(PAD6)
PAD7 <- PAD7[,sum(steps), by = date]
colnames(PAD7) <- c("date", "steps")

png(file = "./data/Histogram_Steps_Taken_Per_Day_1.png", width = 480, height = 480)
hist(PAD7$steps, main = paste("Histogram of Steps Taken Per Day"),xlab = ("Steps Taken Per Day"))
dev.off()

hist(PAD7$steps, main = paste("Histogram of Steps Taken Per Day"),xlab = ("Steps Taken Per Day"))

ActivityMean_1 <- mean(PAD7$steps)
ActivityMedian_1 <- median(PAD7$steps)
ActivityMean_1
ActivityMedian_1

Weekday <- isWeekday(PAD6$date, wday=1:5)
PAD8 <- cbind(PAD6, Weekday)

PAD8 <- group_by(PAD8,date)
PAD9 <- mutate(PAD8,average = mean(steps))
PAD9 <- data.table(PAD9)

for (i in 2:17568) {
  PAD9[i,3] <- 5 * (i -1)}

for (i in 1:17568) {
  if (PAD9$Weekday[i] == TRUE){PAD9$Weekday[i] = "weekday"}}
for (i in 1:17568) {
  if (PAD9$Weekday[i] == FALSE){PAD9$Weekday[i] = "weekend"}}

png(file = "./data/Average Daily Activity Pattern_2.png", width = 480, height = 480)
xyplot(average ~ interval| Weekday, data = PAD9, type = "l",xlab = "Interval",ylab = "Number of steps",layout=c(1,2))
dev.off()

xyplot(average ~ interval| Weekday, data = PAD9, type = "l",xlab = "Interval",ylab = "Number of steps",layout=c(1,2))







