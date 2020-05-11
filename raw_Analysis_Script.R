#This is a dirty script. I recommend not to use. 

library(dplyr)
library(lubridate)
library(ggplot2)
library(xtable)

amd <- read.csv(unz('./data/repdata_data_activity.zip', filename = 'activity.csv'), 
                stringsAsFactors = F, header=T, na.strings = 'NA')

amd$date <- ymd(amd$date)

#Question 1: Total number of steps taken in each day

total_steps_day <- amd %>% group_by(date) %>% summarize(Total_steps=sum(steps, na.rm=T))
ggplot(total_steps_day, aes(Total_steps)) + geom_histogram(bins=20)


f <- amd %>% group_by(interval) %>% summarise(avg_steps=mean(steps, na.rm=T))
ggplot(f, aes(interval, avg_steps)) + geom_line() + ylab('Average steps taken') + 
    xlab('Intervals')

g <- amd %>% group_by(interval) %>% summarize(avg_step=mean(steps, na.rm=T))
amd$interval[which(g$avg_step==max(g$avg_step))]


sum(is.na(amd))

#imputing missing values in steps column
#After calculating the mean of steps across each day, some days have really low values
#Most days have values ranging from 25 to 50
#Remove rows of days whose mean values are very low
#Calculate the mean of the rest
#replace NA with that mean
q <- amd %>% group_by(date) %>% summarise(missing=mean(steps))
x <- q[-c(1,8,2,25,32,35,39,40,41,45,46,61), ]

imputeValue <- floor(mean(x$missing))

amd$steps <- replace(amd$steps, is.na(amd$steps), imputeValue)


total_steps_day <- amd %>% group_by(date) %>% summarize(Total_steps=sum(steps, na.rm=T))
ggplot(total_steps_day, aes(Total_steps)) + geom_histogram(binwidth=1000)

floor(mean(total_steps_day$Total_steps))

amd$weekSig <- weekdays(amd$date)

amd[amd$weekSig!='Saturday' & amd$weekSig!='Sunday', ]$weekSig <- 'weekday'
amd[amd$weekSig=='Saturday' | amd$weekSig=='Sunday', ]$weekSig <- 'weekend'

amd$weekSig <- as.factor(amd$weekSig)

f <- amd %>% group_by(weekSig, interval) %>% summarise(avg_steps=mean(steps, na.rm=T))

ggplot(f, aes(interval, avg_steps)) + geom_line() + 
    ylab('Average steps taken') + xlab('Intervals') +
    facet_grid(weekSig~.)

head(amd)
total_steps_day <- amd %>% group_by(date, interval) %>% summarize(Total_steps=mean(steps, na.rm=T))
