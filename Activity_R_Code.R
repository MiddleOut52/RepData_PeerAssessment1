library(dplyr)
library(stats)
library(ggplot2)

library(knitr)
opts_chunk$set(echo = TRUE)

setwd("~/Documents/DataScience/Reproducible_Research_Peer_Graded_Assignment/Reproducible_Research_Peer_Graded_Assignment")
Activity <- read.csv("activity.csv", stringsAsFactors = FALSE)


Activity$date <- strptime(x = as.character(Activity$date), format = "%Y-%m-%d")

Activity$date <- as.Date(Activity$date)

Date_Step <- Activity %>% group_by(date) %>% summarise(sum = sum(steps))
names(Date_Step) <- c("Date", "Steps")

hist(Date_Step$Steps, col = "orange", xlab = "Total Number of Steps", 
     main = "Total Number of Steps Taken Each Day")

Date_Step[is.na(Date_Step)] <- 0
mean(Date_Step$Steps)
median(Date_Step$Steps) 

mean_data <- Activity %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(steps = mean(steps))

g <- ggplot(mean_data, aes(interval, steps)) + geom_line(color = "orange")
g

mean_data[which.max(mean_data$steps), ]

sum(is.na(Activity$steps))

nas <- is.na(Activity$steps)

mean_interval <- tapply(Activity$steps, Activity$interval, mean, na.rm = TRUE, simplify = TRUE)

Activity$steps[nas] <- mean_interval[as.character(Activity$interval[nas])]

Date_Step <- Activity %>% group_by(date) %>% summarise(sum = sum(steps))
names(Date_Step) <- c("Date", "Steps")
        
hist(Date_Step$Steps, col = "orange", xlab = "Total Number of Steps", 
                     main = "Total Number of Steps Taken Each Day")

mean(Date_Step$Steps)
median(Date_Step$Steps) 

Activity <- mutate(Activity, weektype = ifelse(weekdays(Activity$date) == "Saturday" 
                                               | weekdays(Activity$date) == "Sunday", 
                                               "weekend", "weekday"))
Activity$weektype <- as.factor(Activity$weektype)
head(Activity)

Interval <- Activity %>%
        group_by(interval, weektype) %>%
        summarise(steps = mean(steps))
f <- ggplot(Interval, aes(x = interval, y = steps, color = weektype)) + 
        geom_line() + facet_wrap(~weektype, ncol = 1, nrow = 2)
f


