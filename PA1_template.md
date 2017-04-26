---
title: "PA1_template"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Reproducible Research Peer Graded Assignment 1

## Introduction

The code presented in this document analyzes the data from a single person's activity monitor. The data collected the number of steps taken in 5 minute intervals from October to November.

## Library

```{r}
library(dplyr)
library(stats)
library(ggplot2)
library(knitr)
opts_chunk$set(echo = TRUE)
```

## Loading the Dataset

```{r}
setwd("~/Documents/DataScience/Reproducible_Research_Peer_Graded_Assignment/Reproducible_Research_Peer_Graded_Assignment")
Activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

## Cleaning Up the Data

```{r}
Activity$date <- strptime(x = as.character(Activity$date), format = "%Y-%m-%d")
Activity$date <- as.Date(Activity$date)
```

This puts the dates in a form that is ready for analysis.

## What is the mean total number of steps taken per day?

```{r}
Date_Step <- Activity %>% group_by(date) %>% summarise(sum = sum(steps))
names(Date_Step) <- c("Date", "Steps")

Date_Step[is.na(Date_Step)] <- 0
mean(Date_Step$Steps)
median(Date_Step$Steps) 
```

Date_Step is a data frame that shows the number of steps taken on individual days.  This is then used in order to print the output of the mean and median steps taken per day.

```{r}
hist(Date_Step$Steps, col = "orange", xlab = "Total Number of Steps", 
     main = "Total Number of Steps Taken Each Day")
```

The above histogram represents the total number of steps taken each day.

## What is the average daily activity pattern?

```{r}
mean_data <- Activity %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(steps = mean(steps))

```

This creates a data fram that contains the average daily activity, and it disregards the NA values.

```{r}
g <- ggplot(mean_data, aes(interval, steps)) + geom_line(color = "orange")
g
```

This plot shows the average steps taken in each 5 minute interval.

```{r}
mean_data[which.max(mean_data$steps), ]
```

This calculation shows the day in which the highest average number of steps was taken.

## Imputing Missing Values

```{r}
sum(is.na(Activity$steps))
```

The total number of NA values is 2304.

```{r}
nas <- is.na(Activity$steps)
mean_interval <- tapply(Activity$steps, Activity$interval, mean, na.rm = TRUE, simplify = TRUE)
Activity$steps[nas] <- mean_interval[as.character(Activity$interval[nas])]
```

This code replaces all of the NA values with the mean steps taken, and creates a data set with the results.

```{r}
Date_Step <- Activity %>% group_by(date) %>% summarise(sum = sum(steps))
names(Date_Step) <- c("Date", "Steps")
        
hist(Date_Step$Steps, col = "orange", xlab = "Total Number of Steps", 
                     main = "Total Number of Steps Taken Each Day")
```

This code groups the data by the day in which it occured, and then it produces a histogram that shows the total number of steps taken each day.  This histogram is very similar to the first one.  The only difference is that a couple of the peaks are now higher.

```{r}
mean(Date_Step$Steps)
median(Date_Step$Steps) 
```

The result of removing the NA values is that the mean and median both equal 10766.19

## Are there differences in activity patterns between weekends and weekdays?

```{r}
Activity <- mutate(Activity, weektype = ifelse(weekdays(Activity$date) == "Saturday" 
                                               | weekdays(Activity$date) == "Sunday", 
                                               "weekend", "weekday"))
Activity$weektype <- as.factor(Activity$weektype)
```

Dplyr and weekdays are used in order to distinguish between different days of the week.  These values are then converted into factors.

```{r}
Interval <- Activity %>%
        group_by(interval, weektype) %>%
        summarise(steps = mean(steps))
f <- ggplot(Interval, aes(x = interval, y = steps, color = weektype)) + 
        geom_line() + facet_wrap(~weektype, ncol = 1, nrow = 2)
f
```

The Activity data is grouped by weektype in order to assist in creating the resulting graphics.  These graphs show that the person is active earlier in the day during the week, but is usually more active on the weekends.

