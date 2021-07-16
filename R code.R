# Week 2 Peer review assessment
library(dplyr)

getwd()
setwd("C:/Users/lamti/Desktop/datasciencecoursera/Course 5 Reproducible Research/Week 2/peer review assessment/RepData_PeerAssessment1")


#Question 1

data <- read.csv("activity.csv", header = TRUE)
head(data)
nrow(data)
str(data)

#Question 1 Calculate the total number of steps taken per day
total_step <- sum(data$steps, na.rm = TRUE)


#Question 2 Histogram of the total number of steps taken each day
total_step <- data %>%
            group_by(date)%>%
            summarise(sum_each_day = sum(steps, na.rm=TRUE))


hist(total_step$sum_each_day, xlab = "Total number of steps each day", main = "Histogram of steps perday")


#Question 3 Calculatethe mean and median of the total number of steps taken per day
mean_t_step <- mean(total_step$sum_each_day, na.rm = TRUE)
median_t_step <- median(total_step$sum_each_day, na.rm = TRUE)

mean_t_step
median_t_step


#Question 4 Make a time series plot

clean <- filter(data, !is.na(steps))
clean
tidy <- aggregate(steps~interval, clean, mean)
tidy
plot(tidy$interval, tidy$steps, type = "l", xlab = "Time", ylab = "Steps", main = "Average steps per time interval")



#Question 5 
str(tidy)
tidy2<- tidy%>% 
    rename(Time_interval = interval)

max_step <- filter(tidy, steps==max(steps))
max_step



#Question 6  Calculate and report the total number of missing values in the dataset

miss <- sum(is.na(data))
miss

#Question 7 filling in all missing values

fill_na <- data %>% 
            mutate(steps = case_when(is.na(steps) ~ tidy$steps[match(
              data$interval, tidy$interval)], TRUE ~ as.numeric(steps)
            ))

fill_na



#Question 7 






