# Week 2 Peer review assessment
library(dplyr)
library(lubridate)
library(ggplot2)
library(knitr)

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

data_fill <- data %>% 
            mutate(steps = case_when(is.na(steps) ~ tidy$steps[match(
              data$interval, tidy$interval)], TRUE ~ as.numeric(steps)
            ))

data_fill



#Question 7 

data_fill_sum <- data_fill%>%
                group_by(date)%>%
                  summarise(sum = sum(steps))
data_fill_sum


hist(data_fill_sum$sum, xlab = "Total number of steps each day", 
     main = "Historgram of total number of steps each day", breaks = 10)


mean_fill_step <- mean(data_fill_sum$sum, na.rm = TRUE)
median_fill_step <- median(data_fill_sum$sum, na.rm = TRUE)

mean_dif <- mean_fill_step - mean_t_step
mean_dif
median_dif <- median_fill_step - median_t_step
median_dif

#Both estimates on the mean and median of the total number of steps taken per day
#are higher than that of the first part of the assignment. 

#The number of mean and median total increased after imputing missing data on the estimates of the total daily number of steps




#Part 4 Are there differences in activity patterns between weekdays and weekends?


data_fill_date <- data_fill



day <- data_fill_date %>% 
          mutate(date = ymd(date),
                  day = case_when(wday(date)%in% 2:6 ~ "Weekday", 
                                  wday(date) %in% c(1,7) ~ "Weekend")
          )

colnames(day)
mean_by_day <- day%>% 
                  group_by(interval, day)%>%
                    summarise(steps = mean(steps))
mean_by_day

ggplot(mean_by_day, aes(x = interval, y = steps))+
  labs(x = "Time (5 mins intervals)", y = "Average number of steps",
       title = "Average number of steps every 5 mins on weekday and weekend")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~day, nrow = 2)+
  geom_line()




    
    