---
title: "Reproducible research"
author: "Nicholas"
date: "`r Sys.Date()`"
output: html_document
---


```{r}
Activity <- read.csv("activity.csv")
library(tidyverse)
Activity <-  as_tibble(Activity)
```
What is mean total number of steps taken per day?
```{r}

Total_step_day<-Activity %>%group_by(date) %>%  summarise(steps=sum(steps,na.rm = TRUE))
png("plot11.png",width = 480,height=480)
hist(Total_step_day$steps,breaks = 100)
dev.off()
mean(Total_step_day$steps)

median(Total_step_day$steps)
```
What is the average daily activity pattern?
```{r}
Complete_Activity_int<-Activity %>%group_by(interval) %>% summarize(steps=mean(steps,na.rm=TRUE))
png("plot12.png",width = 480,height=480)
with(Complete_Activity_int,plot(interval,steps,type="l"))
dev.off()
Complete_Activity_int %>%group_by(interval) %>%arrange(-steps) %>% filter(steps==max(Complete_Activity_int$steps))

```
THe 835 interval has the highest average steps

Imputing missing values
```{r}
Activity %>%count(steps %in% NA)
```
 2304 missing values
 
 filling in all of the missing values with the mean
```{r}
mean_activity<- mean(Activity$steps,na.rm=TRUE)
Activity2<-Activity %>% mutate(steps=ifelse(steps %in% NA,mean_activity,steps))

Total_step_day2<-Activity2 %>% group_by(date) %>% summarise(steps=sum(steps))
png("plot13.png",width = 480,height=480)
hist(Total_step_day2$steps,breaks = 100)
dev.off()
mean(Total_step_day2$steps)
median(Total_step_day2$steps)
```

 
Are there differences in activity patterns between weekdays and weekends?

```{r}
Activity2$date<- as.Date(Activity2$date)

Activity_week<- Activity2 %>% mutate(date=if_else((wday(Activity2$date) %in% 2:6),"Weekday","Weekend"))

Activity_week$date<- as.factor(Activity_week$date)

Activity_weekdays<-Activity_week %>%filter(date=="Weekday") %>% group_by(interval) %>%  summarise(steps=mean(steps))

Activity_weekends<-Activity_week %>% filter(date=="Weekend") %>%  group_by(interval) %>% summarise(steps=mean(steps))

png("plot14.png",width = 480,height=480)
plot(Activity_weekdays$interval,Activity_weekdays$steps,type = "l",xlab="interval",ylab="Average steps")
lines(Activity_weekends$interval,Activity_weekends$steps,col="red")
legend("topleft",legend=c("Weekday","Weekend"),
col=c("black","red"),lty=1)
dev.off()
```



 









