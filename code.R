library(tidyverse)
t1<-read.csv("activity.csv")

sum(t1$steps, na.rm = T)
smry<-tapply(t1$steps, t1$date, sum, na.rm=TRUE)
mean(smry, na.rm=TRUE)
median(smry, na.rm=TRUE)
smrytbl<-as.data.frame(smry)
smrytbl %>% ggplot(aes(smry)) + geom_histogram(bins=20)

tsplot<-t1 %>% group_by(interval) %>% summarise(mean=mean(steps,na.rm = T)) 
tsplot %>% ggplot(aes(interval, mean))+geom_line()
tsplot$interval[which.max(tsplot$mean)]

sum(is.na(t1$steps))
t2<-(merge(tsplot, t1))
t2<-t2[order(t2$date), ]
t2$steps[is.na(t2$steps)]<-t2$mean[is.na(t2$steps)]

smry2<-tapply(t2$steps, t2$date, sum, na.rm=TRUE)
mean(smry2, na.rm=TRUE)
median(smry2, na.rm=TRUE)
smrytbl2<-as.data.frame(smry)
smrytbl2 %>% ggplot(aes(smry)) + geom_histogram(bins=20)

###mean(t2$steps)
###median(t2$steps)
###sum(t2$steps)

t3<-t2
t3$date<-as.Date(t3$date)
t3$weekdays<-weekdays(t3$date)
t3$ends<-factor(t3$weekdays)
levels(t3$ends)<-c("Weekday", "Weekday", "Weekend", "Weekend", "Weekday", "Weekday", "Weekday")

t3plot<-t3 %>% group_by(interval, ends)%>%summarise(mean=mean(steps))
t3plot %>% ggplot(aes(interval, mean))+geom_line()+facet_wrap(~ends)
