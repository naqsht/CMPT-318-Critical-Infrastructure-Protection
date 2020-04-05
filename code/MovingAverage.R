library("lubridate")
library(pracma)
library(ggplot2)

setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security\\cmpt318-project')
test1 <- read.csv("test1.txt", header = TRUE, sep = ",", dec = ".")
test2 <- read.csv("test2.txt", header = TRUE, sep = ",", dec = ".")
test3 <- read.csv("test3.txt", header = TRUE, sep = ",", dec = ".")
test4 <- read.csv("test4.txt", header = TRUE, sep = ",", dec = ".")
test5 <- read.csv("test5.txt", header = TRUE, sep = ",", dec = ".")

#pick a random week 2010-03-01 to 2010-03-07
test1 = test1[as.Date(test1$Date, format = '%d/%m/%Y') >= as.Date('2010/03/01')&as.Date(test1$Date, format = '%d/%m/%Y') <= as.Date('2010/03/07'),]
test2 = test2[as.Date(test2$Date, format = '%d/%m/%Y') >= as.Date('2010/03/01')&as.Date(test2$Date, format = '%d/%m/%Y') <= as.Date('2010/03/07'),]
test3 = test3[as.Date(test3$Date, format = '%d/%m/%Y') >= as.Date('2010/03/01')&as.Date(test3$Date, format = '%d/%m/%Y') <= as.Date('2010/03/07'),]
test4 = test4[as.Date(test4$Date, format = '%d/%m/%Y') >= as.Date('2010/03/01')&as.Date(test4$Date, format = '%d/%m/%Y') <= as.Date('2010/03/07'),]
test5 = test5[as.Date(test5$Date, format = '%d/%m/%Y') >= as.Date('2010/03/01')&as.Date(test5$Date, format = '%d/%m/%Y') <= as.Date('2010/03/07'),]
#seperate weekday and weekend
week_days_date = strftime(test1$Date, format="%u")
test1$week_days_num = as.integer(week_days_date)

week_days_date = strftime(test2$Date, format="%u")
test2$week_days_num = as.integer(week_days_date)

week_days_date = strftime(test3$Date, format="%u")
test3$week_days_num = as.integer(week_days_date)

week_days_date = strftime(test4$Date, format="%u")
test4$week_days_num = as.integer(week_days_date)

week_days_date = strftime(test5$Date, format="%u")
test5$week_days_num = as.integer(week_days_date)





test1_weekday<-test1[test1$week_days_num<=5,]
test1_weekend<-test1[test1$week_days_num>5,]


test2_weekday<-test2[test2$week_days_num<=5,]
test2_weekend<-test2[test2$week_days_num>5,]



test3_weekday<-test3[test3$week_days_num<=5,]
test3_weekend<-test3[test3$week_days_num>5,]


test4_weekday<-test4[test4$week_days_num<=5,]
test4_weekend<-test4[test4$week_days_num>5,]


test5_weekday<-test5[test5$week_days_num<=5,]
test5_weekend<-test5[test5$week_days_num>5,]


#compute moving average
test1_weekday$mov_avrg=movavg(test1_weekday$Global_active_power, 10, type="s")
test1_weekend$mov_avrg=movavg(test1_weekend$Global_active_power, 10, type="s")

test2_weekday$mov_avrg=movavg(test2_weekday$Global_active_power, 10, type="s")
test2_weekend$mov_avrg=movavg(test2_weekend$Global_active_power, 10, type="s")

test3_weekday$mov_avrg=movavg(test3_weekday$Global_active_power, 10, type="s")
test3_weekend$mov_avrg=movavg(test3_weekend$Global_active_power, 10, type="s")

test4_weekday$mov_avrg=movavg(test4_weekday$Global_active_power, 10, type="s")
test4_weekend$mov_avrg=movavg(test4_weekend$Global_active_power, 10, type="s")

test5_weekday$mov_avrg=movavg(test5_weekday$Global_active_power, 10, type="s")
test5_weekend$mov_avrg=movavg(test5_weekend$Global_active_power, 10, type="s")

#moving average anomalies threshold 1: (max-min)/2.5 of the dataset
test1_weekday_max<-max(test1_weekday$Global_active_power)
test1_weekday_min<-min(test1_weekday$Global_active_power)
test1_weekend_max<-max(test1_weekend$Global_active_power)
test1_weekend_min<-min(test1_weekend$Global_active_power)
test1_weekday_anomalies_thres1<-test1_weekday[abs(test1_weekday$mov_avrg - test1_weekday$Global_active_power )>(test1_weekday_max-test1_weekday_min)/2.5, ]
test1_weekend_anomalies_thres1<-test1_weekend[abs(test1_weekend$mov_avrg - test1_weekend$Global_active_power )>(test1_weekend_max-test1_weekend_min)/2.5,]

test2_weekday_max<-max(test2_weekday$Global_active_power)
test2_weekday_min<-min(test2_weekday$Global_active_power)
test2_weekend_max<-max(test2_weekend$Global_active_power)
test2_weekend_min<-min(test2_weekend$Global_active_power)
test2_weekday_anomalies_thres1<-test2_weekday[abs(test2_weekday$mov_avrg - test2_weekday$Global_active_power )>(test2_weekday_max-test2_weekday_min)/2.5, ]
test2_weekend_anomalies_thres1<-test2_weekend[abs(test2_weekend$mov_avrg - test2_weekend$Global_active_power )>(test2_weekend_max-test2_weekend_min)/2.5,]

test3_weekday_max<-max(test3_weekday$Global_active_power)
test3_weekday_min<-min(test3_weekday$Global_active_power)
test3_weekend_max<-max(test3_weekend$Global_active_power)
test3_weekend_min<-min(test3_weekend$Global_active_power)
test3_weekday_anomalies_thres1<-test3_weekday[abs(test3_weekday$mov_avrg - test3_weekday$Global_active_power )>(test3_weekday_max-test3_weekday_min)/2.5, ]
test3_weekend_anomalies_thres1<-test3_weekend[abs(test3_weekend$mov_avrg - test3_weekend$Global_active_power )>(test3_weekend_max-test3_weekend_min)/2.5,]

test4_weekday_max<-max(test4_weekday$Global_active_power)
test4_weekday_min<-min(test4_weekday$Global_active_power)
test4_weekend_max<-max(test4_weekend$Global_active_power)
test4_weekend_min<-min(test4_weekend$Global_active_power)
test4_weekday_anomalies_thres1<-test4_weekday[abs(test4_weekday$mov_avrg - test4_weekday$Global_active_power )>(test4_weekday_max-test4_weekday_min)/2.5, ]
test4_weekend_anomalies_thres1<-test4_weekend[abs(test4_weekend$mov_avrg - test4_weekend$Global_active_power )>(test4_weekend_max-test4_weekend_min)/2.5,]

test5_weekday_max<-max(test5_weekday$Global_active_power)
test5_weekday_min<-min(test5_weekday$Global_active_power)
test5_weekend_max<-max(test5_weekend$Global_active_power)
test5_weekend_min<-min(test5_weekend$Global_active_power)
test5_weekday_anomalies_thres1<-test5_weekday[abs(test5_weekday$mov_avrg - test5_weekday$Global_active_power )>(test5_weekday_max-test5_weekday_min)/2.5, ]
test5_weekend_anomalies_thres1<-test5_weekend[abs(test5_weekend$mov_avrg - test5_weekend$Global_active_power )>(test5_weekend_max-test5_weekend_min)/2.5,]



#moving average anomalies threshold 2: 3*mean of the dataset
test1_weekday_mean<-mean(test1_weekday$Global_active_power)
test1_weekend_mean<-mean(test1_weekend$Global_active_power)
test1_weekday_anomalies_thres2<-test1_weekday[abs(test1_weekday$mov_avrg - test1_weekday$Global_active_power )>3*test1_weekday_mean, ]
test1_weekend_anomalies_thres2<-test1_weekend[abs(test1_weekend$mov_avrg - test1_weekend$Global_active_power )>3*test1_weekend_mean,]

test2_weekday_mean<-mean(test2_weekday$Global_active_power)
test2_weekend_mean<-mean(test2_weekend$Global_active_power)
test2_weekday_anomalies_thres2<-test2_weekday[abs(test2_weekday$mov_avrg - test2_weekday$Global_active_power )>3*test2_weekday_mean, ]
test2_weekend_anomalies_thres2<-test2_weekend[abs(test2_weekend$mov_avrg - test2_weekend$Global_active_power )>3*test2_weekend_mean,]

test3_weekday_mean<-mean(test3_weekday$Global_active_power)
test3_weekend_mean<-mean(test3_weekend$Global_active_power)
test3_weekday_anomalies_thres2<-test3_weekday[abs(test3_weekday$mov_avrg - test3_weekday$Global_active_power )>3*test3_weekday_mean, ]
test3_weekend_anomalies_thres2<-test3_weekend[abs(test3_weekend$mov_avrg - test3_weekend$Global_active_power )>3*test3_weekend_mean,]

test4_weekday_mean<-mean(test4_weekday$Global_active_power)
test4_weekend_mean<-mean(test4_weekend$Global_active_power)
test4_weekday_anomalies_thres2<-test4_weekday[abs(test4_weekday$mov_avrg - test4_weekday$Global_active_power )>3*test4_weekday_mean, ]
test4_weekend_anomalies_thres2<-test4_weekend[abs(test4_weekend$mov_avrg - test4_weekend$Global_active_power )>3*test4_weekend_mean,]

test5_weekday_mean<-mean(test5_weekday$Global_active_power)
test5_weekend_mean<-mean(test5_weekend$Global_active_power)
test5_weekday_anomalies_thres2<-test5_weekday[abs(test5_weekday$mov_avrg - test5_weekday$Global_active_power )>3*test5_weekday_mean, ]
test5_weekend_anomalies_thres2<-test5_weekend[abs(test5_weekend$mov_avrg - test5_weekend$Global_active_power )>3*test5_weekend_mean,]
#moving average anomalies threshold 3: 3*median

test1_weekday_median<-median(test1_weekday$Global_active_power)
test1_weekend_median<-median(test1_weekend$Global_active_power)
test1_weekday_anomalies_thres3<-test1_weekday[abs(test1_weekday$mov_avrg - test1_weekday$Global_active_power )>3*test1_weekday_median, ]
test1_weekend_anomalies_thres3<-test1_weekend[abs(test1_weekend$mov_avrg - test1_weekend$Global_active_power )>3*test1_weekend_median,]


test2_weekday_median<-median(test2_weekday$Global_active_power)
test2_weekend_median<-median(test2_weekend$Global_active_power)
test2_weekday_anomalies_thres3<-test2_weekday[abs(test2_weekday$mov_avrg - test2_weekday$Global_active_power )>3*test2_weekday_median, ]
test2_weekend_anomalies_thres3<-test2_weekend[abs(test2_weekend$mov_avrg - test2_weekend$Global_active_power )>3*test2_weekend_median,]


test3_weekday_median<-median(test3_weekday$Global_active_power)
test3_weekend_median<-median(test3_weekend$Global_active_power)
test3_weekday_anomalies_thres3<-test3_weekday[abs(test3_weekday$mov_avrg - test3_weekday$Global_active_power )>3*test3_weekday_median, ]
test3_weekend_anomalies_thres3<-test3_weekend[abs(test3_weekend$mov_avrg - test3_weekend$Global_active_power )>3*test3_weekend_median,]


test4_weekday_median<-median(test4_weekday$Global_active_power)
test4_weekend_median<-median(test4_weekend$Global_active_power)
test4_weekday_anomalies_thres3<-test4_weekday[abs(test4_weekday$mov_avrg - test4_weekday$Global_active_power )>3*test4_weekday_median, ]
test4_weekend_anomalies_thres3<-test4_weekend[abs(test4_weekend$mov_avrg - test4_weekend$Global_active_power )>3*test4_weekend_median,]


test5_weekday_median<-median(test5_weekday$Global_active_power)
test5_weekend_median<-median(test5_weekend$Global_active_power)
test5_weekday_anomalies_thres3<-test5_weekday[abs(test5_weekday$mov_avrg - test5_weekday$Global_active_power )>3*test5_weekday_median, ]
test5_weekend_anomalies_thres3<-test5_weekend[abs(test5_weekend$mov_avrg - test5_weekend$Global_active_power )>3*test5_weekend_median,]














print(nrow(test1_weekday_anomalies_thres1))
print(nrow(test1_weekend_anomalies_thres1))
print(nrow(test2_weekday_anomalies_thres1))
print(nrow(test2_weekend_anomalies_thres1))
print(nrow(test3_weekday_anomalies_thres1))
print(nrow(test3_weekend_anomalies_thres1))
print(nrow(test4_weekday_anomalies_thres1))
print(nrow(test4_weekend_anomalies_thres1))
print(nrow(test5_weekday_anomalies_thres1))
print(nrow(test5_weekend_anomalies_thres1))

print(nrow(test1_weekday_anomalies_thres2))
print(nrow(test1_weekend_anomalies_thres2))
print(nrow(test2_weekday_anomalies_thres2))
print(nrow(test2_weekend_anomalies_thres2))
print(nrow(test3_weekday_anomalies_thres2))
print(nrow(test3_weekend_anomalies_thres2))
print(nrow(test4_weekday_anomalies_thres2))
print(nrow(test4_weekend_anomalies_thres2))
print(nrow(test5_weekday_anomalies_thres2))
print(nrow(test5_weekend_anomalies_thres2))

print(nrow(test1_weekday_anomalies_thres3))
print(nrow(test1_weekend_anomalies_thres3))
print(nrow(test2_weekday_anomalies_thres3))
print(nrow(test2_weekend_anomalies_thres3))
print(nrow(test3_weekday_anomalies_thres3))
print(nrow(test3_weekend_anomalies_thres3))
print(nrow(test4_weekday_anomalies_thres3))
print(nrow(test4_weekend_anomalies_thres3))
print(nrow(test5_weekday_anomalies_thres3))
print(nrow(test5_weekend_anomalies_thres3))

test1_weekday$diff = abs(test1_weekday$mov_avrg - test1_weekday$Global_active_power )
#test 1 weekday
ggplot(test1_weekday, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test1_weekday_max-test1_weekday_min)/2.5, color = 'yellow')+
geom_hline(yintercept = 3*test1_weekday_mean, color = 'blue')+
geom_hline(yintercept = 3*test1_weekday_median, color = 'red')

ggplot(test1_weekend, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test1_weekend_max-test1_weekend_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test1_weekend_mean, color = 'blue')+
  geom_hline(yintercept = 3*test1_weekend_median, color = 'red')


#test 2 weekday 
#test 2 weekend
ggplot(test2_weekday, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test2_weekday_max-test2_weekday_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test2_weekday_mean, color = 'blue')+
  geom_hline(yintercept = 3*test2_weekday_median, color = 'red')

ggplot(test2_weekend, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test2_weekend_max-test2_weekend_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test2_weekend_mean, color = 'blue')+
  geom_hline(yintercept = 3*test2_weekend_median, color = 'red')
#test 3 weekday 
#test 3 weekend
ggplot(test3_weekday, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test3_weekday_max-test3_weekday_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test3_weekday_mean, color = 'blue')+
  geom_hline(yintercept = 3*test3_weekday_median, color = 'red')

ggplot(test3_weekend, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test3_weekend_max-test3_weekend_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test3_weekend_mean, color = 'blue')+
  geom_hline(yintercept = 3*test3_weekend_median, color = 'red')

#test 4 weekday 
#test 4 weekend
ggplot(test4_weekday, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test4_weekday_max-test4_weekday_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test4_weekday_mean, color = 'blue')+
  geom_hline(yintercept = 3*test4_weekday_median, color = 'red')

ggplot(test4_weekend, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test4_weekend_max-test4_weekend_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test4_weekend_mean, color = 'blue')+
  geom_hline(yintercept = 3*test4_weekend_median, color = 'red')
#test 5 weekday 
#test 5 weekend
ggplot(test5_weekday, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test5_weekday_max-test5_weekday_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test5_weekday_mean, color = 'blue')+
  geom_hline(yintercept = 3*test5_weekday_median, color = 'red')

ggplot(test5_weekend, aes(x = Time, y = diff))+geom_point(color="grey")+
  geom_hline(yintercept = (test5_weekend_max-test5_weekend_min)/2.5, color = 'yellow')+
  geom_hline(yintercept = 3*test5_weekend_mean, color = 'blue')+
  geom_hline(yintercept = 3*test5_weekend_median, color = 'red')

