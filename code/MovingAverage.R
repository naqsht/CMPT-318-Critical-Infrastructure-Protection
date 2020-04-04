library("lubridate")
library(pracma)

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

#day time 
daytime_lower_bound<-8
daytime_upper_bound<-12
#night time
nighttime_lower_bound<-18
nighttime_upper_bound<-24


test1_time = factor(test1$Time)
hours = hms(as.character(test1_time))
test1$Time = as.integer(hour(hours))
test1_weekday_daytime<-test1[test1$Time>=daytime_lower_bound&test1$Time<daytime_upper_bound&test1$week_days_num<=5,]
test1_weekday_nighttime<-test1[test1$Time>=nighttime_lower_bound&test1$Time<nighttime_upper_bound&test1$week_days_num<=5,]
test1_weekend_daytime<-test1[test1$Time>=daytime_lower_bound&test1$Time<daytime_upper_bound&test1$week_days_num>5,]
test1_weekend_nighttime<-test1[test1$Time>=nighttime_lower_bound&test1$Time<nighttime_upper_bound&test1$week_days_num>5,]
print(test1_weekday_daytime)

test2_time = factor(test2$Time)
hours = hms(as.character(test2_time))
test2$Time = as.integer(hour(hours))
test2_weekday_daytime<-test2[test2$Time>=daytime_lower_bound&test2$Time<daytime_upper_bound&test2$week_days_num<=5,]
test2_weekday_nighttime<-test2[test2$Time>=nighttime_lower_bound&test2$Time<nighttime_upper_bound&test2$week_days_num<=5,]
test2_weekend_daytime<-test2[test2$Time>=daytime_lower_bound&test2$Time<daytime_upper_bound&test2$week_days_num>5,]
test2_weekend_nighttime<-test2[test2$Time>=nighttime_lower_bound&test2$Time<nighttime_upper_bound&test2$week_days_num>5,]


test3_time = factor(test3$Time)
hours = hms(as.character(test3_time))
test3$Time = as.integer(hour(hours))
test3_weekday_daytime<-test3[test3$Time>=daytime_lower_bound&test3$Time<daytime_upper_bound&test3$week_days_num<=5,]
test3_weekday_nighttime<-test3[test3$Time>=nighttime_lower_bound&test3$Time<nighttime_upper_bound&test3$week_days_num<=5,]
test3_weekend_daytime<-test3[test3$Time>=daytime_lower_bound&test3$Time<daytime_upper_bound&test3$week_days_num>5,]
test3_weekend_nighttime<-test3[test3$Time>=nighttime_lower_bound&test3$Time<nighttime_upper_bound&test3$week_days_num>5,]

test4_time = factor(test4$Time)
hours = hms(as.character(test4_time))
test4$Time = as.integer(hour(hours))
test4_weekday_daytime<-test4[test4$Time>=daytime_lower_bound&test4$Time<daytime_upper_bound&test4$week_days_num<=5,]
test4_weekday_nighttime<-test4[test4$Time>=nighttime_lower_bound&test4$Time<nighttime_upper_bound&test4$week_days_num<=5,]
test4_weekend_daytime<-test4[test4$Time>=daytime_lower_bound&test4$Time<daytime_upper_bound&test4$week_days_num>5,]
test4_weekend_nighttime<-test4[test4$Time>=nighttime_lower_bound&test4$Time<nighttime_upper_bound&test4$week_days_num>5,]

test5_time = factor(test5$Time)
hours = hms(as.character(test5_time))
test5$Time = as.integer(hour(hours))
test5_weekday_daytime<-test5[test5$Time>=daytime_lower_bound&test5$Time<daytime_upper_bound&test5$week_days_num<=5,]
test5_weekday_nighttime<-test5[test5$Time>=nighttime_lower_bound&test5$Time<nighttime_upper_bound&test5$week_days_num<=5,]
test5_weekend_daytime<-test5[test5$Time>=daytime_lower_bound&test5$Time<daytime_upper_bound&test5$week_days_num>5,]
test5_weekend_nighttime<-test5[test5$Time>=nighttime_lower_bound&test5$Time<nighttime_upper_bound&test5$week_days_num>5,]


#compute moving average
test1_weekday_daytime$mov_avrg=movavg(test1_weekday_daytime$Global_active_power, 10, type="s")
test1_weekday_nighttime$mov_avrg=movavg(test1_weekday_nighttime$Global_active_power, 10, type="s")
test1_weekend_daytime$mov_avrg=movavg(test1_weekend_daytime$Global_active_power, 10, type="s")
test1_weekend_nighttime$mov_avrg=movavg(test1_weekend_nighttime$Global_active_power, 10, type="s")

test2_weekday_daytime$mov_avrg=movavg(test2_weekday_daytime$Global_active_power, 10, type="s")
test2_weekday_nighttime$mov_avrg=movavg(test2_weekday_nighttime$Global_active_power, 10, type="s")
test2_weekend_daytime$mov_avrg=movavg(test2_weekend_daytime$Global_active_power, 10, type="s")
test2_weekend_nighttime$mov_avrg=movavg(test2_weekend_nighttime$Global_active_power, 10, type="s")

test3_weekday_daytime$mov_avrg=movavg(test3_weekday_daytime$Global_active_power, 10, type="s")
test3_weekday_nighttime$mov_avrg=movavg(test3_weekday_nighttime$Global_active_power, 10, type="s")
test3_weekend_daytime$mov_avrg=movavg(test3_weekend_daytime$Global_active_power, 10, type="s")
test3_weekend_nighttime$mov_avrg=movavg(test3_weekend_nighttime$Global_active_power, 10, type="s")

test4_weekday_daytime$mov_avrg=movavg(test4_weekday_daytime$Global_active_power, 10, type="s")
test4_weekday_nighttime$mov_avrg=movavg(test4_weekday_nighttime$Global_active_power, 10, type="s")
test4_weekend_daytime$mov_avrg=movavg(test4_weekend_daytime$Global_active_power, 10, type="s")
test4_weekend_nighttime$mov_avrg=movavg(test4_weekend_nighttime$Global_active_power, 10, type="s")

test5_weekday_daytime$mov_avrg=movavg(test5_weekday_daytime$Global_active_power, 10, type="s")
test5_weekday_nighttime$mov_avrg=movavg(test5_weekday_nighttime$Global_active_power, 10, type="s")
test5_weekend_daytime$mov_avrg=movavg(test5_weekend_daytime$Global_active_power, 10, type="s")
test5_weekend_nighttime$mov_avrg=movavg(test5_weekend_nighttime$Global_active_power, 10, type="s")

#moving average anomalies threshold 1: (max-min)/2 of the dataset
test1_max<-max(test1_weekday_daytime$Global_active_power)
test1_min<-min(test1_weekday_daytime$Global_active_power)

test1_weekday_daytime_anomalies_thres1<-test1_weekday_daytime[abs(test1_weekday_daytime$mov_avrg - test1_weekday_daytime$Global_active_power) >(test1_max-test1_min)/2.5, ]
test1_weekday_nighttime_anomalies_thres1<-test1_weekday_nighttime[test1_weekday_nighttime$mov_avrg - test1_weekday_nighttime$Global_active_power >(test1_max-test1_min)/2.5,]
test1_weekend_daytime_anomalies_thres1<-test1_weekend_daytime[test1_weekend_daytime$mov_avrg - test1_weekend_daytime$Global_active_power >(test1_max-test1_min)/2.5,]
test1_weekend_nighttime_anomalies_thres1<-test1_weekend_nighttime[test1_weekend_nighttime$mov_avrg - test1_weekend_nighttime$Global_active_power >(test1_max-test1_min)/2.5,]
print(test1_weekday_daytime_anomalies_thres1)

test2_max<-max(test2_weekday_daytime$Global_active_power)
test2_min<-min(test2_weekday_daytime$Global_active_power)

test2_weekday_daytime_anomalies_thres1<-test2_weekday_daytime[test2_weekday_daytime$mov_avrg - test2_weekday_daytime$Global_active_power >(test2_max-test2_min)/2.5, ]
test2_weekday_nighttime_anomalies_thres1<-test2_weekday_nighttime[test2_weekday_nighttime$mov_avrg - test2_weekday_nighttime$Global_active_power >(test2_max-test2_min)/2.5,]
test2_weekend_daytime_anomalies_thres1<-test2_weekend_daytime[test2_weekend_daytime$mov_avrg - test2_weekend_daytime$Global_active_power >(test2_max-test2_min)/2.5,]
test2_weekend_nighttime_anomalies_thres1<-test2_weekend_nighttime[test2_weekend_nighttime$mov_avrg - test2_weekend_nighttime$Global_active_power >(test2_max-test2_min)/2.5,]


test3_max<-max(test3_weekday_daytime$Global_active_power)
test3_min<-min(test3_weekday_daytime$Global_active_power)

test3_weekday_daytime_anomalies_thres1<-test3_weekday_daytime[test3_weekday_daytime$mov_avrg - test3_weekday_daytime$Global_active_power >(test3_max-test3_min)/2.5, ]
test3_weekday_nighttime_anomalies_thres1<-test3_weekday_nighttime[test3_weekday_nighttime$mov_avrg - test3_weekday_nighttime$Global_active_power >(test3_max-test3_min)/2.5,]
test3_weekend_daytime_anomalies_thres1<-test3_weekend_daytime[test3_weekend_daytime$mov_avrg - test3_weekend_daytime$Global_active_power >(test3_max-test3_min)/2.5,]
test3_weekend_nighttime_anomalies_thres1<-test3_weekend_nighttime[test3_weekend_nighttime$mov_avrg - test3_weekend_nighttime$Global_active_power >(test3_max-test3_min)/2.5,]


test4_max<-max(test4_weekday_daytime$Global_active_power)
test4_min<-min(test4_weekday_daytime$Global_active_power)

test4_weekday_daytime_anomalies_thres1<-test4_weekday_daytime[test4_weekday_daytime$mov_avrg - test4_weekday_daytime$Global_active_power >(test4_max-test4_min)/2.5, ]
test4_weekday_nighttime_anomalies_thres1<-test4_weekday_nighttime[test4_weekday_nighttime$mov_avrg - test4_weekday_nighttime$Global_active_power >(test4_max-test4_min)/2.5,]
test4_weekend_daytime_anomalies_thres1<-test4_weekend_daytime[test4_weekend_daytime$mov_avrg - test4_weekend_daytime$Global_active_power >(test4_max-test4_min)/2.5,]
test4_weekend_nighttime_anomalies_thres1<-test4_weekend_nighttime[test4_weekend_nighttime$mov_avrg - test4_weekend_nighttime$Global_active_power >(test4_max-test4_min)/2.5,]


test5_max<-max(test5_weekday_daytime$Global_active_power)
test5_min<-min(test5_weekday_daytime$Global_active_power)

test5_weekday_daytime_anomalies_thres1<-test5_weekday_daytime[test5_weekday_daytime$mov_avrg - test5_weekday_daytime$Global_active_power >(test5_max-test5_min)/2.5, ]
test5_weekday_nighttime_anomalies_thres1<-test5_weekday_nighttime[test5_weekday_nighttime$mov_avrg - test5_weekday_nighttime$Global_active_power >(test5_max-test5_min)/2.5,]
test5_weekend_daytime_anomalies_thres1<-test5_weekend_daytime[test5_weekend_daytime$mov_avrg - test5_weekend_daytime$Global_active_power >(test5_max-test5_min)/2.5,]
test5_weekend_nighttime_anomalies_thres1<-test5_weekend_nighttime[test5_weekend_nighttime$mov_avrg - test5_weekend_nighttime$Global_active_power >(test5_max-test5_min)/2.5,]


#moving average anomalies threshold 2: 3*mean of the dataset
test1_mean<-mean(test1_weekday_daytime$Global_active_power)
test1_weekday_daytime_anomalies_thres2<-test1_weekday_daytime[test1_weekday_daytime$mov_avrg - test1_weekday_daytime$Global_active_power >3*test1_mean, ]
test1_weekday_nighttime_anomalies_thres2<-test1_weekday_nighttime[test1_weekday_nighttime$mov_avrg - test1_weekday_nighttime$Global_active_power >3*test1_mean,]
test1_weekend_daytime_anomalies_thres2<-test1_weekend_daytime[test1_weekend_daytime$mov_avrg - test1_weekend_daytime$Global_active_power >3*test1_mean,]
test1_weekend_nighttime_anomalies_thres2<-test1_weekend_nighttime[test1_weekend_nighttime$mov_avrg - test1_weekend_nighttime$Global_active_power >3*test1_mean,]

test2_mean<-mean(test2_weekday_daytime$Global_active_power)
test2_weekday_daytime_anomalies_thres2<-test2_weekday_daytime[test2_weekday_daytime$mov_avrg - test2_weekday_daytime$Global_active_power >3*test2_mean, ]
test2_weekday_nighttime_anomalies_thres2<-test2_weekday_nighttime[test2_weekday_nighttime$mov_avrg - test2_weekday_nighttime$Global_active_power >3*test2_mean,]
test2_weekend_daytime_anomalies_thres2<-test2_weekend_daytime[test2_weekend_daytime$mov_avrg - test2_weekend_daytime$Global_active_power >3*test2_mean,]
test2_weekend_nighttime_anomalies_thres2<-test2_weekend_nighttime[test2_weekend_nighttime$mov_avrg - test2_weekend_nighttime$Global_active_power >3*test2_mean,]

test3_mean<-mean(test3_weekday_daytime$Global_active_power)
test3_weekday_daytime_anomalies_thres2<-test3_weekday_daytime[test3_weekday_daytime$mov_avrg - test3_weekday_daytime$Global_active_power >3*test3_mean, ]
test3_weekday_nighttime_anomalies_thres2<-test3_weekday_nighttime[test3_weekday_nighttime$mov_avrg - test3_weekday_nighttime$Global_active_power >3*test3_mean,]
test3_weekend_daytime_anomalies_thres2<-test3_weekend_daytime[test3_weekend_daytime$mov_avrg - test3_weekend_daytime$Global_active_power >3*test3_mean,]
test3_weekend_nighttime_anomalies_thres2<-test3_weekend_nighttime[test3_weekend_nighttime$mov_avrg - test3_weekend_nighttime$Global_active_power >3*test3_mean,]

test4_mean<-mean(test4_weekday_daytime$Global_active_power)
test4_weekday_daytime_anomalies_thres2<-test4_weekday_daytime[test4_weekday_daytime$mov_avrg - test4_weekday_daytime$Global_active_power >3*test4_mean, ]
test4_weekday_nighttime_anomalies_thres2<-test4_weekday_nighttime[test4_weekday_nighttime$mov_avrg - test4_weekday_nighttime$Global_active_power >3*test4_mean,]
test4_weekend_daytime_anomalies_thres2<-test4_weekend_daytime[test4_weekend_daytime$mov_avrg - test4_weekend_daytime$Global_active_power >3*test4_mean,]
test4_weekend_nighttime_anomalies_thres2<-test4_weekend_nighttime[test4_weekend_nighttime$mov_avrg - test4_weekend_nighttime$Global_active_power >3*test4_mean,]

test5_mean<-mean(test5_weekday_daytime$Global_active_power)
test5_weekday_daytime_anomalies_thres2<-test5_weekday_daytime[test5_weekday_daytime$mov_avrg - test5_weekday_daytime$Global_active_power >3*test5_mean, ]
test5_weekday_nighttime_anomalies_thres2<-test5_weekday_nighttime[test5_weekday_nighttime$mov_avrg - test5_weekday_nighttime$Global_active_power >3*test5_mean,]
test5_weekend_daytime_anomalies_thres2<-test5_weekend_daytime[test5_weekend_daytime$mov_avrg - test5_weekend_daytime$Global_active_power >3*test5_mean,]
test5_weekend_nighttime_anomalies_thres2<-test5_weekend_nighttime[test5_weekend_nighttime$mov_avrg - test5_weekend_nighttime$Global_active_power >3*test5_mean,]


#moving average anomalies threshold 3: 2*median
test1_median<-median(test1_weekday_daytime$Global_active_power)
test1_weekday_daytime_anomalies_thres3<-test1_weekday_daytime[test1_weekday_daytime$mov_avrg - test1_weekday_daytime$Global_active_power >2*test1_median, ]
test1_weekday_nighttime_anomalies_thres3<-test1_weekday_nighttime[test1_weekday_nighttime$mov_avrg - test1_weekday_nighttime$Global_active_power >2*test1_median,]
test1_weekend_daytime_anomalies_thres3<-test1_weekend_daytime[test1_weekend_daytime$mov_avrg - test1_weekend_daytime$Global_active_power >2*test1_median,]
test1_weekend_nighttime_anomalies_thres3<-test1_weekend_nighttime[test1_weekend_nighttime$mov_avrg - test1_weekend_nighttime$Global_active_power >2*test1_median,]

test2_median<-median(test2_weekday_daytime$Global_active_power)
test2_weekday_daytime_anomalies_thres3<-test2_weekday_daytime[test2_weekday_daytime$mov_avrg - test2_weekday_daytime$Global_active_power >2*test2_median, ]
test2_weekday_nighttime_anomalies_thres3<-test2_weekday_nighttime[test2_weekday_nighttime$mov_avrg - test2_weekday_nighttime$Global_active_power >2*test2_median,]
test2_weekend_daytime_anomalies_thres3<-test2_weekend_daytime[test2_weekend_daytime$mov_avrg - test2_weekend_daytime$Global_active_power >2*test2_median,]
test2_weekend_nighttime_anomalies_thres3<-test2_weekend_nighttime[test2_weekend_nighttime$mov_avrg - test2_weekend_nighttime$Global_active_power >2*test2_median,]

test3_median<-median(test3_weekday_daytime$Global_active_power)
test3_weekday_daytime_anomalies_thres3<-test3_weekday_daytime[test3_weekday_daytime$mov_avrg - test3_weekday_daytime$Global_active_power >2*test3_median, ]
test3_weekday_nighttime_anomalies_thres3<-test3_weekday_nighttime[test3_weekday_nighttime$mov_avrg - test3_weekday_nighttime$Global_active_power >2*test3_median,]
test3_weekend_daytime_anomalies_thres3<-test3_weekend_daytime[test3_weekend_daytime$mov_avrg - test3_weekend_daytime$Global_active_power >2*test3_median,]
test3_weekend_nighttime_anomalies_thres3<-test3_weekend_nighttime[test3_weekend_nighttime$mov_avrg - test3_weekend_nighttime$Global_active_power >2*test3_median,]

test4_median<-median(test4_weekday_daytime$Global_active_power)
test4_weekday_daytime_anomalies_thres3<-test4_weekday_daytime[test4_weekday_daytime$mov_avrg - test4_weekday_daytime$Global_active_power >2*test4_median, ]
test4_weekday_nighttime_anomalies_thres3<-test4_weekday_nighttime[test4_weekday_nighttime$mov_avrg - test4_weekday_nighttime$Global_active_power >2*test4_median,]
test4_weekend_daytime_anomalies_thres3<-test4_weekend_daytime[test4_weekend_daytime$mov_avrg - test4_weekend_daytime$Global_active_power >2*test4_median,]
test4_weekend_nighttime_anomalies_thres3<-test4_weekend_nighttime[test4_weekend_nighttime$mov_avrg - test4_weekend_nighttime$Global_active_power >2*test4_median,]

test5_median<-median(test5_weekday_daytime$Global_active_power)
test5_weekday_daytime_anomalies_thres3<-test5_weekday_daytime[test5_weekday_daytime$mov_avrg - test5_weekday_daytime$Global_active_power >2*test5_median, ]
test5_weekday_nighttime_anomalies_thres3<-test5_weekday_nighttime[test5_weekday_nighttime$mov_avrg - test5_weekday_nighttime$Global_active_power >2*test5_median,]
test5_weekend_daytime_anomalies_thres3<-test5_weekend_daytime[test5_weekend_daytime$mov_avrg - test5_weekend_daytime$Global_active_power >2*test5_median,]
test5_weekend_nighttime_anomalies_thres3<-test5_weekend_nighttime[test5_weekend_nighttime$mov_avrg - test5_weekend_nighttime$Global_active_power >2*test5_median,]

