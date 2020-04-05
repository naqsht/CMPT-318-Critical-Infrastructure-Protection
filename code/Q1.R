setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security\\cmpt318-project\\plot data')
consumption_data <- read.csv("TrainData.txt", header = TRUE, sep = ",", dec = ".")
consumption_data$Date<-as.Date(consumption_data$Date,format='%d/%m/%Y')
#choose 2006/12/16 Saturday as weekend
weekend<-consumption_data[consumption_data$Date == as.Date('2006/12/16'),]
write.table(weekend, "one_random_weekend_.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

#choose 2006/12/16 Monday as weekday
weekday<-consumption_data[consumption_data$Date == as.Date('2006/12/18'),]
write.table(weekday, "one_random_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

train_consumption_data<-consumption_data
weekdays_date = strftime(train_consumption_data$Date, format="%u")
train_consumption_data$weekdays_num = as.integer(weekdays_date)

train_data_spring_weekday<-train_consumption_data[((train_consumption_data$Date>=as.Date('2007/03/01')&train_consumption_data$Date<=as.Date('2007/05/31'))|
                                                     (train_consumption_data$Date>=as.Date('2008/03/01')&train_consumption_data$Date<=as.Date('2008/05/31')))&
                                                    train_consumption_data$weekdays_num<=5,]



train_data_summer_weekday<-train_consumption_data[((train_consumption_data$Date>=as.Date('2007/06/01')&train_consumption_data$Date<=as.Date('2007/08/31'))|
                                                     (train_consumption_data$Date>=as.Date('2008/06/01')&train_consumption_data$Date<=as.Date('2008/08/31')))&
                                                    train_consumption_data$weekdays_num<=5,]


train_data_autumn_weekday<-train_consumption_data[((train_consumption_data$Date>=as.Date('2007/09/01')&train_consumption_data$Date<=as.Date('2007/11/30'))|
                                                     (train_consumption_data$Date>=as.Date('2008/09/01')&train_consumption_data$Date<=as.Date('2008/11/30')))&
                                                    train_consumption_data$weekdays_num<=5,]


train_data_wintor_weekday<-train_consumption_data[((train_consumption_data$Date>=as.Date('2006/12/01')&train_consumption_data$Date<=as.Date('2007/02/28'))|
                                                     (train_consumption_data$Date>=as.Date('2007/12/01')&train_consumption_data$Date<=as.Date('2008/02/29')))&
                                                    train_consumption_data$weekdays_num<=5,]

write.table(train_data_spring_weekday, "train_data_spring_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

write.table(train_data_summer_weekday, "train_data_summer_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

write.table(train_data_autumn_weekday, "train_data_autumn_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

write.table(train_data_wintor_weekday, "train_data_wintor_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

