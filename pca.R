library(lubridate)
library(ggplot2)
setwd('/home/jl/cmpt318-project')
dataSet <- read.csv("data_without_NA.txt", header = TRUE, sep = ",", dec = ".")
#dataSet <- read.csv("test1.txt", header = TRUE, sep = ",", dec = ".")
dataSet<- na.omit(dataSet)
# Set the format for the weekdays
weekdays_date = strftime(dataSet$Date, format="%u")
dataSet$weekdays_num = as.integer(weekdays_date)
weekdays = dataSet[dataSet$weekdays_num <= 5,]
weekends = dataSet[dataSet$weekdays_num > 5,]

weekdays_time = factor(weekdays$Time)
weekdays_hours = hms(as.character(weekdays_time))
weekdays$hours = as.integer(hour(weekdays_hours))

weekends_time = factor(weekends$Time)
weekends_hours = hms(as.character(weekends_time))
weekends$hours = as.integer(hour(weekends_hours))

# Allot numbers to specific weekdays
Monday = weekdays[weekdays$weekdays_num == 1,]
Tuesday = weekdays[weekdays$weekdays_num == 2,]
Wednesday = weekdays[weekdays$weekdays_num == 3,]
Thursday = weekdays[weekdays$weekdays_num == 4,]
Friday = weekdays[weekdays$weekdays_num == 5,]
Saturday = weekends[weekends$weekdays_num == 6,]
Sunday = weekends[weekends$weekdays_num == 7,]

# Categorize hours into away hours and work hours for Wednesdays and Saturdays
# Away hours: 10 am to 1 pm
# Home hours: 7 pm to 10 pm
wednesday_awayhours = Wednesday[(Wednesday$hours >=10) & (Wednesday$hours <=13),]
wednesday_homehours = Wednesday[(Wednesday$hours >=19) & (Wednesday$hours <=22),]

saturday_awayhours = Saturday[(Saturday$hours >=10) & (Saturday$hours <=13),]
saturday_homehours = Saturday[(Saturday$hours >=19) & (Saturday$hours <=22),]

get_mean<- function(data){
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  mean_active_power <- aggregate(data["Global_active_power"], by=data["Date"], mean)
  mean_reactive_power <- aggregate(data["Global_reactive_power"], by=data["Date"], mean)
  mean_voltage <- aggregate(data["Voltage"], by=data["Date"], mean)
  mean_intensity <- aggregate(data["Global_intensity"], by=data["Date"], mean)
  mean_metering_1 <- aggregate(data["Sub_metering_1"], by=data["Date"], mean)
  mean_metering_2 <- aggregate(data["Sub_metering_2"], by=data["Date"], mean)
  mean_metering_3 <- aggregate(data["Sub_metering_3"], by=data["Date"], mean)
  mean_total <- merge(mean_active_power,mean_reactive_power, by="Date")
  mean_total <- merge(mean_total,mean_voltage, by="Date")
  mean_total <- merge(mean_total, mean_intensity, by="Date")
  mean_total <- merge(mean_total, mean_metering_1, by="Date")
  mean_total <- merge(mean_total, mean_metering_2, by="Date")
  mean_total <- merge(mean_total, mean_metering_3, by="Date")
  pca<- prcomp(mean_total[,2:8] , center = TRUE,scale. = TRUE)
  return(pca)
}
pca <-get_mean(saturday_awayhours)
summary(pca)
