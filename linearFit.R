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
data = saturday_awayhours

get_linearPolyFit <- function(data){
  time <- factor(data$Time)
  count <- 1
  index <- 1
  meanActive <- 0
  meanReactive <- 0
  meanVoltage <- 0
  meanIntensity <- 0
  meanMeter1 <- 0
  meanMeter2 <- 0
  meanMeter3 <-0
  size <- nlevels(time)
  while(count <= size){
    currentTime <- levels(time)[count]
    d<- subset(data, Time == currentTime)
    count<- count+1
    meanActive[index] <- mean(d$Global_active_power)
    meanReactive[index] <- mean(d$Global_reactive_power)
    meanVoltage[index] <- mean(d$Voltage)
    meanIntensity[index] <- mean(d$Global_intensity)
    meanMeter1[index] <- mean(d$Sub_metering_1)
    meanMeter2[index] <- mean(d$Sub_metering_2)
    meanMeter3[index] <-mean(d$Sub_metering_3)
    index <-index+1
  }
  ##
  dataMean <- data.frame("Global_active_power" = meanActive, "Time" = levels(awayTime))
  
  fit_linear <- lm(formula=  Global_active_power ~ as.numeric(Time), data = dataMean )
  fit_polynomial <- lm(Global_active_power~poly( as.numeric(Time),2,raw=TRUE), data= dataMean )
  prediction_linear <- predict(fit_linear, data=dataMean$Global_active_power)
  prediction_polynomial <- predict(fit_polynomial, data=dataMean$Global_active_power)
  dataMean$prediction_polynomial <- prediction_polynomial
  dataMean$prediction_linear <- prediction_linear
  
  
  p1 <-ggplot(data=dataMean , mapping=aes(x=Time, y=Global_active_power, group = 1)) +
    geom_point(color="black") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="red") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Orange") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="Green") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Yellow")
  ##
  dataMean <- data.frame("Global_reactive_power" = meanReactive, "Time" = levels(awayTime))
  
  fit_linear <- lm(formula=  Global_reactive_power ~ as.numeric(Time), data = dataMean )
  fit_polynomial <- lm(Global_reactive_power~poly( as.numeric(Time),2,raw=TRUE), data= dataMean )
  prediction_linear <- predict(fit_linear, data=dataMean$Global_reactive_power)
  prediction_polynomial <- predict(fit_polynomial, data=dataMean$Global_reactive_power)
  dataMean$prediction_polynomial <- prediction_polynomial
  dataMean$prediction_linear <- prediction_linear
  
  
  p2 <-ggplot(data=dataMean , mapping=aes(x=Time, y=Global_reactive_power, group = 1)) +
    geom_point(color="black") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="red") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Orange") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="Green") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Yellow")
  ##
  dataMean <- data.frame("Voltage" = meanVoltage, "Time" = levels(awayTime))
  
  fit_linear <- lm(formula=  Voltage ~ as.numeric(Time), data = dataMean )
  fit_polynomial <- lm(Voltage~poly( as.numeric(Time),2,raw=TRUE), data= dataMean )
  prediction_linear <- predict(fit_linear, data=dataMean$Voltage)
  prediction_polynomial <- predict(fit_polynomial, data=dataMean$Voltage)
  dataMean$prediction_polynomial <- prediction_polynomial
  dataMean$prediction_linear <- prediction_linear
  
  
  p3 <-ggplot(data=dataMean , mapping=aes(x=Time, y=Voltage, group = 1)) +
    geom_point(color="black") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="red") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Orange") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="Green") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Yellow")
  ##
  dataMean <- data.frame("Global_intensity" = meanIntensity, "Time" = levels(awayTime))
  
  fit_linear <- lm(formula=  Global_intensity ~ as.numeric(Time), data = dataMean )
  fit_polynomial <- lm(Global_intensity~poly( as.numeric(Time),2,raw=TRUE), data= dataMean )
  prediction_linear <- predict(fit_linear, data=dataMean$Global_intensity)
  prediction_polynomial <- predict(fit_polynomial, data=dataMean$Global_intensity)
  dataMean$prediction_polynomial <- prediction_polynomial
  dataMean$prediction_linear <- prediction_linear
  
  p4 <-ggplot(data=dataMean , mapping=aes(x=Time, y=Global_intensity, group = 1)) +
    geom_point(color="black") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="red") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Orange") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="Green") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Yellow") 
  ##
  dataMean <- data.frame("Sub_metering_1" = meanMeter1, "Time" = levels(awayTime))
  
  fit_linear <- lm(formula=  Sub_metering_1 ~ as.numeric(Time), data = dataMean )
  fit_polynomial <- lm(Sub_metering_1~poly( as.numeric(Time),2,raw=TRUE), data= dataMean )
  prediction_linear <- predict(fit_linear, data=dataMean$Sub_metering_1)
  prediction_polynomial <- predict(fit_polynomial, data=dataMean$Sub_metering_1)
  dataMean$prediction_polynomial <- prediction_polynomial
  dataMean$prediction_linear <- prediction_linear
  
  p5 <-ggplot(data=dataMean , mapping=aes(x=Time, y=Sub_metering_1, group = 1)) +
    geom_point(color="black") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="red") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Orange") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="Green") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Yellow") 
  ##
  dataMean <- data.frame("Sub_metering_2" = meanMeter2, "Time" = levels(awayTime))
  
  fit_linear <- lm(formula=  Sub_metering_2 ~ as.numeric(Time), data = dataMean )
  fit_polynomial <- lm(Sub_metering_2~poly( as.numeric(Time),2,raw=TRUE), data= dataMean )
  prediction_linear <- predict(fit_linear, data=dataMean$Sub_metering_2)
  prediction_polynomial <- predict(fit_polynomial, data=dataMean$Sub_metering_2)
  dataMean$prediction_polynomial <- prediction_polynomial
  dataMean$prediction_linear <- prediction_linear
  
  p6 <-ggplot(data=dataMean , mapping=aes(x=Time, y=Sub_metering_2, group = 1)) +
    geom_point(color="black") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="red") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Orange") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="Green") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Yellow") 
  ##
  dataMean <- data.frame("Sub_metering_3" = meanMeter3, "Time" = levels(awayTime))
  
  fit_linear <- lm(formula=  Sub_metering_3 ~ as.numeric(Time), data = dataMean )
  fit_polynomial <- lm(Sub_metering_3~poly( as.numeric(Time),2,raw=TRUE), data= dataMean )
  prediction_linear <- predict(fit_linear, data=dataMean$Sub_metering_3)
  prediction_polynomial <- predict(fit_polynomial, data=dataMean$Sub_metering_3)
  dataMean$prediction_polynomial <- prediction_polynomial
  dataMean$prediction_linear <- prediction_linear
  
  p7 <-ggplot(data=dataMean , mapping=aes(x=Time, y=Sub_metering_3, group = 1)) +
    geom_point(color="black") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="red") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Orange") +
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_linear), size=1, color="Green") + 
    geom_line(data=dataMean , mapping=aes(x=Time, y=prediction_polynomial), size=1, color="Yellow") 
  returnList = list(p1,p2,p3,p4,p5,p6,p7)
  return(returnList)
}

plots = get_linearPolyFit(wednesday_homehours)
print(plots[1])
print(plots[2])
print(plots[3])
print(plots[4])
print(plots[5])
print(plots[6])
print(plots[7])
