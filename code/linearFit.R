library(lubridate)
library(ggplot2)
setwd('/home/jl/cmpt318-project')
setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security')

dataSet <- read.csv("data_without_NA.txt", header = FALSE, sep = ",", dec = ".")
#dataSet <- read.csv("test1.txt", header = TRUE, sep = ",", dec = ".")
dataSet<- na.omit(dataSet)
autumnDataset <- read.csv("plot data/train_data_autumn_weekday.txt",header= TRUE)
autumnDataset <- na.omit(autumnDataset)

springDataset <- read.csv("plot data/train_data_spring_weekday.txt",header= TRUE)
springDataset <- na.omit(springDataset)

summerDataset <- read.csv("plot data/train_data_summer_weekday.txt",header= TRUE)
summerDataset <- na.omit(summerDataset)

winterDataset <- read.csv("plot data/train_data_wintor_weekday.txt",header= TRUE)
winterDataset <- na.omit(winterDataset)

weekend <- read.csv("plot data/one_random_weekend_.txt",header= TRUE)
weekend <- na.omit(weekend)
weekday <- read.csv("plot data/one_random_weekday.txt",header= TRUE)
weekday <- na.omit(weekday)
awayTime <- factor(autumnDataset$Time)


keeps<-c(1,121,241,361,481,601,721,841,961,1081,1201,1321,1440) # Indices for levels you want to show

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
    geom_smooth(color="Blue") +
    scale_x_discrete(
      breaks=levels(awayTime)[keeps],
      labels=levels(awayTime)[keeps])
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
    geom_smooth(color="Blue") +
    scale_x_discrete(
      breaks=levels(awayTime)[keeps],
      labels=levels(awayTime)[keeps])
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
    geom_smooth(color="Blue") +
    scale_x_discrete(
      breaks=levels(awayTime)[keeps],
      labels=levels(awayTime)[keeps])
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
    geom_smooth(color="Blue") +
    scale_x_discrete(
      breaks=levels(awayTime)[keeps],
      labels=levels(awayTime)[keeps])
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
    geom_smooth(color="Blue") +
    scale_x_discrete(
      breaks=levels(awayTime)[keeps],
      labels=levels(awayTime)[keeps])
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
    geom_smooth(color="Blue") +
    scale_x_discrete(
      breaks=levels(awayTime)[keeps],
      labels=levels(awayTime)[keeps])
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
    geom_smooth(color="Blue") +
    scale_x_discrete(
      breaks=levels(awayTime)[keeps],
      labels=levels(awayTime)[keeps])
  returnList = list(p1,p2,p3,p4,p5,p6,p7)
  return(returnList)
}

plots = get_linearPolyFit(autumnDataset)
print(plots[1])
print(plots[2])
print(plots[3])
print(plots[4])
print(plots[5])
spring_plots = get_linearPolyFit(springDataset)
print(spring_plots[1])
print(spring_plots[2])
print(spring_plots[4])

winter_plots = get_linearPolyFit(winterDataset)
print(winter_plots[1])
print(winter_plots[2])
print(winter_plots[4])
summer_plots = get_linearPolyFit(summerDataset)
print(summer_plots[1])
print(summer_plots[2])
print(summer_plots[4])

awayTime <- factor(weekend$Time)
keeps<-c(1,38,78,118,158,198,238,278,318,358,372) 
weekend_plots = get_linearPolyFit(weekend)
print(weekend_plots[1])
print(weekend_plots[2])
print(weekend_plots[4])

awayTime <- factor(weekday$Time)
keeps<-c(1,121,241,361,481,601,721,841,961,1081,1201,1321,1440) # Indices for levels you want to show
weekday_plots = get_linearPolyFit(weekday)
print(weekday_plots[1])
print(weekday_plots[2])
print(weekday_plots[4])