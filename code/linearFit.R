library(lubridate)
library(ggplot2)
setwd('/home/jl/cmpt318-project')
setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security')

dataSet <- read.csv("data_without_NA.txt", header = TRUE, sep = ",", dec = ".")
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

time <- factor(summerDataset$Date , Date = "%Y-%m-%d")

get_plot_by_year<- function(data){
  data$Date <- as.Date(data$Date, "%d/%m/%Y")
  dates <- factor(data$Date)
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
  p1 <- ggplot(data=mean_total , mapping=aes(x=Date, y=Global_active_power, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue") +
    scale_x_date(date_labels="%b %Y") +
    xlab("Date")
  p2 <- ggplot(data=mean_total , mapping=aes(x=Date, y=Global_reactive_power, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_date(date_labels="%b %Y")+
    xlab("Date")
  p3 <- ggplot(data=mean_total , mapping=aes(x=Date, y=Global_intensity, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_date(date_labels="%b %Y")+ 
    xlab("Date")
  p4 <- ggplot(data=mean_total , mapping=aes(x=Date, y=Voltage, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_date(date_labels="%b %Y")+ 
    xlab("Date")
  plots <- list(p1,p2,p3,p4)
  return(plots)
}
plot = get_plot_by_year(dataSet)
print(plot[1])
print(plot[2])
print(plot[3])
print(plot[4])
get_mean<- function(data){
  data$Date <- as.Date(data$Date, "%Y-%m-%d")
  data$Date <- format(data$Date, "%m-%d")
  dates <- factor(data$Date)
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
  p1 <- ggplot(data=mean_total , mapping=aes(x=as.Date(Date,"%m-%d"), y=Global_active_power, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue") +
    scale_x_date(date_labels="%b-%d") +
    xlab("Date")
  p2 <- ggplot(data=mean_total , mapping=aes(x=as.Date(Date,"%m-%d"), y=Global_reactive_power, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_date(date_labels="%b-%d")+
    xlab("Date")
  p3 <- ggplot(data=mean_total , mapping=aes(x=as.Date(Date,"%m-%d"), y=Global_intensity, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_date(date_labels="%b-%d")+ 
    xlab("Date")
  p4 <- ggplot(data=mean_total , mapping=aes(x=as.Date(Date,"%m-%d"), y=Voltage, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_date(date_labels="%b-%d")+ 
    xlab("Date")
  plots <- list(p1,p2,p3,p4)
  return(plots)
}

plotSum = get_mean(summerDataset)
print(plotSum[1])
print(plotSum[2])
print(plotSum[3])
print(plotSum[4])

plotAut= get_mean(autumnDataset)
print(plotAut[1])
print(plotAut[2])
print(plotAut[3])
print(plotAut[4])
plotSpr = get_mean(springDataset)
print(plotSpr[1])
print(plotSpr[2])
print(plotSpr[3])
print(plotSpr[4])
get_meanWinter<- function(data){
  data$Date <- as.Date(data$Date, "%Y-%m-%d")
  data$Date <- format(data$Date, "%b-%d")
  dates <- factor(data$Date)
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
  p1 <- ggplot(data=mean_total , mapping=aes(x=Date, y=Global_active_power, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue") +
    scale_x_discrete(
      breaks=levels(dates)[keeps],
      labels=levels(dates)[keeps])
  p2 <- ggplot(data=mean_total , mapping=aes(x=Date, y=Global_reactive_power, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_discrete(
      breaks=levels(dates)[keeps],
      labels=levels(dates)[keeps])
  p3 <- ggplot(data=mean_total , mapping=aes(x=Date, y=Global_intensity, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_discrete(
      breaks=levels(dates)[keeps],
      labels=levels(dates)[keeps])
  p4 <- ggplot(data=mean_total , mapping=aes(x=Date, y=Voltage, group = 1)) +
    geom_point(color="black") +
    geom_smooth(color="Blue")  +
    scale_x_discrete(
      breaks=levels(dates)[keeps],
      labels=levels(dates)[keeps])
  plots <- list(p1,p2,p3,p4)
  return(plots)
}
keeps<-c(1,10,20,30,40,50,60,70,80,90,92)
plotWin= get_meanWinter(winterDataset)

print(plotWin[1])
print(plotWin[2])
print(plotWin[3])
print(plotWin[4])

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
keeps<-c(1,121,241,361,481,601,721,841,961,1081,1201,1321,1440) # Indices for levels you want to show
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
print(weekend_plots[3])
print(weekend_plots[4])

awayTime <- factor(weekday$Time)
keeps<-c(1,121,241,361,481,601,721,841,961,1081,1201,1321,1440) # Indices for levels you want to show
weekday_plots = get_linearPolyFit(weekday)
print(weekday_plots[1])
print(weekday_plots[2])
print(weekday_plots[3])
print(weekday_plots[4])
