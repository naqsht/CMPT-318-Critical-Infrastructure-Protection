library(lubridate)
library(ggplot2)
setwd('/home/jl/cmpt318-project')
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

get_mean<- function(data){
  data$Date <- as.Date(data$Date, "%Y-%m-%d")
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
pca <-get_mean(autumnDataset)
summary(pca)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(pca)

pca_summer <-get_mean(summerDataset)
ggbiplot(pca_summer)

pca_spring <-get_mean(springDataset)
ggbiplot(pca_spring)

pca_winter<-get_mean(winterDataset)
ggbiplot(pca_winter)
