library(lubridate)
library("depmixS4")
countLines <- function(data){
  row_num <- nrow(data)
  Dates<-data.frame(paste0(data[1:row_num, 1]))
  current_date<-Dates[2,1]
  lines_count <- list()
  count<-1
  for (i in 2:row_num)
  {
    
    if(Dates[i,1]==current_date)
    {
      count = count + 1
    }
    else
    {
      lines_count[[paste0(current_date)]]<-count
      count=1
    }
    current_date = Dates[i,1]
  }
  lines_count[[paste0(Dates[row_num,1])]]<-count
  return (lines_count)
}



set.seed(1)
#Parse training data
#setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security\\cmpt318-project')
setwd('D:\\CMPT318D1-1121-Term-Project\\cmpt318-project')

consumption_data <- read.csv("data_without_NA.txt", header = TRUE, sep = ",", dec = ".")
consumption_data$Date<-as.Date(consumption_data$Date,format='%d/%m/%Y')

train_consumption_data<-consumption_data[consumption_data$Date < as.Date('2010/01/01'),]
weekdays_date = strftime(train_consumption_data$Date, format="%u")
train_consumption_data$weekdays_num = as.integer(weekdays_date)

train_data_spring_weekday<-train_consumption_data[(train_consumption_data$Date>=as.Date('2007/03/01')&train_consumption_data$Date<=as.Date('2007/05/31'))&
                                                    train_consumption_data$weekdays_num<=5,]

train_data_spring_weekend<-train_consumption_data[(train_consumption_data$Date>=as.Date('2007/03/01')&train_consumption_data$Date<=as.Date('2007/05/31'))&
                                                    
                                                    train_consumption_data$weekdays_num>5,]


train_data_summer_weekday<-train_consumption_data[(train_consumption_data$Date>=as.Date('2007/06/01')&train_consumption_data$Date<=as.Date('2007/08/31'))&
                                                    train_consumption_data$weekdays_num<=5,]

train_data_summer_weekend<-train_consumption_data[(train_consumption_data$Date>=as.Date('2007/06/01')&train_consumption_data$Date<=as.Date('2007/08/31'))&
                                                    train_consumption_data$weekdays_num>5,]


train_data_autumn_weekday<-train_consumption_data[(train_consumption_data$Date>=as.Date('2007/09/01')&train_consumption_data$Date<=as.Date('2007/11/30'))&
                                                    train_consumption_data$weekdays_num<=5,]


train_data_autumn_weekend<-train_consumption_data[(train_consumption_data$Date>=as.Date('2007/09/01')&train_consumption_data$Date<=as.Date('2007/11/30'))&
                                                    train_consumption_data$weekdays_num>5,]


train_data_winter_weekday<-train_consumption_data[(train_consumption_data$Date>=as.Date('2006/12/01')&train_consumption_data$Date<=as.Date('2007/02/28'))&
                                                    train_consumption_data$weekdays_num<=5,]


train_data_winter_weekend<-train_consumption_data[(train_consumption_data$Date>=as.Date('2006/12/01')&train_consumption_data$Date<=as.Date('2007/02/28'))&
                                                    train_consumption_data$weekdays_num>5,]



setwd('D:\\CMPT318D1-1121-Term-Project\\cmpt318-project\\statstics data')
test_data_spring_weekday<-read.csv("test4_spring_weekday.txt", header = TRUE, sep = ",", dec = ".")
test_data_spring_weekend<-read.csv("test4_spring_weekend.txt", header = TRUE, sep = ",", dec = ".")
test_data_summer_weekday<-read.csv("test4_summer_weekday.txt", header = TRUE, sep = ",", dec = ".")
test_data_summer_weekend<-read.csv("test4_summer_weekend.txt", header = TRUE, sep = ",", dec = ".")
test_data_autumn_weekday<-read.csv("test4_autumn_weekday.txt", header = TRUE, sep = ",", dec = ".")
test_data_autumn_weekend<-read.csv("test4_autumn_weekend.txt", header = TRUE, sep = ",", dec = ".")
test_data_winter_weekday<-read.csv("test4_winter_weekday.txt", header = TRUE, sep = ",", dec = ".")
test_data_winter_weekend<-read.csv("test4_winter_weekend.txt", header = TRUE, sep = ",", dec = ".")

test_data_spring_weekday<-na.omit(test_data_spring_weekday)
test_data_spring_weekend<-na.omit(test_data_spring_weekend)
test_data_summer_weekday<-na.omit(test_data_summer_weekday)
test_data_summer_weekend<-na.omit(test_data_summer_weekend)
test_data_autumn_weekday<-na.omit(test_data_autumn_weekday)
test_data_autumn_weekend<-na.omit(test_data_autumn_weekend)
test_data_winter_weekday<-na.omit(test_data_winter_weekday)
test_data_winter_weekend<-na.omit(test_data_winter_weekend)





###########################features to train on univariate###################################
hmm_features<-Global_intensity~1
family_list<-gaussian()
####################################################################################
hmm_spring_weekday <- depmix(hmm_features, data=train_data_spring_weekday, nstates=7,ntimes=unlist(countLines(train_data_spring_weekday), use.names=FALSE),
                             family=family_list)
fhmm_spring_weekday <- fit(hmm_spring_weekday)
print(fhmm_spring_weekday)
hmm_spring_weekday_test <- depmix(hmm_features, data=test_data_spring_weekday, nstates=7, ntimes= unlist(countLines(test_data_spring_weekday), use.names=FALSE),
                                  family=family_list)
hmm_spring_weekday_test <- setpars(hmm_spring_weekday_test,getpars(fhmm_spring_weekday))
logLik(hmm_spring_weekday_test)

####################################################################################

hmm_summer_weekday <- depmix(hmm_features, data=train_data_summer_weekday, nstates=6,ntimes=unlist(countLines(train_data_summer_weekday), use.names=FALSE),
                             family=family_list)
fhmm_summer_weekday <- fit(hmm_summer_weekday)
print(fhmm_summer_weekday)
hmm_summer_weekday_test <- depmix(hmm_features, data=test_data_summer_weekday, nstates=6, ntimes= unlist(countLines(test_data_summer_weekday), use.names=FALSE),
                                  family=family_list)
hmm_summer_weekday_test <- setpars(hmm_summer_weekday_test,getpars(fhmm_summer_weekday))
logLik(hmm_summer_weekday_test)
print(test_data_summer_weekday)
########################################################################################################################################################################

hmm_autumn_weekday <- depmix(hmm_features, data=train_data_autumn_weekday, nstates=4,ntimes=unlist(countLines(train_data_autumn_weekday), use.names=FALSE),
                             family=family_list)
fhmm_autumn_weekday <- fit(hmm_autumn_weekday)
print(fhmm_autumn_weekday)
hmm_autumn_weekday_test <- depmix(hmm_features, data=test_data_autumn_weekday, nstates=4, ntimes= unlist(countLines(test_data_autumn_weekday), use.names=FALSE),
                                  family=family_list)
hmm_autumn_weekday_test <- setpars(hmm_autumn_weekday_test,getpars(fhmm_autumn_weekday))
logLik(hmm_autumn_weekday_test)
########################################################################################################################################################################
hmm_winter_weekday <- depmix(hmm_features, data=train_data_winter_weekday, nstates=8,ntimes=unlist(countLines(train_data_winter_weekday), use.names=FALSE),
                             family=family_list)
fhmm_winter_weekday <- fit(hmm_winter_weekday)
print(fhmm_winter_weekday)
hmm_winter_weekday_test <- depmix(hmm_features, data=test_data_winter_weekday, nstates=8, ntimes= unlist(countLines(test_data_winter_weekday), use.names=FALSE),
                                  family=family_list)
hmm_winter_weekday_test <- setpars(hmm_winter_weekday_test,getpars(fhmm_winter_weekday))
logLik(hmm_winter_weekday_test)
########################################################################################################################################################################

hmm_spring_weekend <- depmix(hmm_features, data=train_data_spring_weekend, nstates=5,ntimes=unlist(countLines(train_data_spring_weekend), use.names=FALSE),
                             family=family_list)
fhmm_spring_weekend <- fit(hmm_spring_weekend)
print(fhmm_spring_weekend)
hmm_spring_weekend_test <- depmix(hmm_features, data=test_data_spring_weekend, nstates=5, ntimes= unlist(countLines(test_data_spring_weekend), use.names=FALSE),
                                  family=family_list)
hmm_spring_weekend_test <- setpars(hmm_spring_weekend_test,getpars(fhmm_spring_weekend))
logLik(hmm_spring_weekend_test)

########################################################################################################################################################################

hmm_summer_weekend <- depmix(hmm_features, data=train_data_summer_weekend, nstates=6,ntimes=unlist(countLines(train_data_summer_weekend), use.names=FALSE),
                             family=family_list)
fhmm_summer_weekend <- fit(hmm_summer_weekend)
print(fhmm_summer_weekend)
hmm_summer_weekend_test <- depmix(hmm_features, data=test_data_summer_weekend, nstates=6, ntimes= unlist(countLines(test_data_summer_weekend), use.names=FALSE),
                                  family=family_list)
hmm_summer_weekend_test <- setpars(hmm_summer_weekend_test,getpars(fhmm_summer_weekend))
logLik(hmm_summer_weekend_test)
########################################################################################################################################################################

hmm_autumn_weekend <- depmix(hmm_features, data=train_data_autumn_weekend, nstates=7,ntimes=unlist(countLines(train_data_autumn_weekend), use.names=FALSE),
                             family=family_list)
fhmm_autumn_weekend <- fit(hmm_autumn_weekend)
print(fhmm_autumn_weekend)
hmm_autumn_weekend_test <- depmix(hmm_features, data=test_data_autumn_weekend, nstates=7, ntimes= unlist(countLines(test_data_autumn_weekend), use.names=FALSE),
                                  family=family_list)
hmm_autumn_weekend_test <- setpars(hmm_autumn_weekend_test,getpars(fhmm_autumn_weekend))
logLik(hmm_autumn_weekend_test)
########################################################################################################################################################################

hmm_winter_weekend <- depmix(hmm_features, data=train_data_winter_weekend, nstates=5,ntimes=unlist(countLines(train_data_winter_weekend), use.names=FALSE),
                             family=family_list)
fhmm_winter_weekend <- fit(hmm_winter_weekend)
print(fhmm_winter_weekend)
hmm_winter_weekend_test <- depmix(hmm_features, data=test_data_winter_weekend, nstates=5, ntimes= unlist(countLines(test_data_winter_weekend), use.names=FALSE),
                                  family=family_list)
hmm_winter_weekend_test <- setpars(hmm_winter_weekend_test,getpars(fhmm_winter_weekend))
logLik(hmm_winter_weekend_test)

