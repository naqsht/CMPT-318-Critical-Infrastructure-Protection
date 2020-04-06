setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security\\CMPT318D1-1121-Term-Project\\cmpt318-project')
test1 <- read.csv("test1.txt", header = TRUE, sep = ",", dec = ".")
test2 <- read.csv("test2.txt", header = TRUE, sep = ",", dec = ".")
test3 <- read.csv("test3.txt", header = TRUE, sep = ",", dec = ".")
test4 <- read.csv("test4.txt", header = TRUE, sep = ",", dec = ".")
test5 <- read.csv("test5.txt", header = TRUE, sep = ",", dec = ".")

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




#test 1 spring weekday 
test1_spring_weekday<-test1[(as.Date(test1$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test1$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test1$week_days_num<=5,]

test1_spring_weekend<-test1[(as.Date(test1$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test1$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test1$week_days_num>5,]

test1_summer_weekday<-test1[(as.Date(test1$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test1$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))
                            &
                              test1$week_days_num<=5,]

test1_summer_weekend<-test1[(as.Date(test1$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test1$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))&
                              test1$week_days_num>5,]

test1_autumn_weekday<-test1[(as.Date(test1$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test1$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test1$week_days_num<=5,]

test1_autumn_weekend<-test1[(as.Date(test1$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test1$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test1$week_days_num>5,]

test1_winter_weekday<-test1[(as.Date(test1$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test1$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test1$week_days_num<=5,]

test1_winter_weekend<-test1[(as.Date(test1$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test1$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test1$week_days_num>5,]

setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security\\CMPT318D1-1121-Term-Project\\cmpt318-project\\statstics data')

write.table(test1_spring_weekday, "test1_spring_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test1_spring_weekend, "test1_spring_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test1_summer_weekday, "test1_summer_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test1_summer_weekend, "test1_summer_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test1_autumn_weekday, "test1_autumn_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test1_autumn_weekend, "test1_autumn_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test1_winter_weekday, "test1_winter_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test1_winter_weekend, "test1_winter_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
#####################################################################################
#test 2
test2_spring_weekday<-test2[(as.Date(test2$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test2$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test2$week_days_num<=5,]

test2_spring_weekend<-test2[(as.Date(test2$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test2$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test2$week_days_num>5,]

test2_summer_weekday<-test2[(as.Date(test2$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test2$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))
                            &
                              test2$week_days_num<=5,]

test2_summer_weekend<-test2[(as.Date(test2$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test2$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))&
                              test2$week_days_num>5,]

test2_autumn_weekday<-test2[(as.Date(test2$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test2$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test2$week_days_num<=5,]

test2_autumn_weekend<-test2[(as.Date(test2$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test2$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test2$week_days_num>5,]

test2_winter_weekday<-test2[(as.Date(test2$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test2$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test2$week_days_num<=5,]

test2_winter_weekend<-test2[(as.Date(test2$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test2$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test2$week_days_num>5,]


write.table(test2_spring_weekday, "test2_spring_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test2_spring_weekend, "test2_spring_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test2_summer_weekday, "test2_summer_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test2_summer_weekend, "test2_summer_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test2_autumn_weekday, "test2_autumn_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test2_autumn_weekend, "test2_autumn_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test2_winter_weekday, "test2_winter_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test2_winter_weekend, "test2_winter_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
####################################################################################
#test 3
test3_spring_weekday<-test3[(as.Date(test3$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test3$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test3$week_days_num<=5,]

test3_spring_weekend<-test3[(as.Date(test3$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test3$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test3$week_days_num>5,]

test3_summer_weekday<-test3[(as.Date(test3$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test3$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))
                            &
                              test3$week_days_num<=5,]

test3_summer_weekend<-test3[(as.Date(test3$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test3$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))&
                              test3$week_days_num>5,]

test3_autumn_weekday<-test3[(as.Date(test3$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test3$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test3$week_days_num<=5,]

test3_autumn_weekend<-test3[(as.Date(test3$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test3$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test3$week_days_num>5,]

test3_winter_weekday<-test3[(as.Date(test3$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test3$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test3$week_days_num<=5,]

test3_winter_weekend<-test3[(as.Date(test3$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test3$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test3$week_days_num>5,]


write.table(test3_spring_weekday, "test3_spring_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test3_spring_weekend, "test3_spring_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test3_summer_weekday, "test3_summer_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test3_summer_weekend, "test3_summer_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test3_autumn_weekday, "test3_autumn_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test3_autumn_weekend, "test3_autumn_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test3_winter_weekday, "test3_winter_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test3_winter_weekend, "test3_winter_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
#####################################################################################
#test4
test4_spring_weekday<-test4[(as.Date(test4$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test4$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test4$week_days_num<=5,]

test4_spring_weekend<-test4[(as.Date(test4$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test4$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test4$week_days_num>5,]

test4_summer_weekday<-test4[(as.Date(test4$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test4$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))
                            &
                              test4$week_days_num<=5,]

test4_summer_weekend<-test4[(as.Date(test4$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test4$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))&
                              test4$week_days_num>5,]

test4_autumn_weekday<-test4[(as.Date(test4$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test4$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test4$week_days_num<=5,]

test4_autumn_weekend<-test4[(as.Date(test4$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test4$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test4$week_days_num>5,]

test4_winter_weekday<-test4[(as.Date(test4$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test4$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test4$week_days_num<=5,]

test4_winter_weekend<-test4[(as.Date(test4$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test4$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test4$week_days_num>5,]

setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security\\CMPT318D1-1121-Term-Project\\cmpt318-project\\statstics data')

write.table(test4_spring_weekday, "test4_spring_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test4_spring_weekend, "test4_spring_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test4_summer_weekday, "test4_summer_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test4_summer_weekend, "test4_summer_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test4_autumn_weekday, "test4_autumn_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test4_autumn_weekend, "test4_autumn_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test4_winter_weekday, "test4_winter_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test4_winter_weekend, "test4_winter_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
#############################################################################
#test5
test5_spring_weekday<-test5[(as.Date(test5$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test5$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test5$week_days_num<=5,]

test5_spring_weekend<-test5[(as.Date(test5$Date, format = '%d/%m/%Y')>=as.Date('2010/03/01')&as.Date(test5$Date, format = '%d/%m/%Y')<=as.Date('2010/05/31'))
                            &
                              test5$week_days_num>5,]

test5_summer_weekday<-test5[(as.Date(test5$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test5$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))
                            &
                              test5$week_days_num<=5,]

test5_summer_weekend<-test5[(as.Date(test5$Date, format = '%d/%m/%Y')>=as.Date('2010/06/01')&as.Date(test5$Date, format = '%d/%m/%Y')<=as.Date('2010/08/31'))&
                              test5$week_days_num>5,]

test5_autumn_weekday<-test5[(as.Date(test5$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test5$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test5$week_days_num<=5,]

test5_autumn_weekend<-test5[(as.Date(test5$Date, format = '%d/%m/%Y')>=as.Date('2010/09/01')&as.Date(test5$Date, format = '%d/%m/%Y')<=as.Date('2010/11/30'))
                            &
                              test5$week_days_num>5,]

test5_winter_weekday<-test5[(as.Date(test5$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test5$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test5$week_days_num<=5,]

test5_winter_weekend<-test5[(as.Date(test5$Date, format = '%d/%m/%Y')>=as.Date('2009/12/01')&as.Date(test5$Date, format = '%d/%m/%Y')<=as.Date('2010/02/28'))
                            &
                              test5$week_days_num>5,]

setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security\\CMPT318D1-1121-Term-Project\\cmpt318-project\\statstics data')

write.table(test5_spring_weekday, "test5_spring_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test5_spring_weekend, "test5_spring_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test5_summer_weekday, "test5_summer_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test5_summer_weekend, "test5_summer_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test5_autumn_weekday, "test5_autumn_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test5_autumn_weekend, "test5_autumn_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test5_winter_weekday, "test5_winter_weekday.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)
write.table(test5_winter_weekend, "test5_winter_weekend.txt", append = FALSE, sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)