library(corrplot)
setwd('E:\\OneDrive - sfu.ca\\CMPT 318 Cyber Security\\cmpt318-project')
data <- read.csv("TrainData.txt", header = TRUE, sep = ",", dec = ".")
# eliminate all NA's
i<-1
while(i <= nrow(data))
{
  #print(i)
  if(is.na(data[i, 3])||is.na(data[i, 4])||is.na(data[i, 5])||is.na(data[i, 6])||is.na(data[i, 7])||is.na(data[i, 8])||is.na(data[i, 9]))
  {
    data<- data[-c(i), ]
    
  }
  else
  {
    i = i + 1
  }
  
}
print(data)

count <- nrow(data)

Global_active_power<-data.frame(c(data[1:count, 3]))
Global_reactive_power<-data.frame(c(data[1:count, 4] ))
Voltage<-data.frame(c(data[1:count, 5]))
Global_intensity <- data.frame(c(data[1:count, 6] ))
Submetering_1 <- data.frame(c(data[1:count, 7] ))
Submetering_2 <- data.frame(c(data[1:count, 8] ))
Submetering_3 <- data.frame(c(data[1:count, 9] ))

list1<-list(Global_active_power,Global_reactive_power,Voltage,Global_intensity, Submetering_1, Submetering_2, Submetering_3)
correlation_mat<-matrix(, nrow = 7 , ncol = 7 )
print(length(correlation_mat))
count1<-1
count2<-1
for(x in list1)
{
  count2 = 1
  for(y in list1)
  {
    correlation_mat[count1,count2]<-cor(x,y, method ="pearson")
    count2 = count2 + 1
  }
  count1 = count1 + 1
}
rownames(correlation_mat)<-c("Global_active_power","Global_reactive_power","Voltage","Global_intensity", "Submetering_1", "Submetering_2", "Submetering_3")
colnames(correlation_mat)<-c("Global_active_power","Global_reactive_power","Voltage","Global_intensity", "Submetering_1", "Submetering_2", "Submetering_3")
print(correlation_mat)

corrplot(correlation_mat, method = 'color', order="hclust", addCoef.col = 'black', tl.col="black", tl.srt=45, type="upper",mar=c(0.1,0.1,0.1,0.1))



