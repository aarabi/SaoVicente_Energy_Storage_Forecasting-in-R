setwd("R:/projects/1 week thing/assign")
library(gdata)
library(plyr)
library(ggplot2)
library(reshape)
require(lubridate)


#load data
  demand_data <- read.csv(file="sao.vicente.load.csv", header=TRUE)
  wind_data<- read.csv(file="sao.vicente.wind.csv", header=TRUE)
  turbine_data <- read.csv(file="WIND_VXE_2013.csv", header=TRUE)

# formatting data
  data <- turbine_data
  demand_data <- demand_data[-c(1,2)]
colnames(data) <- c("DateTime","P1","P2","P3","P4","P5","P6","P7","A1","A2","A3","A4","A5","A6","A7","Avg_win","min_win","max_win","Del")
colnames(demand_data) <- c("DateTime","Demand")
demand_data$DateTime<-as.POSIXlt(demand_data$DateTime, format="%Y-%m-%d %H:%M:%S")
data$Del<-c(data$Del[2:(nrow(data))]-data$Del[1:(nrow(data)-1)],0)


# adding all 10 min to 1 hour values
for(i in 1:nrow(data))
{
  if(i%%6==0)
  {
    
    data$counter[(i-5):i]<- i
    data$day[(i-5):i]<- yday(as.Date(data$DateTime[i], format="%m/%d/%Y"))
    
  }
  
}
data$week <- week(as.Date(data$DateTime,format="%m/%d/%y"))
data$month <- month(as.Date(data$DateTime,format="%m/%d/%y"))
data$hour <- as.POSIXlt(data$DateTime,format="%m/%d/%y %H:%M")$hour
data$hour[is.na(data$hour)]<-0

demand_data$week <- week(as.Date(demand_data$DateTime,format="%Y-%m-%d %H:%M:%S"))
demand_data$day <- yday(as.Date(demand_data$DateTime,format="%Y-%m-%d %H:%M:%S"))
demand_data$month <- month(as.Date(demand_data$DateTime,format="%Y-%m-%d %H:%M:%S"))
demand_data$hour <- as.POSIXlt(demand_data$DateTime,format="%Y-%m-%d %H:%M:%S")$hour
demand_data$hour[is.na(demand_data$hour)]<-0


data<-aggregate( . ~ counter, data = data[,-c(1)], sum)

# setting right for columns such as min, max and avg wind speeds which cannot be added up
data[c(1,16:18,20:23)] <- apply(data[c(1,16:18,20:23)], 1:2, function(x) as.integer(x/6))

#calculating new parameters: curtailes, uncurtailed power
data$energy_prod <- (1/2000)*1.225*2174*7*(data$Avg_win)^3
data$eff<-data$Del/data$energy_prod
data$uncurtailed_power<-apply(data[,2:8], 1, sum)
data$curtailed_power<-data$uncurtailed_power-data$energy_prod

#clean the data of outliers
data<-subset(data, data$Del >= 0)
data<-subset(data, data$Del <= (850*7))

#merge data
demand_data$merged <- demand_data$day+ as.integer(demand_data$hour)/100
data$merged <- data$day +as.integer(data$hour)/100
total <- merge(data,demand_data,by=c("merged"))


# save the data into a workspace 
rm()
save(total,file="data_storage.RData")