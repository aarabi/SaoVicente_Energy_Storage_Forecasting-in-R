setwd("R:/projects/1 week thing/assign")
library(gdata)
library(plyr)
library(ggplot2)
library(reshape2)
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

#clean merged data
remove_outliers<-quantile(total$Del, probs=0.1)
total<-subset(total, total$Del > remove_outliers)
total<-subset(total, total$Demand > remove_outliers)
summary(total$Del)
summary(total$Demand)

#reshape data to suit needs
#Long- to wide-format data: the cast function
temp_subset<-total[,c(20:24,25,30)]
week_Del<-  dcast(temp_subset,temp_subset$week ~ temp_subset$hour,fun.aggregate = mean, value.var="Del")
colnames(week_Del) <- c("week",0:23)
week_Del<-colMeans(week_Del,na.rm=TRUE)
week_Del <-week_Del[-1]
week_Demand<-   dcast(temp_subset,temp_subset$week ~ temp_subset$hour,fun.aggregate = mean, value.var="Demand")
colnames(week_Demand) <- c("week",0:23)
week_Demand<-colMeans(week_Demand,na.rm=TRUE)
week_Demand<- week_Demand[-1]
week_data<- cbind(week_Del,week_Demand)
id <- rownames(week_data)
week_data <- cbind(id=id, week_data)
colnames(week_data) <- c("hour","Del","Demand")
week_data<- as.data.frame(week_data)


week_data$Del<- as.numeric(levels(week_data$Del))[week_data$Del]
week_data$Demand<- as.numeric(levels(week_data$Demand))[week_data$Demand]
week_data$hour<- as.numeric(levels(week_data$hour))[week_data$hour]
week_data$baseload<- 3000


  
# save the data into a workspace 
rm()
save(total,week_data,file="data_storage.RData")