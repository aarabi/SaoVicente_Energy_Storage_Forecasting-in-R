library(gdata)
library(lubridate)



data<-read.csv(file="WIND_VXE_2013.csv", header=TRUE)
new.names<-c("date_time","T1_Possible_Power","T2_Possible_Power","T3_Possible_Power",
               "T4_Possible_Power","T5_Possible_Power","T6_Possible_Power","T7_Possible_Power",
                "T1_Total_Active_Power","T2_Total_Active_Power","T3_Total_Active_Power","T4_Total_Active_Power",
                "T5_Total_Active_Power","T6_Total_Active_Power","T7_Total_Active_Power","mean_wind_mps", "min_wind_mps",
                "max_wind_mps", "cum_energy_delivered_kwh")

cbind(names(data), new.names)
names(data) <- new.names

#subset data
cumulative <- subset(data, select=c(1,19))
possible <- subset(data, select=1:8)
active<- subset(data, select=c(1,9:15))
wind<-subset(data, select=c(1,16:18))

dat<-data
dim(dat) #dimesnsions of the data
str(dat) #structure of data
summary(dat) # statistical summary


#check for any missing data
# check<-function(df){
#   # count NA (missing values)
#   NAs<-sum(is.na(df))
#   print(paste("Missing Values:", NAs))
#   
#   # count incomplete records (rows containing missing values)
#   ok<-complete.cases(df)
#   print(paste("Incomplete Records:", sum(! ok)))
#   
#   # Show incomplete records (if less than 100 NAs). 
#   if(NAs > 0 & NAs <= 100) print( df[which(! complete.cases(df)), ] )
#   
#   # If more than 100, show column-wise distribution of NAs.
#   if (NAs > 100) hist(which(is.na(df), arr.ind=TRUE)[,2], xlab="Column", freq=TRUE, breaks=1:dim(df)[2], main="Column-wise distribution of missing values")
# }
# removed<-function(nrow, nrow1){
#   print(paste("number of records REMOVED:", nrow-nrow1, sep=" "))
#   print(paste("number of records REMAINING:", nrow1, sep=" "))
# }
# 
# check(dat)  # NAs present. Column-wise distribution is relatively uniform (besides wind with relatively few)

nrow<-dim(dat)[1] # record the dimensions of the data (before removing anything!)
dat<-na.omit(dat) # omit rows containing missing values
nrow1<-dim(dat)[1] # record the new dimensions of the data (after removing NAs)
removed(nrow, nrow1) # check how many records have been removed

# given is cumulative energy. so subtract the next and current values to get the actual value

n<-length(dat$cum_energy_delivered_kwh)
a<-dat$cum_energy_delivered_kwh[1:n-1]
b<-dat$cum_energy_delivered_kwh[2:n]
diff<-b-a
dat$energy_sentout_10min_kwh<-c(diff,0)


# compute the kinetic energy in the wind at each windspeed
# compute kinetic energy in the wind at each windspeed
# Wind Power = (1/2)*rho*area*(velocity)^3 = [kg/m^3]*[m^2]*[m/s]^3 = [kg*m^2/s^3] = [kg*m^2/s^2][1/s] = [Newton-meter]/[second] = [Joules/second] = [Watts]
rho=1.225 # density of wind (kg/m^3)
area=2174 # sweep area of wind turbines (m^2)
turbines=7 # number of turbines
c<-(1/2)*rho*area
dat$wind_power_kw<-c*(dat$mean_wind_mps)^3*turbines/1000 # kW avg power
dat$wind_energy_10min_kwh<-c*(dat$mean_wind_mps)^3*turbines/(1000*6) # kWh in 10 min (60/10)

betz.coef<- 16/27
dat$betz_limit_10min_kwh<-dat$wind_energy_10min_kwh*betz.coef

# compute turbine efficiency
dat$turbine_eff<-dat$energy_sentout_10min_kwh/dat$wind_energy_10min_kwh

# curtailement: is reducing generation at a facility below what it could be capable of producing
# compute total Possible Power
# instead of using a loop, use the apply function
      # apply(X, MARGIN, FUN, ...)
      # 
      # where:
      #   X is an array or matrix;
      # MARGIN is a variable that determines whether the function is applied over rows (MARGIN=1), columns (MARGIN=2), or both (MARGIN=c(1,2));
      # FUN is the function to be applied.
uncurtailed_power<-apply(X=dat[,2:8], MARGIN=1, FUN=sum)
dat$uncurtailed_10min_kwh<-(uncurtailed_power)/6

# compute curtailment
dat$curtailment_10min_kwh<-dat$uncurtailed - dat$energy_sentout_10min_kwh

check(dat)

# NaN arise when we compute turbine efficiency with zero in the numerator and denominator (due to windspeed = 0 & energy_sentout = 0). Set these to zero.
nan<-which(dat$turbine_eff == "NaN")
# length(nan) # same as number of NAs
# head(dat[nan, ]) # look at the rows containing "NaN"
dat$turbine_eff[nan]<-0

# Likewise, Inf arise when we compute turbine efficiency with zero in the denominator (due to windspeed = 0).  Set these to zero.
inf<-which(dat$turbine_eff == "Inf")
# length(inf) # not counted in NA count, beware!
# head(dat[inf, ]) # look at the rows containing "NaN"
dat$turbine_eff[inf]<-0

check(dat)


# group according to the time
dates_data <- as.Date(dat$date_time,"%Y/%m/%d")
dat$month <- cut(dates_data, breaks = "month")
week <- cut(dates_data, breaks = "week")
day <- cut(dates_data, breaks = "day")
hour <- cut(dates_data, breaks = "hour")
