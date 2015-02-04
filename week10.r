setwd("R:/projects/1 week thing/assign")
library(gdata)
library(plyr)
library(ggplot2)
library(reshape)
#load data
demand_data <- read.csv(file="sao.vicente.load.csv", header=TRUE)
wind_data<- read.csv(file="sao.vicente.wind.csv", header=TRUE)

#create temp storage
modified_wind_data <- wind_data

# statistical summary of the data
# dim(modified_wind_data)
# str(modified_wind_data)
# summary(modified_wind_data)



# clean data - remove NAs 
prev_rows <- dim(modified_wind_data)
modified_wind_data <- na.omit(modified_wind_data)
curr_rows <- dim(modified_wind_data)
print(prev_rows-curr_rows)

# calculate turbine efficeincy
modified_wind_data$turbine_eff<- modified_wind_data$energy_delivered_hourly / modified_wind_data$wind.supply.kwh

# compute uncurtailed power
modified_wind_data$uncurtailed_power<-apply(X=modified_wind_data[,5:11], 1, sum)

# compute curtailed power
modified_wind_data$curtailed_power<-modified_wind_data$uncurtailed_power-modified_wind_data$energy_delivered_hourly

#betz calc
betz.coef<- 16/27
modified_wind_data$betz_limit<-modified_wind_data$wind.supply.kwh*betz.coef


# clean data - remove NAs 
prev_rows <- dim(modified_wind_data)
modified_wind_data <- na.omit(modified_wind_data)
curr_rows <- dim(modified_wind_data)
print(prev_rows-curr_rows)

# divide by month, week, day, hour
# consistent format
modified_wind_data$datetime <- paste(modified_wind_data$date, modified_wind_data$hour)
print(modified_wind_data$datetime)
modified_wind_data$datetime<- paste(modified_wind_data$datetime,":00:00", sep="")
print(modified_wind_data$datetime)
modified_wind_data$datetime<-as.POSIXlt( modified_wind_data$datetime, format="%m/%d/%Y %H:%M")


# categorize
modified_wind_data$month <- cut(modified_wind_data$datetime, breaks = 'month', labels=FALSE)
modified_wind_data$week <- cut(modified_wind_data$datetime, breaks = 'week', labels=FALSE)
modified_wind_data$day <- cut(modified_wind_data$datetime, breaks = 'day', labels=FALSE)


energy<-subset(modified_wind_data, select=c("day", "energy_delivered_hourly", "wind.supply.kwh", "betz_limit", "uncurtailed_power", "curtailed_power"))
energy<-ddply(energy, .(day), numcolwise(sum))

test<-melt(energy, id.vars=("day"))
ggplot(test, aes(x=day, y=value/10^3, group=variable, colour=variable, linetype=variable)) +
  geom_line() +
  scale_y_continuous(name="MWh per day") + 
  labs(title="Energy Timeseries") +
  theme_classic() +
  scale_x_discrete(breaks=test$day[seq(1, 360, by=60)], labels=abbreviate)


energy<-subset(modified_wind_data, select=c("wind_mps_corrected_hourly", "energy_delivered_hourly", "wind.supply.kwh", "betz_limit", "uncurtailed_power", "curtailed_power"))
energy<-melt(energy, id.vars=("wind_mps_corrected_hourly"))
ggplot(energy, aes(x=wind_mps_corrected_hourly, y=value, group=variable, colour=variable)) + 
  geom_point() +
  scale_y_continuous(name="KWh in 10min", limit=c(0, max(modified_wind_data$energy_delivered_hourly))) + 
  scale_x_continuous(name="Windspeed (mps)") + 
  labs(title="Empirical Power Curve with Betz Limit and Theoretical Wind Energy") +
  theme_classic()
