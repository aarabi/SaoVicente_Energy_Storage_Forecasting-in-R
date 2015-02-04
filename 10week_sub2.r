setwd("R:/projects/1 week thing/assign")
load("data_storage.RData")

require(ggplot2)
require(shiny)


ggplot(data=demand_data, aes(x=DateTime, y=Demand, group=1)) + geom_point()
ggplot(data=data, aes(x=day, y=energy_prod, group=1)) + geom_line()
summary(demand_data$Demand)

ggplot(data, aes(x=time, y=value, colour=day,group=day)) + geom_line()


runApp("myapp")
