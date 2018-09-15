rm(list=ls())
library(tidyverse)
setwd("/Users/jingmiao/Downloads")

demographics = read_csv("demographics.csv")
fares = read_csv("fares.csv")
real_estate = read_csv("real_estate.csv")
ridership = read_csv("ridership.csv")
stations = read_csv("stations.csv")
weather = read_csv("weather.csv")

# cropping data
a = unique(ridership$hour)
b = unique(ridership$exit_abbr)
c = unique(ridership$enter_abbr)


friday = match(6,ridership$day_of_week)
saturday = match(0,ridership$day_of_week)

days = unique(ridership$day)
days_length = length(days)
fourier_data = matrix(0,days_length,2)
fourier_data[,1] = days

trips_per_day = ridership %>% 
  group_by(enter_abbr,exit_abbr) %>%
  summarise(trips_per_day = mean(trip_count))

trips_per_day["ZIPCODE"] <- NA 

station_with_zip = stations %>% mutate(enter_abbr = abbr) %>%select(enter_abbr, zipcode) 
combined= right_join(trips_per_day, station_with_zip, by = "enter_abbr")
combined2 = right_join(demographics,combined,by="zipcode")

mr2011 = apply(real_estate[,c(185:196)],1,mean)
mr2012 = apply(real_estate[,c(197:208)],1,mean)
mr2013 = apply(real_estate[,c(209:220)],1,mean)
mr2014 = apply(real_estate[,c(221:232)],1,mean)
mr2015 = apply(real_estate[,c(233:244)],1,mean)

real_estate["mean2011"] = mr2011
real_estate["mean2012"] = mr2012
real_estate["mean2013"] = mr2013
real_estate["mean2014"] = mr2014
real_estate["mean2015"] = mr2015

write.csv(real_estate,file="real_estate2.csv")


demographics2 = read_csv("demographics2.csv")
a = (demographics2$totol_trip[109:144] - demographics2$totol_trip[1:36])/3
use_for_2011 = demographics2$totol_trip - a


pred = c(0.20521278, -0.39109476, -0.5934323 , -0.54126939,  4.24276071,
          2.20633289,  1.27344464,  1.84050709,  0.01271506, -0.14437332,
          -0.28699839,  0.06971329, -0.15356059, -0.50837326, -0.54067223,
          -0.6009725 , -0.50239965, -0.62733182, -0.40609777, -0.56681781,
          -0.49693235, -0.29589807, -0.54958149, -0.30227191, -0.40919   ,
          -0.33886662, -0.19127358,  0.25110356, -0.26007384,  1.0173453 ,
          -0.49226431, -0.04300618, -0.46480073, -0.40803562,  0.32344511,
          -0.44740696)


y = c(0.31676498, -0.31834755, -0.59961676, -0.6368963 ,  4.51203792,
        2.48182437,  1.49258558,  2.3876442 ,  0.18469659, -0.22220384,
        -0.16123303,  0.24380267, -0.16441159, -0.48756266, -0.6612373 ,
        -0.67637745, -0.62226689, -0.68784325, -0.39454062, -0.63248924,
        -0.42605559, -0.40295481, -0.51798165, -0.24972516, -0.34883838,
        -0.33181648, -0.16437112,  0.37685795, -0.1175993 ,  1.34781205,
        -0.42719085, -0.10518032, -0.51645292, -0.41655627,  0.26317601,
        -0.53947072)

x = 1:length(pred)

plot(x,pred,type="l",col="blue",
     xlab = "different zipcode area",ylab = "normalized trip counts",main="2015 predicted trip counts vs true trip counts")
lines(x,y,type="l",col="red")
legend("topright", legend = c("predicted","true"), col = c("blue","red"),pch = 1)

write.csv(use_for_2011,"use_for_2011.csv")

library(glmnet)
demographics2 = read.csv("demographics2.csv")
glmmod <- glmnet(x=as.matrix(demographics2[3:17]), y=demographics2$totol_trip, alpha=1)