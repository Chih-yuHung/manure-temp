#This file is to find out possile manure input and manure removal time
#The concept is to find arupt temperature chamge at 1.5 and 2.5 m
#Deal with the manure temperature
temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/manure temp Fittja.csv",header=T)
#Use the data until Feb 27, 2021 (2428)
temp<-temp[1:2428,]

for (i in 1:3) {
temp[,i+5]<-c(0,ifelse(diff(temp[,i+1])<(-2),1,0))
}

timen0.5<-subset(temp, V6 == 1, select = Time)
timen1.5<-subset(temp, V7 == 1, select = Time)
timen2.5<-subset(temp, V8 == 1, select = Time)

cbind(time1.5,time0.5)
library(plyr)
time<-cbind.fill(time0.5,time1.5)

#It looks like there is a removal event in August
#about August 13