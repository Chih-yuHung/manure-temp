library(ggplot2)
library(tidyverse)
library(dplyr)
library(hydroGOF) #NSE 
#Sweden project-Fittja
#To compare my simulation result to the measured data

#output to an excel file
Envir.daily<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/2. Method/input/daily env input_Fittja_May1.csv",header=T)
temp<-((Envir.daily$AirTmax1+Envir.daily$AirTmin1)/2)[731:1095] #Air Temp.avg

#observed data
result<-"C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/"
obs.Fittja<-read.csv(paste(result,"temp.Fittja.daily.csv",sep=""),header=T) 

#simulated data before calibration
sim.Fittja.og<-read.csv(paste(result,"Fittja/original/",Location,Sys.Date(),".csv",sep=""),header=T) 
SR.og<-sim.Fittja.og$total.radiation[1096:1460]/12/277.77778
#simulated data after calibration and modification
sim.Fittja<-read.csv(paste(result,"Fittja/with shade/",Location,Sys.Date(),".csv",sep=""),header=T)
SR<-sim.Fittja$total.radiation[1096:1460]/12/277.77778
#Draw the last year only
sim.Fittja.og<-sim.Fittja.og[c(1096:1460),]
sim.Fittja<-sim.Fittja[c(1096:1460),]

#For measured manure temperature
png(file=paste(result,"Fittja/figures/",Location,Sys.Date(),".png",sep="")
    ,width=800, height =600)
par(mar=c(3,4,2,4))
plot(temp,type="l",xaxt='n',col="green",ylim=c(-15,30)
     ,xlab="Date",ylab="Temperature",las=1) #Air temperature
lines(obs.Fittja$temp.avg,type="l") #manure avg. obs temperature
lines(sim.Fittja.og$Temperature.C,type="l",col="blue",lwd=2) #without shade calibration
lines(sim.Fittja$Temperature.C,col="red",lwd=2)
legend(10,-10,c("Tair","Tm meausrement","Tm model","Tm revised model"
                ,"solar irradiation","solar irradiation revised model")
       ,col=c("green","black","blue","red","lightblue","red")
       ,lty=c(1,1,1,1,2),lwd=2,ncol=2,bty="n")
lines(SR.og,col="lightblue",lty="dashed")
lines(SR,col="red",lty="dashed")
Axis(side=1, at=c(1,93,185,277),las=1
     ,labels=c("May 1, 2020","Aug. 1, 2020","Nov. 1, 2020","Feb. 1, 2021"))
arrows(69,-5,69,0) #removal dates
arrows(155,-5,155,0)
arrows(183,-5,183,0)
arrows(347,-5,347,0)
Axis(side=4, at=c(0,5,10,15,20,25,30)
     ,labels=c("0","5","10","15","20","25","30"))
mtext("Solar irradiation (MJ /m2)",side=4,line = 2.5)
text(20,28,"Örsundsbro (OR) site")
dev.off()

#For manure depth
png(file=paste(result,"Fittja/figures/",Location,Sys.Date(),"_depth.png",sep="")
    ,width=800, height =600)
plot(sim.Fittja$Depth.cm,type="l"
     ,ylim=c(0,350),xaxt='n'
     ,col="black",xlab="Date"
     ,ylab="Depth (cm)")
#retrieved from measurement data
points(c(1,49,116,130,174,289,303),
       c(67,242,310,190,73,280,230))
Axis(side=1, at=c(1,93,185,277)
     ,labels=c("May 1, 2020","Aug. 1, 2020","Nov. 1, 2020","Feb. 1, 2021"))
dev.off()


#obtain a stat table
source("stat output.R")
########################################
##A plot to compare Ta, Tm-avg. Tm-0.5m, Tm-1.5, and Tm-2.5
#A Temperature
plot(temp,type="l",col="grey")
lines(result.m$temp.avg,col="red",lwd=2)
lines(result.m$temp0.5,col="orange",lwd=2)
lines(result.m$temp1.5,col="green",lwd=2,lty=2)
lines(result.m$temp2.5,col="blue",lwd=2,lty=2)
legend(0,20,c("Ta","Tm-avg.","Tm-0.5m","Tm-1.5m","Tm-2.5m"),col=c("grey","red","orange","green","blue")
       ,lty=1,lwd=2,ncol=1,bty="n")
#B real depth of the manure thermometer
plot(result.m$Depth,type="l",col="grey",ylim=c(3,0))
lines(result.m$depth0.5,type="l",col="orange",lwd=2)
lines(result.m$depth1.5,type="l",col="green",lwd=2)
lines(result.m$depth2.5,type="l",col="blue",lwd=2)

###########################################################################
#do a color depth plot
result<-result.m[1:365,]
#organize the data to be three columns only, date, depth and temp
DOY1<-rep(result$DOY,each=3)
Temp<-as.vector(t(result[,6:8]))
Temp[is.na(Temp)]<-runif(10,min=0,max=0.005)
Depth<-as.vector(t(result[,10:12]))
Depth[is.na(Depth)]<-runif(1,min=0,max=0.005)
manure<-as.data.frame(cbind(DOY1,Temp,Depth))
#To skip the part with measurments
manure.1<-manure[1:441,]
manure.2<-manure[505:879,]
manure.3<-manure[924:1095,]

#interpolate the temp between depth in a day
estimate_temp_by_date <- function(target_date, target_depth) {
  data_for_date <- manure %>% 
    filter(DOY1 == target_date) %>%
    arrange(Depth)
  
  # approx() is one way to do a linear interpolation
  approx(data_for_date$Depth, data_for_date$Temp, xout = target_depth)$y
}
# To obtain temp in mutilple days
temp_interp_depth.1<- crossing(
  # the same dates as manure.1
  tibble(DOY1 = unique(manure.1$DOY1)),
  # depths can now be any value
  tibble(manure.depth = seq(0.5, 2.5, length.out = 100))
  ) %>% 
  group_by(DOY1)%>% 
  mutate(temp1 = estimate_temp_by_date(DOY1[1], manure.depth))
  
temp_interp_depth.2<- crossing(
  # the same dates as manure.2
  tibble(DOY1 = unique(manure.2$DOY1)),
  # depths can now be any value
  tibble(manure.depth = seq(0.5, 2.5, length.out = 100))
) %>% 
  group_by(DOY1)%>% 
  mutate(temp1 = estimate_temp_by_date(DOY1[1], manure.depth))

temp_interp_depth.3<- crossing(
  # the same dates as manure.2
  tibble(DOY1 = unique(manure.3$DOY1)),
  # depths can now be any value
  tibble(manure.depth = seq(0.5, 2.5, length.out = 100))
) %>% 
  group_by(DOY1)%>% 
  mutate(temp1 = estimate_temp_by_date(DOY1[1], manure.depth))


temp_interp_depth<-rbind(temp_interp_depth.1,temp_interp_depth.2,temp_interp_depth.3)

#For air temperature
temp<-(Envir.daily$AirTmax1+Envir.daily$AirTmin1)/2 #Air Temp.avg
air_temp<-as.data.frame(cbind(c(1:365),rep(0.4,365),temp[1:365]))
colnames(air_temp)<-c("DOY1","manure.depth","temp1")
aa<-tibble(air_temp)
temp_interp_depth<-rbind(temp_interp_depth,tibble(air_temp))

ggplot(temp_interp_depth, aes(DOY1, manure.depth, fill = temp1)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 5, #  5 is the median temperature 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  ) +
  coord_cartesian(expand = FALSE)

ggplot(result.daily, aes(DOY, manure.depth, fill=manure.temp)) +
  geom_tile() +
  scale_y_reverse() +
  scale_fill_gradient2(
    midpoint = 15, #  5 is the median temperature 
    high = scales::muted("red"), 
    low = scales::muted("blue")
  ) +
  coord_cartesian(expand = FALSE)
