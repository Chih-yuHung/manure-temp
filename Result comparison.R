library(devEMF)
library(tidyverse)
library(dplyr)
library(hydroGOF) #NSE 
#Sweden project-Fittja
#To compare my simulation result to the measured data
result <- "C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/"
#output to an excel file
#Envir.daily <- read.csv(paste("input/daily env input_",Location,".csv",sep = ""),header = T)
temp <- ((Envir.daily$AirTmax1 + Envir.daily$AirTmin1)/2)[731:1095] #Air Temp.avg

#observed data
obs <- read.csv(paste(result,Location,"/",Location,".daily.csv",sep = ""),header = T) 
depth.m <- read.csv(paste(result,Location,"/",Location,".depth.csv",sep = ""),header = T)

#simulated data before calibration
sim.og <- read.csv(paste(result,Location,"/original/",
                                Location,"_",test,".csv",sep = ""),header = T) 
SR.og <- sim.og$total.radiation/12/277.77778
SR.og.cum <- cumsum(SR.og)
Eva.og.cum <- cumsum(sim.og$Evaporation.cm)
#simulated data after calibration and modification
sim.re <- read.csv(paste(result,Location,"/with shade/",
                             Location,"_",test,".csv",sep = ""),header = T)
SR <- sim.re$total.radiation/12/277.77778
SR.cum <- cumsum(SR)
Eva.cum <- cumsum(sim.re$Evaporation.cm)
sim.re$snow.depth[sim.re$snow.depth == 0] <- NA 
#obtain removal days
removal.a <- removal.start[1:4] - as.numeric(as.Date(start.date)) + 1

#two data.frame for the days in VA and OR sites
OR.date <- data.frame(day = c(1,62,124,185,246,305,366), 
                      date = c("5/1/20","7/1","9/1","11/1",
                               "1/1/21","3/1","5/1"))
VA.date <- data.frame(day = c(1,45,106,167,228,286,348),
                      date = c("6/18/20","8/1","10/1","12/1",
                               "2/1/21","4/1","6/1"))


if (Location == "OR") {
  plot.day <- OR.date$day
  plot.date <- OR.date$date
} else {
  plot.day <- VA.date$day
  plot.date <- VA.date$date
}

#cols <- c("#211d0c","#8077ff","#fcab42","#42bd42")
cols <- c("#5A4F20","#9EACD5","#F6B8A1","#94D1A9")

#plots for the measured manure temperature (Air, Avg. and the 3 depths)
plot.measurement <- function() {
par(mar = c(4,5,1,4),oma = c(2,0,0,2))
plot(temp,type = "l",
     xaxt = 'n',col = "grey",
     ylim = c(-15,30),xlab = "",
     ylab = "",las = 1,
     lty = "dashed", 
     cex.axis = 2,cex.lab = 2) #Air temperature
lines(obs$temp0.5,
      type = "l",lwd = 3,
      col = cols[2], lty = "longdash" ) #manure at 0.5m obs temperature
lines(obs$temp1.5,
      type = "l",lwd = 3,
      col = cols[3] , lty = "dotdash") #manure at 1.5m obs temperature
lines(obs$temp2.5,
      type = "l",lwd = 3,
      col = cols[4], lty = "dotted") #manure at 2.5m obs temperature
lines(obs$temp.avg,
      type = "l",lwd = 3,
      col = cols[1]) #manure avg. obs temperature
legend(-23,-12,c("Tair"),
       col = "grey",
       lty = 3,lwd = 3,
       bty = "n",cex = 1.5, x.intersp = 0) #160,30
legend(25,-12,c("Avg. Tm","Tm (0.5m)","Tm (1.5m)",
                 "Tm (2.5m)"),
       col = cols,
       lty = c(1,5,4,3),lwd = 3,
       bty = "n",cex = 1.5,xjust = 0,
       horiz = T, x.intersp = 0,
       text.width = 45) #160,30
for (i in 1:length(removal.a)) {#removal dates
  arrows(removal.a[i],-12,removal.a[i],-7) 
}
axis(side = 1, at = plot.day,
     cex.axis = 1.5,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = plot.date)
mtext(expression(paste("Temperature ( ",degree,"C)",
                sep = "")),
      side = 2,line = 3, cex = 1.5)
mtext("Date",side = 1,line = 4, cex = 1.5)
text(5,28, paste(Location," tank",sep = ""),
     cex = 1.5,pos = 4)
#Measured manure depth and temperature profile
if(Location == "VA"){
points(depth.m[,1],
       depth.m[,2]*(45/300)-15,cex = 2,lwd = 2, pch = 20)
axis(side = 4, at = c(-15,0,15,30),
       cex.axis = 1.5,lwd.ticks = 2,tck = -0.03,mgp = c(0,1,0),
       labels = c(0,100,200,300),las=1)
} else {
points(depth.m[,1],
         depth.m[,2]*(45/360)-15,cex = 2,lwd = 2, pch = 20)
axis(side = 4, at = c(-15,0,15,30),
       cex.axis = 1.5,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
       labels = c(0,120,240,360))
}
mtext("Manure depth (cm)",
      side = 4,line = 3.5, cex = 1.5)
}



#For simulated manure temperature
plotoutput <- function() {
par(mfrow = c(4,1), mar = c(4,8.5,1,6),oma = c(3,0,0,0))
#A. Temperature
plot(temp,type = "l",xaxt = 'n',col = "grey",ylim = c(-16,30)
     ,xlab = "",ylab = "",las = 1,cex.axis = 2,
     lty = "dashed", lwd = 3, font = 2) #Air temperature
lines(obs$temp.avg,type = "l",
      col = cols[1],lwd = 3) #manure avg. obs temperature
lines(sim.og$Temperature.C[1:length(temp)],
      col = cols[2],
      lwd = 3, lty = "dotted") #without shade calibration
lines(sim.re$Temperature.C[1:length(temp)],
      col = cols[2],
      lwd = 3)                #with shade calibration
legend(115,30,c("Tair","Tm measurement","Tm model","Tm revised model"),
       col = c("grey",cols[c(1,2,2)]),
       lty = c(2,1,3,1),lwd = 3,ncol = 2,
       bty = "n",cex = 2.5,
       title = "Average temperature", title.adj = 0.5)
axis(side = 1, at = plot.day,
     cex.axis = 2,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = plot.date, font = 2)
for (i in 1:length(removal.a)) {#removal dates
arrows(removal.a[i],-15,removal.a[i],-10) 
}
mtext(expression(paste("Temperature (",degree,"C)",sep = "")),side = 2,line = 3, cex = 2)
text(5,28, paste(Location," tank",sep = ""), cex = 2.5,pos = 4)

#B. Manure depth
plot(sim.re$Depth.cm,type = "l",
     ylim = c(0,350),xaxt = 'n',
     col = "black",xlab = "",
     ylab = "", las = 2,
     cex = 3, cex.lab = 2, cex.axis = 2, font = 2 )
points(depth.m[,1],
       depth.m[,2],cex = 2,lwd = 2, pch = 20)
for (i in 1:4) {#removal dates
  arrows(removal.a[i],5,removal.a[i],35)
}
axis(side = 1, at = plot.day,
     cex.axis = 2,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = plot.date, font = 2)
mtext("Date",line = 1 , cex = 2.5,side = 1,
      outer = T)
mtext("Depth (cm)",side = 2,cex = 2,line = 5)
text(5,320, paste(Location," tank",sep = ""), cex = 2.5,pos = 4)


#C. Solar radiation and albedo
plot(SR.og.cum,type = "l",
     col = cols[1],lty = "dotted",
     xaxt = "n",yaxt = "n",
     xlab = "" , ylab = "", 
     lwd = 2,ylim = c(0,3500),
     yaxs = "i")                 #original SR
lines(SR.cum,
      col = cols[1],lwd = 2)     #Revised SR
lines((1-sim.og$Solar.absorb)*1000,
      lty = 2, col = cols[3])       #original albedo
lines((1-sim.re$Solar.absorb)*1000,
      lty = 1, col = cols[3])       #revised albedo
axis(side = 1, at = plot.day,
     cex.axis = 2,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = plot.date, font = 2)
axis(side = 2, at = c(0,500,1000,1500,2000,2500,3000,3500)
     ,labels = c("0","500","1000","1500","2000",
                 "2500","3000","3500"),
     las = 2, cex.axis = 2, font = 2)
axis(side = 4, at = c(0,200,400,600,800,1000)
     ,labels = c("0","20","40","60","80","100"),
     las = 2, cex.axis = 2, font = 2)
mtext(expression(paste("Solar irradiation (MJ/ ",m^2,")",sep = "")),
                 side = 2,line = 5, cex = 2)
mtext("Albedo (%)",
      side = 4,line = 3.5, cex = 2)
legend(1,3250,
       c("Cumulative solar irradiation","Cumulative solar irradiation (revised)",
         "Surface albedo","Surface albedo (revised)"),
       col = cols[c(1,1,3,3)],
       lty = c(2,1,2,1),lwd = 2,bty = "n",
       cex = 2.5)
text(5,3300, paste(Location," tank",sep = ""), cex = 2.5,pos = 4)

#D. Evaporation, snow cover, precipitation 
plot(sim.re$snow.depth/10,
     type = "l",
     lty = "dashed",xaxt = "n",
     yaxt = "n",xlab = "" , 
     ylab = "", lwd = 2,
     ylim = c(0,5),yaxs = "i")
lines(sim.re$Precipitation.cm,
      lwd = 2,col = cols[1])
lines(Eva.og.cum/20,
      lwd = 2, col = cols[4],
      lty = 2)
lines(Eva.cum/20,
      lwd = 2, col = cols[4])
axis(side = 1, at = plot.day,
     cex.axis = 2,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = plot.date, font = 2)
axis(side = 2, at = c(0,1,2,3,4,5),
     labels = c("0","10","20","30","40","50"),
     las = 2, cex.axis = 2, font = 2)
axis(side = 4, at = c(0,1,2,3,4,5)
     ,labels = c("0","20","40","60","80","100"),
     las = 2, cex.axis = 2, font = 2)
mtext("Precipitation/ snow cover (cm)",
      side = 2,line = 5, cex = 2)
mtext("Evaporation (cm)",
      side = 4,line = 4, cex = 2)
legend(5,4.5,
       c("Cumulative evaporation","Cumulative evaporation (revised model)",
         "Precipitation","Snow cover"),
       col = cols[c(4,4,1,1)],
       lty = c(2,1,1,2),lwd = 2,bty = "n",
       cex = 2.5)
text(5,4.7, paste(Location," tank",sep = ""), cex = 2.5,pos = 4)

}


#Measurement data
png(file = paste(result,Location,"/figures/png/",Location,"_measurement.png",sep = "")
    ,width = 1000, height = 600)
plot.measurement()
dev.off()

emf(file = paste(result,Location,"/figures/",Location,"_measurement.emf",sep = "")
    ,width = 10, height = 6,emfPlus = FALSE, family = "Calibri")
plot.measurement()
dev.off()

#Simulated data
png(file = paste(result,Location,"/figures/png/",Location,"_",test,".png",sep = "")
    ,width = 1200, height = 2400)
plotoutput()
dev.off()

emf(file = paste(result,Location,"/figures/",Location,"_",test,".emf",sep = "")
    ,width = 12, height = 24,emfPlus = FALSE, family = "Calibri")
plotoutput()
dev.off()



