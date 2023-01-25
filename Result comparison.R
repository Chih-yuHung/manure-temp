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

#simulated data before calibration
sim.og <- read.csv(paste(result,Location,"/original/",
                                Location,"_",test,".csv",sep = ""),header = T) 
SR.og <- sim.og$total.radiation/12/277.77778
SR.og.cum <- cumsum(SR.og)
#simulated data after calibration and modification
sim.re <- read.csv(paste(result,Location,"/with shade/",
                             Location,"_",test,".csv",sep = ""),header = T)
SR <- sim.re$total.radiation/12/277.77778
SR.cum <- cumsum(SR)
sim.re$snow.depth[sim.re$snow.depth == 0] <- NA 
#obtain removal days
removal.a <- removal.start[1:4] - as.numeric(as.Date(start.date)) + 1

#two data.frame for the days in VA and OR sites
OR.date <- data.frame(day = c(1,93,185,277,365), date = c("5/1, 2020","8/1, 2020","11/1, 2020","2/1, 2021","5/1, 2021"))
VA.date <- data.frame(day = c(1,76,167,257,349), date = c("6/18, 2020","9/1, 2020","12/1, 2020","3/1, 2021","6/1, 2021"))

if (Location == "OR") {
  plot.day <- OR.date$day
  plot.date <- OR.date$date
} else {
  plot.day <- VA.date$day
  plot.date <- VA.date$date
}

#For measured manure temperature
plotoutput <- function() {
par(mfrow = c(3,1), mar = c(4,8,1,7),oma = c(3,0,0,0))
#A. Temperature
plot(temp,type = "l",xaxt = 'n',col = "grey",ylim = c(-15,30)
     ,xlab = "",ylab = "",las = 1,cex.axis = 2) #Air temperature
lines(obs$temp.avg,type = "l",lwd = 2) #manure avg. obs temperature
lines(sim.og$Temperature.C[1:nrow(obs)],type = "l",col = "blue",lwd = 2) #without shade calibration
lines(sim.re$Temperature.C[1:nrow(obs)],col = "red",lwd = 2)
lines(sim.re$In.M.temp[1:nrow(obs)],col = "red",lwd = 2,lty = 3)
#lines(obs$temp0.5,type = "l",lwd = 2, lty = 3) #manure avg. obs temperature
#lines(sim.og$temp.05,type = "l",col = "blue",lwd = 2, lty = 3) #without shade calibration
#lines(sim.re$temp.05,col = "red",lwd = 2, lty = 3)
legend(10,-3.5,c("Tair","Tm meausrement","Tm model","Tm revised model","Incoming manure"),
       col = c("grey","black","blue","red","red"),
       lty = c(1,1,1,1,3),lwd = 2,ncol = 2,
       bty = "n",cex = 2.5,
       title = "Average temperature", title.adj = 0)
#legend(180,30,c("Tm measurement","Tm model","Tm revised model"),
#       col = c("black","blue","red"),
#       lty = 3,lwd = 2,ncol = 1,
#       bty = "n",cex = 2.5, 
#       title = "Temperature at 0.5 m depth ")
axis(side = 1, at = plot.day,
     cex.axis = 2,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = plot.date)
for (i in 1:4) {#removal dates
arrows(removal.a[i],-5,removal.a[i],-1) 
}
mtext(expression(paste("Temperature (",degree,"C)",sep = "")),side = 2,line = 3, cex = 2)
text(5,28, paste(Location," site",sep = ""), cex = 2.5,pos = 4)
#B. Solar radiation, precipitation
plot(SR.og.cum,type = "l",col = "blue",lty = "dashed",xaxt = "n",
     yaxt = "n",xlab = "" , ylab = "", lwd = 2,
     ylim = c(0,3500),yaxs = "i")
lines(SR.cum,col = "red",lty = "dashed", lwd = 2)
axis(side = 1, at = plot.day,
     cex.axis = 2,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = plot.date)
axis(side = 2, at = c(0,500,1000,1500,2000,2500,3000,3500)
     ,labels = c("0","500","1000","1500","2000",
                 "2500","3000","3500"),
     las = 2, cex.axis = 2)
axis(side = 4, at = c(0,500,1000,1500,2000)
     ,labels = c("0","1","2","3","4"),
     las = 2, cex.axis = 2)
mtext(expression(paste("Solar irradiation (MJ/ ",m^2,")",sep = "")),
                 side = 2,line = 5, cex = 2)
mtext("Precipitation/",
      side = 4,line = 3.5, cex = 2)
mtext("Snow water equivalent (cm)",
      side = 4,line = 5.5, cex = 2)
legend(5,3500,
       c("cumulative solar irradiation","cumulative solar irradiation (revised model)",
         "precipitation","snow water equivalent"),
       col = c("blue","red","black","red"),
       lty = c(2,2,1,1),lwd = 2,bty = "n",
       cex = 2.5)
lines(sim.re$Precipitation.cm*500, lwd = 2 )
lines(sim.re$snow.depth*50,lwd = 2,col = "red")
#C. Manure depth
plot(sim.re$Depth.cm,type = "l",
     ylim = c(0,350),xaxt = 'n',
     col = "black",xlab = "",
     ylab = "", las = 2,
     cex = 3, cex.lab = 2, cex.axis = 2 )
#retrieved from measurement data
depth.m <- read.csv(paste(result,Location,"/",Location,".depth.csv",sep = ""),header = T)
points(depth.m[,1],
       depth.m[,2])
for (i in 1:4) {#removal dates
  arrows(removal.a[i],20,removal.a[i],50) 
}
axis(side = 1, at = plot.day,
     cex.axis = 2,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = plot.date)
mtext("Date",line = 1 , cex = 2.5,side = 1,
      outer = T)
mtext("Depth (cm)",side = 2,cex = 2,line = 5)
#abline(h = 0.5*Htank*100)
#abline(v = c(49,144,235,326))
}



png(file = paste(result,Location,"/figures/png/",Location,"_",test,".png",sep = "")
    ,width = 1200, height = 1800)
plotoutput()
dev.off()

emf(file = paste(result,Location,"/figures/",Location,"_",test,".emf",sep = "")
    ,width = 12, height = 18,emfPlus = FALSE, family = "Calibri")
plotoutput()
dev.off()
