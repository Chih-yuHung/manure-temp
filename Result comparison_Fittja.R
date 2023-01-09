library(devEMF)
library(tidyverse)
library(dplyr)
library(hydroGOF) #NSE 
#Sweden project-Fittja
#To compare my simulation result to the measured data

#output to an excel file
Envir.daily <- read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/2. Method/input/daily env input_Fittja_May1.csv",header = T)
temp <- ((Envir.daily$AirTmax1 + Envir.daily$AirTmin1)/2)[731:1095] #Air Temp.avg

#observed data
result <- "C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/"
obs.Fittja <- read.csv(paste(result,"temp.Fittja.daily.csv",sep = ""),header = T) 

#simulated data before calibration
sim.Fittja.og <- read.csv(paste(result,"Fittja/original/",
                                Location,"_",test,".csv",sep = ""),header = T) 
SR.og <- sim.Fittja.og$total.radiation/12/277.77778
SR.og.cum <- cumsum(SR.og)
#simulated data after calibration and modification
sim.Fittja <- read.csv(paste(result,"Fittja/with shade/",
                             Location,"_",test,".csv",sep = ""),header = T)
SR <- sim.Fittja$total.radiation/12/277.77778
SR.cum <- cumsum(SR)
sim.Fittja$snow.depth[sim.Fittja$snow.depth == 0] <- NA 
#obtain removal days
removal.a <- removal.start[1:4] - as.numeric(as.Date(start.date)) + 1

#For measured manure temperature
#png(file = paste(result,"Fittja/figures/",Location,"_",test,".png",sep = "")
#    ,width = 1200, height = 1800)
#emf(file = paste(result,"Fittja/figures/",Location,"_",test,".emf",sep = "")
#    ,width = 12, height = 18,emfPlus = FALSE, family = "Calibri")
plotoutput <- function() {
par(mfrow = c(3,1), mar = c(4,8,1,7),oma = c(3,0,0,0))
#A. Temperature
plot(temp,type = "l",xaxt = 'n',col = "grey",ylim = c(-15,30)
     ,xlab = "",ylab = "",las = 1,cex.axis = 2) #Air temperature
lines(obs.Fittja$temp.avg,type = "l",lwd = 2) #manure avg. obs temperature
lines(sim.Fittja.og$Temperature.C,type = "l",col = "blue",lwd = 2) #without shade calibration
lines(sim.Fittja$Temperature.C,col = "red",lwd = 2)
lines(obs.Fittja$temp0.5,type = "l",lwd = 2, lty = 3) #manure avg. obs temperature
lines(sim.Fittja.og$temp.05,type = "l",col = "blue",lwd = 2, lty = 3) #without shade calibration
lines(sim.Fittja$temp.05,col = "red",lwd = 2, lty = 3)
legend(10,-3.5,c("Tair","Tm meausrement","Tm model","Tm revised model"),
       col = c("grey","black","blue","red"),
       lty = 1,lwd = 2,ncol = 2,
       bty = "n",cex = 2.5,
       title = "Average temperature", title.adj = 0)
legend(180,30,c("Tm measurement","Tm model","Tm revised model"),
       col = c("black","blue","red"),
       lty = 3,lwd = 2,ncol = 1,
       bty = "n",cex = 2.5, 
       title = "Temperature at 0.5 m depth ")
axis(side = 1, at = c(1,93,185,277,365),las = 1,
     labels = rep("",5),lwd.ticks = 2, tck = -0.03)
for (i in 1:4) {#removal dates
arrows(removal.a[i],-5,removal.a[i],-1) 
}
mtext(expression(paste("Temperature (",degree,"C)",sep = "")),side = 2,line = 3, cex = 2)
text(5,28,"Örsundsbro (OR) site", cex = 2.5,pos = 4)
#B. Solar radiation, precipitation
plot(SR.og.cum,type = "l",col = "blue",lty = "dashed",xaxt = "n",
     yaxt = "n",xlab = "" , ylab = "", lwd = 2,
     ylim = c(0,3500),yaxs = "i")
lines(SR.cum,col = "red",lty = "dashed", lwd = 2)
axis(side = 1, at = c(1,93,185,277,365),las = 1,
     labels = rep("",5),lwd.ticks = 2, tck = -0.03)
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
lines(sim.Fittja$Precipitation.cm*500, lwd = 2 )
lines(sim.Fittja$snow.depth*50,lwd = 2,col = "red")
#C. Manure depth
plot(sim.Fittja$Depth.cm,type = "l",
     ylim = c(0,350),xaxt = 'n',
     col = "black",xlab = "",
     ylab = "", las = 2,
     cex = 3, cex.lab = 2, cex.axis = 2 )
#retrieved from measurement data
points(c(1,49,116,130,174,301,307),
       c(67,242,310,190,73,280,230))
for (i in 1:4) {#removal dates
  arrows(removal.a[i],20,removal.a[i],50) 
}
axis(side = 1, at = c(1,93,185,277,365),
     cex.axis = 2,lwd.ticks = 2,tck = -0.03,mgp = c(0,2,0),
     labels = c("5/1, 2020","8/1, 2020","11/1, 2020","2/1, 2021","5/1, 2021"))
mtext("Date",line = 1 , cex = 2.5,side = 1,
      outer = T)
mtext("Depth (cm)",side = 2,cex = 2,line = 5)
abline(h = 0.5*Htank*100)
abline(v = c(49,144,235,326))
}



png(file = paste(result,"Fittja/figures/png/",Location,"_",test,".png",sep = "")
    ,width = 1200, height = 1800)
plotoutput()
dev.off()

emf(file = paste(result,"Fittja/figures/",Location,"_",test,".emf",sep = "")
    ,width = 12, height = 18,emfPlus = FALSE, family = "Calibri")
plotoutput()
dev.off()
