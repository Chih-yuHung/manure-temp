#This script calculate a table for measured and simulate data.
#Site: Fittja,i.e. Orsundsbro(OR), site.
#Four stats: RMSE, D, R2 and bias
#five time period: annual, summer, fall, winter, spring. 
#Spring: 80-171 (March 20, 2020-June 19,2020), Summer: 172-265 (June 20,2020,Sept 21)
#Fall: 266-355 (Sept 22, 2020-Dec 20,2020), Winter:356-79 (Dec 21,2020-Mar 20, 2021)
#2020 is a leap year
library(lubridate)

#force the simulation length = observation
obs.n <- ifelse(Location == "OR", 303, 312)
obs <- obs[1:obs.n,]
sim.og <- sim.og[1:obs.n,]
sim.re <- sim.re[1:obs.n,]
Envir.obs <- Envir.daily[1:obs.n,]
#remove the effect of leap year on DOY
obs$DOY[which(leap_year(obs$Year))] <- obs$DOY[which(leap_year(obs$Year))]-1

#organize data for the last year and seasons
#Observed manure temperature
obs[,9] <- obs$Depth*100
colnames(obs)[9] <- "Depth.cm"

Spring.obs <- obs[80 <= obs$DOY & obs$DOY <= 171,]
Summer.obs <- obs[172 <= obs$DOY & obs$DOY <= 265,]
Fall.obs   <- obs[266 <= obs$DOY & obs$DOY <= 355,]
Winter.obs <- rbind(obs[356 <= obs$DOY,],
                          obs[obs$DOY <= 79,])
obs.y      <- list(obs[1:obs.n,], 
                         Spring.obs, Summer.obs
                         ,Fall.obs, Winter.obs)
#Avg. temp for Table S2
obs.avg <- data.frame(temp0.5 = rep(0,5),
                      temp1.5 = rep(0,5),
                      temp2.5 = rep(0,5),
                      tempavg = rep(0,5),
                      row.names = c("year","spring",
                                    "summer","fall",
                                    "winter"))
for (i in 1:5) {
obs.avg[i,1:3] <- round(colMeans(obs.y[[i]][,6:8]),1)
obs.avg[i,4] <- round(mean(obs.y[[i]][,13]),1)
}

#The simulation results from original model 
Spring.sim.og <- sim.og[80 <= sim.og$DOY & sim.og$DOY <= 171,]
Summer.sim.og <- sim.og[172 <= sim.og$DOY & sim.og$DOY <= 265,]
Fall.sim.og   <- sim.og[266 <= sim.og$DOY & sim.og$DOY <= 355,]
Winter.sim.og <- rbind(sim.og[356 <= sim.og$DOY,],
                      sim.og[sim.og$DOY <= 79,])
sim.og.y <- list(sim.og[1:obs.n,],
                       Spring.sim.og, Summer.sim.og
                       ,Fall.sim.og, Winter.sim.og)

#Avg. temp for Table S2
sim.og.avg <- data.frame(temp0.5 = rep(0,5),
                         temp1.5 = rep(0,5),
                         temp2.5 = rep(0,5),
                         tempavg = rep(0,5),
                         row.names = c("year","spring",
                                    "summer","fall",
                                    "winter"))
for (i in 1:5) {
  sim.og.avg[i,1:3] <- round(colMeans(sim.og.y[[i]][,13:15]),1)
  sim.og.avg[i,4] <- round(mean(sim.og.y[[i]][,6]),1)
}


#The simulation results with revised model
Spring.sim <- sim.re[80 <= sim.re$DOY & sim.re$DOY <= 171,]
Summer.sim <- sim.re[172 <= sim.re$DOY & sim.re$DOY <= 265,]
Fall.sim   <- sim.re[266 <= sim.re$DOY & sim.re$DOY <= 355,]
Winter.sim <- rbind(sim.re[356 <= sim.re$DOY,],
                      sim.re[sim.re$DOY <= 79,])
sim.re.y <- list(sim.re[1:obs.n,], 
                    Spring.sim, Summer.sim
                    ,Fall.sim, Winter.sim)

#Avg. temp for Table S2
sim.avg <- data.frame(temp0.5 = rep(0,5),
                      temp1.5 = rep(0,5),
                      temp2.5 = rep(0,5),
                      tempavg = rep(0,5),
                      row.names = c("year","spring",
                                    "summer","fall",
                                    "winter"))
for (i in 1:5) {
  sim.avg[i,1:3] <- round(colMeans(sim.re.y[[i]][,13:15]),1)
  sim.avg[i,4] <- round(mean(sim.re.y[[i]][,6]),1)
}


#Descriptive stat data
summary(obs$temp0.5) #VA:-0.8 to 20.3, OR:-0.4 to 21.4
summary(obs$temp2.5) #VA:2.5 to 16.5, OR: 0.6 to 19.7
summary(obs$temp.avg) #VA: 8.6, OR: 10.7
mean(Summer.obs$temp.avg,na.rm=T) #Summer temperature VA:15.4, OR:17.2
#This is adjust to obtian air temperature during study period only.
Spring.air <- Envir.obs[80 <= Envir.obs$DOY & Envir.obs$DOY <= 171,]
mean((Spring.air$AirTmax1+Spring.air$AirTmin1)/2) #VA:5.4   OR:11.0
Summer.air <- Envir.obs[172 <= Envir.obs$DOY & Envir.obs$DOY <= 265,]
mean((Summer.air$AirTmax1+Summer.air$AirTmin1)/2) #VA:15.0   OR:16.2
Fall.air <- Envir.obs[266 <= Envir.obs$DOY & Envir.obs$DOY <= 355,]
mean((Fall.air$AirTmax1+Fall.air$AirTmin1)/2) #VA:6.2   OR:6.7
Winter.air <- rbind(Envir.obs[356 <= Envir.obs$DOY,],
                    Envir.obs[Envir.obs$DOY <= 79,])
mean((Winter.air$AirTmax1+Winter.air$AirTmin1)/2) #VA:-2.2   OR:-2.6
mean((Envir.obs$AirTmax1+Envir.obs$AirTmin1)/2) #VA: 6.4 OR:8.3

#A table for RMSE, d, R2, bias
stat.avg <- data.frame(Depth = c("sample size",rep(c("Avg.","0.5 m","1.5 m", "2.5 m"),each = 4)),
                      stat.name = c("",rep(c("RMSE","D","R2","Bias"), 4)),
                      year.og = NA,year = NA,
                      spring.og = NA,spring = NA,
                      summer.og = NA,summer = NA,
                      fall.og = NA,fall = NA,
                      winter.og = NA,winter = NA,
                      stringsAsFactors = FALSE
                      )
#the sample size
for (i in 1:5) {
stat.avg[1, c(2 * i + 1)] <- sum(!is.na(obs.y[[i]]$temp.avg)) 
}

#RMSE caculation only for the last year
RMSE <- function(x,y){
  xy <- na.omit(cbind(x,y)) #to remove NA if need
  round(sqrt(sum((xy[,1] - xy[,2])^2)/length(xy[,1])),2)
}

#D function
D <- function(x,y){
  xy <- na.omit(cbind(x,y))
  ybar <- mean(xy[,2])
  round(1 - (sum((xy[,1] - xy[,2])^2)/sum((abs(xy[,1] - ybar) + abs(xy[,2] - ybar))^2)),2)
}

#R2 valeus
rsq <- function(x, y) {
  xy <- na.omit(cbind(x,y))
  round(cor(xy[,1], xy[,2]) ^ 2,2)
  }

#average bias valeus
bias <- function(x, y) {
  xy <- na.omit(cbind(x,y))
  round(sum(xy[,1] - xy[,2])/length(xy[,1]),2)
  }

stat <- list(RMSE,D,rsq,bias)

#stat by year and seasons for the original model 
for (i in 1:16) {
  for (j in 1:5) {
    if (i <= 4) {
    stat.avg[i + 1,2*j + 1] <- stat[[i]](sim.og.y[[j]]$Temperature.C
                          ,obs.y[[j]]$temp.avg)
    } else if (4 < i & i <= 8) {
    stat.avg[i + 1,2*j + 1] <- stat[[i - 4]](sim.og.y[[j]]$temp.05
                                   ,obs.y[[j]]$temp0.5)  
    } else if (8 < i & i <= 12) {
    stat.avg[i + 1,2*j + 1] <- stat[[i - 8]](sim.og.y[[j]]$temp.15
                                   ,obs.y[[j]]$temp1.5)  
    } else {
    stat.avg[i + 1,2*j + 1] <- stat[[i - 12]](sim.og.y[[j]]$temp.25
                                   ,obs.y[[j]]$temp2.5)  
    }
  }  
}

#stat by year and seasons for the REVISED model 
for (i in 1:16) {
  for (j in 1:5) {
    if (i <= 4) {
      stat.avg[i + 1,2*j + 2] <- stat[[i]](sim.re.y[[j]]$Temperature.C
                                   ,obs.y[[j]]$temp.avg)
    } else if (4 < i & i <= 8) {
      stat.avg[i + 1,2*j + 2] <- stat[[i - 4]](sim.re.y[[j]]$temp.05
                                     ,obs.y[[j]]$temp0.5)  
    } else if (8 < i & i <= 12) {
      stat.avg[i + 1,2*j + 2] <- stat[[i - 8]](sim.re.y[[j]]$temp.15
                                     ,obs.y[[j]]$temp1.5)  
    } else {
      stat.avg[i + 1,2*j + 2] <- stat[[i - 12]](sim.re.y[[j]]$temp.25
                                      ,obs.y[[j]]$temp2.5)  
    }
  }  
}

#The table to show the improvement between shallow and deep manure depth
#s means shallow < 50% depth, d means deep >= 50% depth, OG = original model, Mod = modified model
stat.avg.depth <- data.frame(stat.name = c("sample size","RMSE","D","R2","Bias"),
                       OG.s = c(1:5),Mod.s = c(1:5),
                       OG.d = c(1:5),Mod.d = c(1:5),
                       stringsAsFactors = FALSE
)

H.50 <- 0.5 * Htank * 100 #convert 50% tank height to cm 
#obtain the days have <50% and >=50% depth
og.s <- which(sim.og.y[[1]]$Depth.cm < H.50)
og.d <- which(sim.og.y[[1]]$Depth.cm >= H.50)
mod.s <- which(sim.re.y[[1]]$Depth.cm < H.50)
mod.d <- which(sim.re.y[[1]]$Depth.cm >= H.50)

#sample size
stat.avg.depth[1,2:5] <- round(c(length(og.s),length(mod.s),length(og.d),length(mod.d)),0) 

#stat by year and seasons for the original model 
for (i in 1:4) {
  stat.avg.depth[i + 1,2] <- stat[[i]](sim.og.y[[1]]$Temperature.C[og.s]
                                       ,obs.y[[1]]$temp.avg[og.s])
  stat.avg.depth[i + 1,3] <- stat[[i]](sim.re.y[[1]]$Temperature.C[mod.s]
                                   ,obs.y[[1]]$temp.avg[mod.s])
  stat.avg.depth[i + 1,4] <- stat[[i]](sim.og.y[[1]]$Temperature.C[og.d]
                                  ,obs.y[[1]]$temp.avg[og.d]) 
  stat.avg.depth[i + 1,5] <- stat[[i]](sim.re.y[[1]]$Temperature.C[mod.d]
                                   ,obs.y[[1]]$temp.avg[mod.d]) 
  }


# Summary for the results, part 2. Only output after all simulation is done. 
#The data of manure tank, i.e output L1:M18
Output.tank <- data.frame(matrix(ncol = 2,nrow = 29))
Output.tank[1:29,1] <- c("Location","SurfaceArea(m2)","Starting.Depth(m)"
                         ,"Starting.Volume.m3","Total.Solids(%)",""
                         ,"Tank.Storage","Total.Tank.Volume(m3)"
                         ,"Yearly Maximum Storage Volume.m3","Yearly.Rain.Volume.m3"
                         ,"Yearly.Manure.Storage.Volume.m3","Tank.Diameter.m",""
                         ,"Average.Tm.C","Max.Tm.C","Min.Tm.C","","Max.d.cm","annual snow.cm"
                         ,"Summer.Tm.C","Summer.Tm.re.C","Summer.Tm.me.C"
                         ,"Winter.Tm.C","Winter.Tm.re.C","Winter.Tm.me.C"
                         ,"Annual.Tm.C","Annual.Tm.re.C","Annual.Tm.me.C"
                         ,"Total radiation")
Output.tank[1,2] <- Location
Output.tank[2:3,2] <- c(Au,M.depth)        #area and initial depth, m2 and m
Output.tank[4,2] <- as.numeric(Output.tank[2,2])*as.numeric(Output.tank[3,2]) #starting volume.
Output.tank[5,2] <- Total.solid                 #%
Output.tank[c(6,7,13,17),2] <- ""                      #blank
Output.tank[8,2] <- Tank.v                      #Total Tank volume, m3
Output.tank[9,2] <- Max.storage                 #yearly manure storage,m3
Output.tank[10,2] <- Rain.v                     #yearly rain volume in storage,m3
Output.tank[11,2] <- M.storage                  #yearly Manure storage,m3
Output.tank[12,2] <- ri*2                       #Diameter of tank,m
Output.tank[14,2] <- mean(Output$Temperature.C) #Avg. manure Temperature for the estimate years
Output.tank[15,2] <- max(Output$Temperature.C)  #Max manure Temperature
Output.tank[16,2] <- min(Output$Temperature.C)  #Min manure Temperature
Output.tank[18,2] <- max(Output$Depth.cm)       #Maximum Manure Depth
Output.tank[19,2] <- sum(Output$`snow depth`)       #Maximum Manure Depth
Output.tank[c(20,21,22),2] <- c(mean(Summer.sim.og$Temperature.C),mean(Summer.sim$Temperature.C),
                                mean(Summer.obs$temp.avg))
Output.tank[c(23,24,25),2] <- c(mean(Winter.sim.og$Temperature.C),mean(Winter.sim$Temperature.C),
                                mean(Winter.obs$temp.avg))
Output.tank[c(26,27,28),2] <- c(mean(sim.og$Temperature.C),mean(sim.re$Temperature.C),
                                mean(obs$temp.avg))
Output.tank[29,2] <- sum(Output$`total radiation`)/12/277.77778

#write the results out
library(xlsx)
manure.pic <- paste(result,Location,"/figures/png/",Location,"_",test,".png",sep = "")
wb <- createWorkbook()
sheet <- createSheet(wb, "pic")
addPicture(manure.pic, sheet, startRow = 1, startColumn = 1)
saveWorkbook(wb, file = paste(result,Location,"/stat/",Location,"_",
test,".xlsx",sep = ""), password = NULL)
write.xlsx(sim.og,
           file = paste(result,Location,"/stat/",Location,"_",
                        test,".xlsx",sep = ""),
           sheetName = "Output.og", row.names = F, append = TRUE)
write.xlsx(sim.re,
             file = paste(result,Location,"/stat/",Location,"_",
                          test,".xlsx",sep = ""),
             sheetName = "Output.re", row.names = F, append = TRUE)

write.xlsx(format(stat.avg,digit = 2),
           file = paste(result,Location,"/stat/",Location,"_",
                        test,".xlsx",sep = ""),
               sheetName = "overall stat", row.names = F, append = TRUE)
write.xlsx(format(stat.avg.depth,digit =2),
           file = paste(result,Location,"/stat/",Location,"_",
                        test,".xlsx",sep = ""),
           sheetName = "stat by depth", row.names = F,append = TRUE)
write.xlsx(Output.tank,
           file = paste(result,Location,"/stat/",Location,"_",
                        test,".xlsx",sep = ""),
           sheetName = "output summary_revised model", row.names = F,append = TRUE)
write.xlsx(parameters,
           file = paste(result,Location,"/stat/",Location,"_",
                        test,".xlsx",sep = ""),
           sheetName = "Parameters", row.names = F,append = TRUE, showNA = FALSE)

