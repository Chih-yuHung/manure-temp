#This script calculate a table for measured and simulate data.
#Site: Fittja,i.e. Orsundsbro(OR), site.
#Four stats: RMSE, D, R2 and bias
#five time period: annual, summer, fall, winter, spring. 
#Spring: 122-171 (May 1, 2020-June 19,2020), Summer: 172-265 (June 20,2020,Sept 21)
#Fall: 266-355 (Sept 22, 2020-Dec 20,2020), Winter:356-58 (Dec 21,2020-Feb 27, 2021)
#2020 is a leap year

#organize data for the last year and seasons
#Observed manure temperature
Spring.obs.Fittja <-obs.Fittja[122 <= obs.Fittja$DOY & obs.Fittja$DOY <= 171,]
Summer.obs.Fittja <-obs.Fittja[172 <= obs.Fittja$DOY & obs.Fittja$DOY <= 265,]
Fall.obs.Fittja   <-obs.Fittja[266 <= obs.Fittja$DOY & obs.Fittja$DOY <= 355,]
Winter.obs.Fittja <-rbind(obs.Fittja[356 <= obs.Fittja$DOY,],
                          obs.Fittja[obs.Fittja$DOY <= 58,])
obs.Fittja.y      <-list(obs.Fittja[1:303,], 
                         Spring.obs.Fittja, Summer.obs.Fittja
                         ,Fall.obs.Fittja, Winter.obs.Fittja)

#The simulation results from original model 
Spring.sim.og <-sim.Fittja.og[122 <= sim.Fittja.og$DOY & sim.Fittja.og$DOY <= 171,]
Summer.sim.og <-sim.Fittja.og[172 <= sim.Fittja.og$DOY & sim.Fittja.og$DOY <= 265,]
Fall.sim.og   <-sim.Fittja.og[266 <= sim.Fittja.og$DOY & sim.Fittja.og$DOY <= 355,]
Winter.sim.og <-rbind(sim.Fittja.og[356 <= sim.Fittja.og$DOY,],
                      sim.Fittja.og[sim.Fittja.og$DOY <= 59,])
sim.Fittja.og.y <-list(sim.Fittja.og[1:303,],
                       Spring.sim.og, Summer.sim.og
                       ,Fall.sim.og, Winter.sim.og)

#The simulation results with revised model
Spring.sim <-sim.Fittja[122 <= sim.Fittja$DOY & sim.Fittja$DOY <= 171,]
Summer.sim <-sim.Fittja[172 <= sim.Fittja$DOY & sim.Fittja$DOY <= 265,]
Fall.sim   <-sim.Fittja[266 <= sim.Fittja$DOY & sim.Fittja$DOY <= 355,]
Winter.sim <-rbind(sim.Fittja[356 <= sim.Fittja$DOY,],
                      sim.Fittja[sim.Fittja$DOY <= 59,])
sim.Fittja.y <-list(sim.Fittja[1:303,], 
                    Spring.sim, Summer.sim
                    ,Fall.sim, Winter.sim)

#A table for RMSE, d, R2 
stat.avg<-data.frame(Depth = rep(c("Avg.","0.5 m","1.5 m", "2.5 m"),each = 4)
                      ,stat.name = rep(c("RMSE","D","R2","Bias"), 4)
                      ,year.og=c(1:16),year=c(1:16)
                      ,spring.og=c(1:16),spring=c(1:16)
                      ,summer.og=c(1:16),summer=c(1:16)
                      ,fall.og=c(1:16),fall=c(1:16)
                      ,winter.og=c(1:16),winter=c(1:16)
                      ,stringsAsFactors=FALSE
                      )
#RMSE caculation only for the last year
RMSE<-function(x,y){
 round(sqrt(sum((x-y)^2)/length(x)),2)
}

#D function
D<-function(x,y){
  x<-na.omit(x)
  y<-na.omit(y)
  ybar<-mean(y)
  round(1-(sum((x-y)^2)/sum((abs(x-ybar)+abs(y-ybar))^2)),2)
}

#R2 valeus
rsq <- function(x, y) {
  x<-na.omit(x)
  y<-na.omit(y)
  round(cor(x, y) ^ 2,2)
  }

#average bias valeus
bias <- function(x, y) {
  x<-na.omit(x)
  y<-na.omit(y)
  round(sum(x-y)/length(x),2)
  }

stat<-list(RMSE,D,rsq,bias)

#stat by year and seasons for the original model 
for (i in 1:16) {
  for (j in 1:5) {
    if (i <=4){
    stat.avg[i,2*j+1]<-stat[[i]](sim.Fittja.og.y[[j]]$Temperature.C
                          ,obs.Fittja.y[[j]]$temp.avg)
    } else if (4 < i & i <= 8) {
    stat.avg[i,2*j+1]<-stat[[i-4]](sim.Fittja.og.y[[j]]$temp.05
                                   ,obs.Fittja.y[[j]]$temp0.5)  
    } else if (8 < i & i <= 12){
    stat.avg[i,2*j+1]<-stat[[i-8]](sim.Fittja.og.y[[j]]$temp.15
                                   ,obs.Fittja.y[[j]]$temp1.5)  
    } else {
    stat.avg[i,2*j+1]<-stat[[i-12]](sim.Fittja.og.y[[j]]$temp.25
                                   ,obs.Fittja.y[[j]]$temp2.5)  
    }
  }  
}

#stat by year and seasons for the REVISED model 
for (i in 1:16) {
  for (j in 1:5) {
    if (i <=4){
      stat.avg[i,2*j+2]<-stat[[i]](sim.Fittja.y[[j]]$Temperature.C
                                   ,obs.Fittja.y[[j]]$temp.avg)
    } else if (4 < i & i <= 8) {
      stat.avg[i,2*j+2]<-stat[[i-4]](sim.Fittja.y[[j]]$temp.05
                                     ,obs.Fittja.y[[j]]$temp0.5)  
    } else if (8 < i & i <= 12){
      stat.avg[i,2*j+2]<-stat[[i-8]](sim.Fittja.y[[j]]$temp.15
                                     ,obs.Fittja.y[[j]]$temp1.5)  
    } else {
      stat.avg[i,2*j+2]<-stat[[i-12]](sim.Fittja.y[[j]]$temp.25
                                      ,obs.Fittja.y[[j]]$temp2.5)  
    }
  }  
}


#write the results out
library(xlsx)
write.xlsx(stat.avg,
    file=paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/stat/"
               ,Sys.Date(),"_OR.xlsx",sep=""), sheetName="OR site", row.names=F)
