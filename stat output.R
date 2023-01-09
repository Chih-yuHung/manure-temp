#This script calculate a table for measured and simulate data.
#Site: Fittja,i.e. Orsundsbro(OR), site.
#Four stats: RMSE, D, R2 and bias
#five time period: annual, summer, fall, winter, spring. 
#Spring: 122-171 (May 1, 2020-June 19,2020), Summer: 172-265 (June 20,2020,Sept 21)
#Fall: 266-355 (Sept 22, 2020-Dec 20,2020), Winter:356-58 (Dec 21,2020-Feb 27, 2021)
#2020 is a leap year

#organize data for the last year and seasons
#Observed manure temperature
obs.Fittja[,9] <- obs.Fittja$Depth*100
colnames(obs.Fittja)[9] <- "Depth.cm"
obs.n <- nrow(na.omit(obs.Fittja))
Spring.obs.Fittja <- obs.Fittja[122 <= obs.Fittja$DOY & obs.Fittja$DOY <= 171,]
Summer.obs.Fittja <- obs.Fittja[172 <= obs.Fittja$DOY & obs.Fittja$DOY <= 265,]
Fall.obs.Fittja   <- obs.Fittja[266 <= obs.Fittja$DOY & obs.Fittja$DOY <= 355,]
Winter.obs.Fittja <- rbind(obs.Fittja[356 <= obs.Fittja$DOY,],
                          obs.Fittja[obs.Fittja$DOY <= 58,])
obs.Fittja.y      <- list(obs.Fittja[1:obs.n,], 
                         Spring.obs.Fittja, Summer.obs.Fittja
                         ,Fall.obs.Fittja, Winter.obs.Fittja)
length(na.omit(obs.Fittja$temp.avg))
#The simulation results from original model 
Spring.sim.og <- sim.Fittja.og[122 <= sim.Fittja.og$DOY & sim.Fittja.og$DOY <= 171,]
Summer.sim.og <- sim.Fittja.og[172 <= sim.Fittja.og$DOY & sim.Fittja.og$DOY <= 265,]
Fall.sim.og   <- sim.Fittja.og[266 <= sim.Fittja.og$DOY & sim.Fittja.og$DOY <= 355,]
Winter.sim.og <- rbind(sim.Fittja.og[356 <= sim.Fittja.og$DOY,],
                      sim.Fittja.og[sim.Fittja.og$DOY <= 59,])
sim.Fittja.og.y <- list(sim.Fittja.og[1:obs.n,],
                       Spring.sim.og, Summer.sim.og
                       ,Fall.sim.og, Winter.sim.og)

#The simulation results with revised model
Spring.sim <- sim.Fittja[122 <= sim.Fittja$DOY & sim.Fittja$DOY <= 171,]
Summer.sim <- sim.Fittja[172 <= sim.Fittja$DOY & sim.Fittja$DOY <= 265,]
Fall.sim   <- sim.Fittja[266 <= sim.Fittja$DOY & sim.Fittja$DOY <= 355,]
Winter.sim <- rbind(sim.Fittja[356 <= sim.Fittja$DOY,],
                      sim.Fittja[sim.Fittja$DOY <= 59,])
sim.Fittja.y <- list(sim.Fittja[1:obs.n,], 
                    Spring.sim, Summer.sim
                    ,Fall.sim, Winter.sim)

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
stat.avg[1, c(2 * i + 1)] <- nrow(obs.Fittja.y[[i]]) 
}

#RMSE caculation only for the last year
RMSE <- function(x,y){
 round(sqrt(sum((x - y)^2)/length(x)),2)
}

#D function
D <- function(x,y){
  x <- na.omit(x)
  y <- na.omit(y)
  ybar <- mean(y)
  round(1 - (sum((x - y)^2)/sum((abs(x - ybar) + abs(y - ybar))^2)),2)
}

#R2 valeus
rsq <- function(x, y) {
  x <- na.omit(x)
  y <- na.omit(y)
  round(cor(x, y) ^ 2,2)
  }

#average bias valeus
bias <- function(x, y) {
  x <- na.omit(x)
  y <- na.omit(y)
  round(sum(x - y)/length(x),2)
  }

stat <- list(RMSE,D,rsq,bias)

#stat by year and seasons for the original model 
for (i in 1:16) {
  for (j in 1:5) {
    if (i <= 4) {
    stat.avg[i + 1,2*j + 1] <- stat[[i]](sim.Fittja.og.y[[j]]$Temperature.C
                          ,obs.Fittja.y[[j]]$temp.avg)
    } else if (4 < i & i <= 8) {
    stat.avg[i + 1,2*j + 1] <- stat[[i - 4]](sim.Fittja.og.y[[j]]$temp.05
                                   ,obs.Fittja.y[[j]]$temp0.5)  
    } else if (8 < i & i <= 12) {
    stat.avg[i + 1,2*j + 1] <- stat[[i - 8]](sim.Fittja.og.y[[j]]$temp.15
                                   ,obs.Fittja.y[[j]]$temp1.5)  
    } else {
    stat.avg[i + 1,2*j + 1] <- stat[[i - 12]](sim.Fittja.og.y[[j]]$temp.25
                                   ,obs.Fittja.y[[j]]$temp2.5)  
    }
  }  
}

#stat by year and seasons for the REVISED model 
for (i in 1:16) {
  for (j in 1:5) {
    if (i <= 4) {
      stat.avg[i + 1,2*j + 2] <- stat[[i]](sim.Fittja.y[[j]]$Temperature.C
                                   ,obs.Fittja.y[[j]]$temp.avg)
    } else if (4 < i & i <= 8) {
      stat.avg[i + 1,2*j + 2] <- stat[[i - 4]](sim.Fittja.y[[j]]$temp.05
                                     ,obs.Fittja.y[[j]]$temp0.5)  
    } else if (8 < i & i <= 12) {
      stat.avg[i + 1,2*j + 2] <- stat[[i - 8]](sim.Fittja.y[[j]]$temp.15
                                     ,obs.Fittja.y[[j]]$temp1.5)  
    } else {
      stat.avg[i + 1,2*j + 2] <- stat[[i - 12]](sim.Fittja.y[[j]]$temp.25
                                      ,obs.Fittja.y[[j]]$temp2.5)  
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

H.50 <- 0.5 * Htank * 100 #convert to cm 
#obtain the days have <50% and >=50% depth
og.s <- which(sim.Fittja.og.y[[1]]$Depth.cm < H.50)
og.d <- which(sim.Fittja.og.y[[1]]$Depth.cm >= H.50)
mod.s <- which(sim.Fittja.y[[1]]$Depth.cm < H.50)
mod.d <- which(sim.Fittja.y[[1]]$Depth.cm >= H.50)

#sample size
stat.avg.depth[1,2:5] <- round(c(length(og.s),length(mod.s),length(og.d),length(mod.d)),0) 

#stat by year and seasons for the original model 
for (i in 1:4) {
  stat.avg.depth[i + 1,2] <- stat[[i]](sim.Fittja.og.y[[1]]$Temperature.C[og.s]
                                       ,obs.Fittja.y[[1]]$temp.avg[og.s])
  stat.avg.depth[i + 1,3] <- stat[[i]](sim.Fittja.y[[1]]$Temperature.C[mod.s]
                                   ,obs.Fittja.y[[1]]$temp.avg[mod.s])
  stat.avg.depth[i + 1,4] <- stat[[i]](sim.Fittja.og.y[[1]]$Temperature.C[og.d]
                                  ,obs.Fittja.y[[1]]$temp.avg[og.d]) 
  stat.avg.depth[i + 1,5] <- stat[[i]](sim.Fittja.y[[1]]$Temperature.C[mod.d]
                                   ,obs.Fittja.y[[1]]$temp.avg[mod.d]) 
  }


# Summary for the results, part 2. Only output after all simulation is done. 
#The data of manure tank, i.e output L1:M18
Output.tank <- data.frame(matrix(ncol = 2,nrow = 18))
Output.tank[1:18,1] <- c("Location","SurfaceArea(m2)","Starting.Depth(m)"
                         ,"Starting.Volume.m3","Total.Solids(%)",""
                         ,"Tank.Storage","Total.Tank.Volume(m3)"
                         ,"Yearly Maximum Storage Volume.m3","Yearly.Rain.Volume.m3"
                         ,"Yearly.Manure.Storage.Volume.m3","Tank.Diameter.m",""
                         ,"Average.Tm.C","Max.Tm.C","Min.Tm.C","","Max.d.cm")
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



#write the results out
library(xlsx)
manure.pic <- paste(result,"Fittja/figures/png/",Location,"_",test,".png",sep = "")
wb <- createWorkbook()
sheet <- createSheet(wb, "pic")
addPicture(manure.pic, sheet, startRow = 1, startColumn = 1)
saveWorkbook(wb, file = paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/stat/OR_",
test,".xlsx",sep = ""), password = NULL)

write.xlsx(stat.avg,
           file = paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/stat/OR_",
                        test,".xlsx",sep = ""),
               sheetName = "OR site", row.names = F, append = TRUE)
write.xlsx(stat.avg.depth,
           file = paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/stat/OR_",
                       test,".xlsx",sep = ""),
           sheetName = "OR site_depth", row.names = F,append = TRUE)
write.xlsx(Output.tank,
           file = paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/stat/OR_",
                        test,".xlsx",sep = ""),
           sheetName = "output summary_revised", row.names = F,append = TRUE)
write.xlsx(parameters,
           file = paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/stat/OR_",
                        test,".xlsx",sep = ""),
           sheetName = "Parameters", row.names = F,append = TRUE, showNA = FALSE)

