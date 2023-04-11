#Start the simulation here. 
starttime <- Sys.time()
for (i in 1:d.length) {
  #To read daily data from environment input.
  print(paste("Date ID =",Output$`Date ID`[i],"DOY=",Output$DOY[i])) 
  T.day <- Output$DOY[i]
  AirTmax0 <- Envir.daily$AirTmax0[i]
  AirTmax1 <- Envir.daily$AirTmax1[i]
  AirTmin1 <- Envir.daily$AirTmin1[i]
  AirTmin2 <- Envir.daily$AirTmin2[i]
  SR <- Envir.daily$SR[i]
  wind <- Envir.daily$wind[i]
  wind.v <- Envir.daily$wind[i]   #daily wind speed at 2m, m/h
  wind.f <- (2.36 + 1.67*wind.v)*Au^(-0.05)
  cc <- min(Envir.daily$cloud[i],1) #cloud clover
  precip.d <- ifelse(submodels == 1, 
                     (rain[i] + melt.act[i])/100, Envir.daily$precip[i]/1000)
  if (submodels == 1) {
    source("3.2. Alpha.s_adjustment.R",echo = F)
    snow <- snow.p[i]
  } else {
    snow <- 0
  }
  RH6 <- Envir.daily$RH.6[i]
  RH15 <- Envir.daily$RH.15[i]

  #To calculate manure volume, encoding to be change if use mac
  source("5. Manure volume.R",echo = F)
  #To calculate solar radiation, soil temp, and manure temp at 300 sec steps.
  source("6. Solar radiation and soil temp_shade.R",echo = F)
  #To calculate enthalpy.
  source("6.1 Enthalpy calculation.R",echo = F)
  cat(paste("snow depth =",snow, sep = ""))
  #To calculate final hourly temp
  source("7. hourly temp.R",echo = F)
  #retrieve manure temperature at 0.5m, 1.5m and 2.5 m
  source("7.1 temp at three depths.R",echo = F)
  

  #Write the results
  Output[i,6:17] <- c(Avg.M.temp.d,M.depth*100,M.volume.current,
                      Evap.depth.d*100,precip.d*100,sum(q.net.rad),snow,
                      M.temp.depth,In.M.temp,alpha.s)
  print(paste("Sequence",i,"And Manure temp",Avg.M.temp.d))

  source("5.1. Manure volume removal.R",echo = F)
  
  #Save the new temperatures
  ini.M.Temp <- Final.M.Temp
  ini.S.Temp <- S.Temp[,288]
}
endtime <- Sys.time()
print(endtime - starttime)

Output <- Output[(d.length - 364):d.length,]
result <- "C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/"
if (submodels == 1) {
  #Shade/output to an excel file
  write.csv(Output,paste(result,Location,"/with shade/"
                         ,Location,"_",test,".csv",sep = ""),row.names = FALSE)
} else {
  #Without shade/output to an excel file
  write.csv(Output,paste(result,Location,"/original/"
                         ,Location,"_",test,".csv",sep = ""),row.names = FALSE)
}
