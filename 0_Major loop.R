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
  precip.d <- Envir.daily$precip[i]/1000
  if (submodels == 1) {
    source("3.1. Alpha.s_adjustment.R",echo = F)
  } else {
    snow <- 0
  }
  RH6 <- Envir.daily$RH.6[i]
  RH15 <- Envir.daily$RH.15[i]
  
  #source("5.1. Manure volume_initial depth and removal amount.R",echo = F)
  #To calculate manure volume, encoding to be change if use mac
  source("5. Manure volume_Fittja.R",echo = F)
  #print(paste("after volume",Sys.time()-starttime))
  #To calculate solar radiation, soil temp, and manure temp at 300 sec steps.
  source("6. Solar radiation and soil temp_Fittja_shade.R",echo = F)
  #To calculate enthalpy.
  source("6.1 Enthalpy calculation.R",echo = F)
  #print(paste("after solar",Sys.time()-starttime))
  #To calculate final hourly temp
  source("7. hourly temp_Fittja.R",echo = F)
  #retrieve manure temperature at 0.5m, 1.5m and 2.5 m
  source("7.1 temp at three depths_Fittja.R",echo = F)
  
  #To obtain temp and depth at the end of the day
  # I need manure temperature in different depth every day for the last year
  #It would be a table with DOY, manure.depth, and temp
  if (is.element(i,tail(1:d.length,n = 365))) {
    manure.temp <- c(manure.temp,M.temp.d)
    aa <- seq(0,M.depth,length.out = 30)
    manure.depth <- c(manure.depth,aa)
  }
  
  #Write the results
  Output[i,6:15] <- c(Avg.M.temp.d,M.depth*100,M.volume.current,
                      Evap.depth.d*100,precip.d*100,sum(q.net.rad),snow,
                      M.temp.depth)
  print(paste("Sequence",i,"And Manure temp",Avg.M.temp.d))
  #Write the results for the four dates at the last year
  #the four dates are May 1, Aug 1, Nov 1, Feb 1. 
  #four.date<-1 # for temp on four dates
  #date.four<-as.numeric(as.Date(c("2022-6-18","2022-8-24","2022-9-7","2022-10-21"),by="days"))
  #if (Output$`Date ID`[i] %in% date.four){
  #  source("9. Manure temperature on four dates.R",echo = F)
  #}
  
  source("5.2. Manure volume removal.R",echo = F)
  
  #Save the new temperatures
  #In the sheet, it save a final temp to R60:R89, this was not used, so I skipped it.
  ini.M.Temp <- Final.M.Temp
  #This the soil temperature(to depth 2.995m), it was a two step to reset ini.S.Temp in marco
  ini.S.Temp <- S.Temp[,288]
}
endtime <- Sys.time()
#Output<-Output[366:d.length,] # first year is for soil temp stabilization
print(endtime - starttime)

Output <- Output[(d.length - 364):d.length,]
if (submodels == 1) {
  #Shade/output to an excel file
  write.csv(Output,paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/",Location,"/with shade/"
                         ,Location,"_",test,".csv",sep = ""),row.names = FALSE)
} else {
  #Without shade/output to an excel file
  write.csv(Output,paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/",Location,"/original/"
                         ,Location,"_",test,".csv",sep = ""),row.names = FALSE)
}
