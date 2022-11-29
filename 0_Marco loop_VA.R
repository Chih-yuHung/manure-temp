library(REdaS); library(xlsx); library(beepr) ;library(dplyr); library(imputeTS)
#Set location, initial date and end time; date origin in R, 1970-1-1
Location<-"Fittja"
start.date<-"2020-5-1"  
end.date<-"2023-4-30" 
#insert multiple removal date in multiple years, can be different dates
removal.start<-as.numeric(as.Date(c("2020-7-8","2020-10-2","2020-10-30","2021-4-12"
                                    ,"2020-7-8","2020-10-2","2020-10-30","2021-4-12"
                                    ,"2021-7-8","2021-10-2","2021-10-30","2022-4-12"
                                    ,"2022-7-8","2022-10-2","2022-10-30","2023-4-12"),by="days"))
removal.end<-as.numeric(as.Date(c("2020-7-9","2020-10-4","2020-10-31","2021-4-13"
                                  ,"2020-7-9","2020-10-4","2020-10-31","2021-4-13"
                                  ,"2021-7-9","2021-10-4","2021-10-31","2022-4-13"
                                  ,"2022-7-9","2022-10-4","2022-10-31","2023-4-13"),by="days"))
removal.day<-(removal.end-removal.start)[1:4]+1
removal.duration<-list()
    for (i in 5:16){ 
        removal.duration[[i-4]]<-c(removal.start[i]:removal.end[i])}

#Shade effect or not
submodels<-0  #1 with submodel effects, 0 is without
#It includes (1) shadow effect, (2) latent heat and snow accumulation, (3) agitation
mixing.day<-5
#start from here. The removal dates doesn't match. I also did it wrong on the M.volume update
Envir.daily<-read.csv("input/daily env input_Fittja_May1.csv",header=T)
#To produce an extra year for balance soil temperature
Envir.daily<-Envir.daily[c(1:365,1:1095),]
#to know how many days we have for the loop
d.length<-nrow(Envir.daily)
#initial manure temp
ini.M.Temp<-read.csv("input/Initial M temp.csv",header=T)
ini.M.Temp<-ini.M.Temp[,"Initial.Temp"] #change to vector

#Read parameters
source("3. Parameters.R",echo=F)           #Parameters we can change
source("4. Constants_Fittja.R",echo = F)   #Constants no need to change

#to store daily manure.temp in the 30 layers
manure.temp<-c()
manure.depth<-c()
#Start the simulation here. 
starttime<-Sys.time()
for (i in 1:d.length){
#To read daily data from environment input.
print(paste("Date ID =",Output$`Date ID`[i],"DOY=",Output$DOY[i])) 
T.day<-Output$DOY[i]
AirTmax0<-Envir.daily$AirTmax0[i]
AirTmax1<-Envir.daily$AirTmax1[i]
AirTmin1<-Envir.daily$AirTmin1[i]
AirTmin2<-Envir.daily$AirTmin2[i]
SR<-Envir.daily$SR[i]
wind<-Envir.daily$wind[i]
wind.v<-Envir.daily$wind[i]   #daily wind speed at 2m, m/h
wind.f<-(2.36+1.67*wind.v)*Au^(-0.05)
cc<-min(Envir.daily$cloud[i],1) #cloud clover
precip.d<-Envir.daily$precip[i]/1000
if (submodels == 1){
source("3.1. Alpha.s_adjustment.R",echo=F)
} else{
  snow <-0
}
RH6<-Envir.daily$RH.6[i]
RH15<-Envir.daily$RH.15[i]

#inital depth and removal amount
source("5.1. Manure volume_initial depth and removal amount.R",echo=F)
#To calculate manure volume, encoding to be change if use mac
source("5. Manure volume_Fittja.R",echo=F)
#print(paste("after volume",Sys.time()-starttime))
#To calculate solar radiation, soil temp, and manure temp at 300 sec steps.
source("6. Solar radiation and soil temp_Fittja_shade.R",echo=F)
#To calculate enthalpy.
source("6.1 Enthalpy calculation.R",echo=F)
#print(paste("after solar",Sys.time()-starttime))
#To calculate final hourly temp
source("7. hourly temp_Fittja.R",echo=F)
#retrieve manure temperature at 0.5m, 1.5m and 2.5 m
source("7.1 temp at three depths_Fittja.R",echo = F)

#To obtain temp and depth at the end of the day
# I need manure temperature in different depth every day for the last year
#It would be a table with DOY, manure.depth, and temp
if(is.element(i,tail(1:d.length,n=365))){
    manure.temp<-c(manure.temp,M.temp.d)
    aa<-seq(0,M.depth,length.out=30)
    manure.depth<-c(manure.depth,aa)
}

#Write the results
Output[i,6:15]<-c(Avg.M.temp.d,M.depth*100,M.volume.current,
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
ini.M.Temp<-Final.M.Temp
#This the soil temperature(to depth 2.995m), it was a two step to reset ini.S.Temp in marco
ini.S.Temp<-S.Temp[,288]
}
endtime<-Sys.time()
#Output<-Output[366:d.length,] # first year is for soil temp stabilization
print(endtime-starttime)


# Summary for the results, part 2. Only output after all simulation is done. 
#The data of manure tank, i.e output L1:M18
Output.tank<-data.frame(matrix(ncol=2,nrow = 18))
Output.tank[1:18,1]<-c("Location","SurfaceArea(m2)","Starting.Depth(m)"
                       ,"Starting.Volume.m3","Total.Solids(%)",""
                       ,"Tank.Storage","Total.Tank.Volume(m3)"
                       ,"Yearly Maximum Storage Volume.m3","Yearly.Rain.Volume.m3"
                       ,"Yearly.Manure.Storage.Volume.m3","Tank.Diameter.m",""
                       ,"Average.Tm.C","Max.Tm.C","Min.Tm.C","","Max.d.cm")
Output.tank[1,2]<-Location
Output.tank[2:3,2]<-c(Au,M.depth)        #area and initial depth, m2 and m
Output.tank[4,2]<-as.numeric(Output.tank[2,2])*as.numeric(Output.tank[3,2]) #starting volume.
Output.tank[5,2]<-Total.solid                 #%
Output.tank[c(6,7,13,17),2]<-""                      #blank
Output.tank[8,2]<-Tank.v                      #Total Tank volume, m3
Output.tank[9,2]<-Max.storage                 #yearly manure storage,m3
Output.tank[10,2]<-Rain.v                     #yearly rain volume in storage,m3
Output.tank[11,2]<-M.storage                  #yearly Manure storage,m3
Output.tank[12,2]<-ri*2                       #Diameter of tank,m
Output.tank[14,2]<-mean(Output$Temperature.C) #Avg. manure Temperature for the estimate years
Output.tank[15,2]<-max(Output$Temperature.C)  #Max manure Temperature
Output.tank[16,2]<-min(Output$Temperature.C)  #Min manure Temperature
Output.tank[18,2]<-max(Output$Depth.cm)       #Maximum Manure Depth

#tabulate a daily manure temp and depth 
DOY<-rep(c(167:365,1:166),each=30)
daily.data<-as.data.frame(cbind(DOY,manure.depth,manure.temp))
# 
# DOY<-rep(1:365,each=288)
# daily.Sb.data<-as.data.frame(cbind(DOY,Sb.daily,Sb.daily.noshade,Sd.daily,Sd.daily.noshade,qnet,qnet.noshade))

if (submodels == 1) {
#Shade/output to an excel file
write.csv(Output,paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/with shade/"
                       ,Location,Sys.Date(),".csv",sep=""),row.names = FALSE)
} else {
  #Without shade/output to an excel file
write.csv(Output,paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/original/"
                       ,Location,Sys.Date(),".csv",sep=""),row.names = FALSE)
}

