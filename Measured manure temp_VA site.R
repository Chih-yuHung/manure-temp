library(imputeTS) # for NA interpolation
library(lubridate)
#Deal with the manure temperature
setwd("C:/AAFC/Project 3_Sweden/3. Results")
# temp.cover<-read.csv("raw/manure temp with cover.csv",header=T)
# temp.no<-read.csv("raw/manure temp without cover.csv",header=T)
# 
# #There are some missing data, I used linear interpolation to fill them
# temp.cover<-na_interpolation(temp.cover,option="linear")
# temp.no<-na_interpolation(temp.no,option="linear")
# # #Give data, and time first
# time<-strsplit(temp.cover$Time,"\\s")
# time<-as.data.frame(matrix(unlist(time),ncol=2,byrow=TRUE))
# hour<-matrix(unlist(strsplit(time[,2],":")),ncol=2,byrow=TRUE)[,1]
# #for date, my goal is to have year, month, day and DOY in four columns
# #need to convert date to the same form first, now I have  "mm/dd/yyyy", and "yy/mm/dd"
# time$V1[1:1000]<-as.character(mdy(time$V1[1:1000]))
# time$V1[1001:length(time$V1)]<-as.character(ymd(time$V1[1001:length(time$V1)]))
# DOY<-yday(as.Date(time$V1))
# D<-as.data.frame(matrix(unlist(strsplit(time$V1,"-")),ncol=3,byrow=TRUE))
# colnames(D)<-c("Year","Month","Day")
# # # #combine the date and temperature data
# temp.cover<-cbind(temp.cover[,-1],D,hour,DOY)
# temp.no<-cbind(temp.no[,-1],D,hour,DOY)
# temp.cover<-temp.cover[,c(5:9,1:4)]
# temp.no<-temp.no[,c(5:9,1:4)]
# #
# # #Convert the real temperature of depth based on the depth data.
# # ####For tank with cover
# #depth.05, depth.15 and depth.25 are the actual depth of the three thermometers
# depth.05<-c()
# depth.15<-c()
# depth.25<-c()
# # # determine the depth by a loop
# for (i in 1:2745) {
#   if (temp.cover$Depth[i]<0.5){
#     depth.05[i]<-temp.cover$Depth[i]
#     depth.15[i]<-temp.cover$Depth[i]
#     depth.25[i]<-temp.cover$Depth[i] # because all thermometers are at the bottom
#   } else
#   if (temp.cover$Depth[i]>= 0.5 & temp.cover$Depth[i]<1.5) {
#     depth.05[i]<-0.5
#     depth.15[i]<-temp.cover$Depth[i]
#     depth.25[i]<-temp.cover$Depth[i] # because the last two thermometers are at the bottom
#   } else
#   if (temp.cover$Depth[i]>= 1.5 & temp.cover$Depth[i]<2.5){
#     depth.05[i]<-0.5
#     depth.15[i]<-1.5
#     depth.25[i]<-temp.cover$Depth[i] # only this thermometers is at the bottom
#   } else {
#     depth.05[i]<-0.5
#     depth.15[i]<-1.5
#     depth.25[i]<-2.5
#   }
# }
# 
# temp.cover<-cbind(temp.cover,depth.05,depth.15,depth.25)
# #
# # ####For tank without cover
# # #depth.05, depth.15 and depth.25 are the actual depth of the three thermometers
# depth.05<-c()
# depth.15<-c()
# depth.25<-c()
# # # determine the depth by a loop
# for (i in 1:2745) {
#   if (temp.no$Depth[i]<0.5){
#     depth.05[i]<-temp.no$Depth[i]
#     depth.15[i]<-temp.no$Depth[i]
#     depth.25[i]<-temp.no$Depth[i] # because all thermometers are at the bottom
#   } else
#     if (temp.no$Depth[i]>= 0.5 & temp.no$Depth[i]<1.5) {
#       depth.05[i]<-0.5
#       depth.15[i]<-temp.no$Depth[i]
#       depth.25[i]<-temp.no$Depth[i] # because the last two thermometers are at the bottom
#     } else
#       if (temp.no$Depth[i]>= 1.5 & temp.no$Depth[i]<2.5){
#         depth.05[i]<-0.5
#         depth.15[i]<-1.5
#         depth.25[i]<-temp.no$Depth[i] # only this thermometers is at the bottom
#       } else {
#         depth.05[i]<-0.5
#         depth.15[i]<-1.5
#         depth.25[i]<-2.5
#       }
# }
# 
# temp.no<-cbind(temp.no,depth.05,depth.15,depth.25)
#
# #Save the data out. it's hourly data.
# write.csv(temp.cover,"temp.cover.hourly.csv") # with cover
# write.csv(temp.no,"temp.no.hourly.csv") #without cover

temp.cover<-read.csv("temp.cover.hourly.csv",header=T)
temp.no<-read.csv("temp.no.hourly.csv",header=T)

# #to obtain daily measurement.
temp.cover.daily<-unique(temp.cover[c(2,3,4,6)])
temp.no.daily<-unique(temp.no[c(2,3,4,6)])

# #For tank with cover
for (i in 7:13){
agg<-aggregate(temp.cover[,i],list(temp.cover$DOY), FUN = mean)
temp.cover.daily[,(i-2)]<-agg[match(temp.cover.daily$DOY,agg[,1]),2]
}
colnames(temp.cover.daily)[5:11]<-c("temp0.5","temp1.5","temp2.5","Depth"
                                    ,"depth0.5","depth1.5","depth2.5")
# #For tank without cover
for (i in 7:13){
  agg<-aggregate(temp.no[,i],list(temp.no$DOY), FUN = mean)
  temp.no.daily[,(i-2)]<-agg[match(temp.no.daily$DOY,agg[,1]),2]
}
colnames(temp.no.daily)[5:11]<-c("temp0.5","temp1.5","temp2.5","Depth"
                                    ,"depth0.5","depth1.5","depth2.5")

#average the temperature by manure depth
#assumption: every thermocouples can only detect temperature +- 0.5 m
#For temperature in covered tank
for (i in 1:length(temp.cover.daily$DOY)){
  Depth<-temp.cover.daily$Depth[i]
  weight.05<-min(1,Depth) #the weight of first thermo
  weight.15<-ifelse(Depth<=1.5,0.5,min(1,(Depth-1.5)+0.5)) #the weight of the 2nd thermo
  weight.25<-ifelse(Depth<=2.5,0.5 #the weight of the 3rd thermo
                    ,ifelse(Depth>=3.0,(Depth-2.0),min(1,(Depth-2.5)+0.5)))
  t05<-temp.cover.daily$temp0.5[i]
  t15<-temp.cover.daily$temp1.5[i]
  t25<-temp.cover.daily$temp2.5[i]
  if (Depth <= 0.5){ #<0.5, average the three
    temp.cover.daily$temp.avg[i]<-mean(c(t05,t15,t25))
  }
  if (Depth > 0.5 & Depth <=1.5){
    temp.cover.daily$temp.avg[i]<-((t05*weight.05+mean(c(t15,t25))*weight.15)
                                    /(weight.05+weight.15))
  }
  if (Depth > 1.5){
    temp.cover.daily$temp.avg[i]<-(((t05*weight.05)+(t15*weight.15)+(t25*weight.25))
                                   /(weight.05+weight.15+weight.25))
}
}

#For temperature in no cover tank
for (i in 1:length(temp.no.daily$DOY)){
  Depth<-temp.no.daily$Depth[i]
  weight.05<-min(1,Depth) #the weight of first thermo
  weight.15<-ifelse(Depth<=1.5,0.5,min(1,(Depth-1.5)+0.5)) #the weight of the 2nd thermo
  weight.25<-ifelse(Depth<=2.5,0.5 #the weight of the 3rd thermo
                    ,ifelse(Depth>=3.0,(Depth-2.0),min(1,(Depth-2.5)+0.5)))
  t05<-temp.no.daily$temp0.5[i]
  t15<-temp.no.daily$temp1.5[i]
  t25<-temp.no.daily$temp2.5[i]
  if (Depth <= 0.5){ #<0.5, average the three
    temp.no.daily$temp.avg[i]<-mean(c(t05,t15,t25))
  }
  if (Depth > 0.5 & Depth <=1.5){
    temp.no.daily$temp.avg[i]<-((t05*weight.05+mean(c(t15,t25))*weight.15)
                                   /(weight.05+weight.15))
  }
  if (Depth > 1.5){
    temp.no.daily$temp.avg[i]<-(((t05*weight.05)+(t15*weight.15)+(t25*weight.25))
                                   /(weight.05+weight.15+weight.25))
  }
}


# #to integrate soil temperature data 
temp<-read.csv("C:/AAFC/Project 3_Sweden/3. Results/raw/Soil temp.csv",header=T)
Stemp10<-tapply(temp$Stemp10,temp$Date,mean)
Stemp50<-tapply(temp$Stemp50,temp$Date,mean)
temp.no.daily<-cbind(temp.no.daily,Stemp10[1:344],Stemp50[1:344])
temp.cover.daily<-cbind(temp.cover.daily,Stemp10[1:344],Stemp50[1:344])
colnames(temp.cover.daily)[13:14]<-c("Stemp10","Stemp50")
colnames(temp.no.daily)[13:14]<-c("Stemp10","Stemp50")

# #Save the daily data out.
write.csv(temp.cover.daily,"temp.cover.daily.csv") # with cover
write.csv(temp.no.daily,"temp.no.daily.csv") #without cover



