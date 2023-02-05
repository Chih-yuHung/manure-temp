library(imputeTS) # for NA interpolation
library(lubridate)
#Deal with the manure temperature
temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/manure temp Fittja.csv",header=T)
#Use the data until Feb 27, 2021 (2428)
temp<-temp[1:2428,]
#There are some missing data, I used linear interpolation to fill them
temp<-na_interpolation(temp,option="linear")
# #Give data, and time first
time<-strsplit(temp$Time,"\\s")
time<-as.data.frame(matrix(unlist(time),ncol=2,byrow=TRUE))
hour<-matrix(unlist(strsplit(time[,2],":")),ncol=2,byrow=TRUE)[,1]
#for date, my goal is to have year, month, day and DOY in four columns
#need to convert date to the same form first, now I have  "mm/dd/yyyy"
time$V1<-as.character(mdy(time$V1))
DOY<-yday(as.Date(time$V1))
D<-as.data.frame(matrix(unlist(strsplit(time$V1,"-")),ncol=3,byrow=TRUE))
colnames(D)<-c("Year","Month","Day")
# # #combine the date and temperature data
temp<-cbind(D,hour,DOY,temp[,-1])


#Convert the real temperature of depth based on the depth data.
#depth.05, depth.15 and depth.25 are the actual depth of the three thermometers
depth.05<-c()
depth.15<-c()
depth.25<-c()
# determine the depth by a loop
for (i in 1:length(temp[,1])) {
  if (temp$Depth[i]<0.5){
    depth.05[i]<-temp$Depth[i]
    depth.15[i]<-temp$Depth[i]
    depth.25[i]<-temp$Depth[i] # because all thermometers are at the bottom
  } else
  if (temp$Depth[i]>= 0.5 & temp$Depth[i]<1.5) {
    depth.05[i]<-0.5
    depth.15[i]<-temp$Depth[i]
    depth.25[i]<-temp$Depth[i] # because the last two thermometers are at the bottom
  } else
  if (temp$Depth[i]>= 1.5 & temp$Depth[i]<2.5){
    depth.05[i]<-0.5
    depth.15[i]<-1.5
    depth.25[i]<-temp$Depth[i] # only this thermometers is at the bottom
  } else {
    depth.05[i]<-0.5
    depth.15[i]<-1.5
    depth.25[i]<-2.5
  }
}

temp<-cbind(temp,depth.05,depth.15,depth.25)

#Save the data out. it's hourly data.
write.csv(temp,"C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/temp.Fittja.hourly.csv")

temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/temp.Fittja.hourly.csv",header=T)

# #to obtain daily measurement.
temp.daily<-unique(temp[c(2,3,4,6)])

# #For tank with cover
for (i in 7:13){
agg<-aggregate(temp[,i],list(temp$DOY), FUN = mean)
temp.daily[,(i-2)]<-agg[match(temp.daily$DOY,agg[,1]),2]
}
colnames(temp.daily)[5:11]<-c("temp0.5","temp1.5","temp2.5","Depth"
                                    ,"depth0.5","depth1.5","depth2.5")

#average the temperature by manure depth
#assumption: every thermocouples can only detect temperature +- 0.5 m
for (i in 1:length(temp.daily$DOY)){
  Depth<-temp.daily$Depth[i]
  weight.05<-min(1,Depth) #the weight of first thermo
  weight.15<-ifelse(Depth<=1.5,0.5,min(1,(Depth-1.5)+0.5)) #the weight of the 2nd thermo
  weight.25<-ifelse(Depth<=2.5,0.5 #the weight of the 3rd thermo
                    ,ifelse(Depth>=3.0,(Depth-2.0),min(1,(Depth-2.5)+0.5)))
  t05<-temp.daily$temp0.5[i]
  t15<-temp.daily$temp1.5[i]
  t25<-temp.daily$temp2.5[i]
  if (Depth <= 0.5){ #<0.5, average the three
    temp.daily$temp.avg[i]<-mean(c(t05,t15,t25))
  }
  if (Depth > 0.5 & Depth <=1.5){
    temp.daily$temp.avg[i]<-((t05*weight.05+mean(c(t15,t25))*weight.15)
                                    /(weight.05+weight.15))
  }
  if (Depth > 1.5){
    temp.daily$temp.avg[i]<-(((t05*weight.05)+(t15*weight.15)+(t25*weight.25))
                                   /(weight.05+weight.15+weight.25))
}
}


# #to integrate soil temperature data 
# temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/Soil temp.csv",header=T)
# Stemp10<-tapply(temp$Stemp10,temp$Date,mean)
# Stemp50<-tapply(temp$Stemp50,temp$Date,mean)
# temp.no.daily<-cbind(temp.no.daily,Stemp10[1:344],Stemp50[1:344])
# temp.cover.daily<-cbind(temp.cover.daily,Stemp10[1:344],Stemp50[1:344])
# colnames(temp.cover.daily)[13:14]<-c("Stemp10","Stemp50")
# colnames(temp.no.daily)[13:14]<-c("Stemp10","Stemp50")

#The data is from 4/30-Feb27, But I want to have May 1 - Apr30
#I left Feb 28 -Apr 30 empty. 
DAY<-as.character(as.Date(0:61,origin = "2021-02-28"))
DAY<-as.data.frame(matrix(unlist(strsplit(DAY,split="-")),ncol=3,byrow=TRUE))
DAY<-cbind(DAY,c(59:120))
colnames(DAY)<-c("Year","Month","Day","DOY")
DAY<-type.convert(DAY)
temp.daily <- temp.daily %>% full_join(DAY)
temp.daily<-temp.daily[-1,]
# #Save the daily data out.
write.csv(temp.daily,"C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/temp.Fittja.daily.csv") # with cover



