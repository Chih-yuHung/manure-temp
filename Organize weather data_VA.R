library(imputeTS) # for NA interpolation
library(REdaS)

#I just need a table to know what I should do for weather input
#template<-read.csv("C:/AAFC/Project 2_Manure Temp/2. Method/input/daily env input_RH.csv",header=T)
#Radiation data input from Bromma
rad<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/Radiation Daily data 2007_Bromma.csv",header=T)

#To organize weather input for Sweden data, it's from Arlanda, Temp and RH only
temp<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/hourly data from Arlanda airport.csv",header=T)
temp[temp=="."]=NA
temp$Temp<-as.numeric(temp$Temp)
#interpolate NA by linear
weather<-temp %>% na_interpolation(option="linear")#Start with June 15.
#So the max and min is obtained from max and min per hour not from average of hour
#Note that tapply sort my data again by following the original order in my data, i.e start from June 15.
AirTmax1<-rep(tapply(weather$Temp,weather$Date,max),3)
AirTmin1<-rep(tapply(weather$Temp,weather$Date,min),3)
AirTmin2<-c(AirTmin1[2:1095],AirTmin1[1])
AirTmax0<-c(AirTmax1[1095],AirTmax1[1:1094])

#RH
RH.6<-rep(weather$RH[weather$Hour=="600"],3)
RH.15<-rep(weather$RH[weather$Hour=="1500"],3)

#Radiation data from 2007/6/15-2021/6/14 from Bromma
rad.2020<-rad[4773:5137,c(3,19)] #to obtain radiation from 2020/6/15-2021/6/14 our experiment period 
SR<-rad.2020$SOLIN
rad<-rad[38:5137,c(3,19)]
rad$year<-substring(rad[,1],first=1,last=4)
rad$month<-substring(rad[,1],first=5,last=6)
rad$day<-substring(rad[,1],first=7,last=8)
rad$monthday<-substring(rad[,1],first=5,last=8)
rad<-rad[complete.cases(rad),]
radiation<-tapply(rad$SOLIN,rad$monthday,mean)[c(1:59,61:366)] #exclude leap day
radiation.max<-tapply(rad$SOLIN,rad$monthday,max)[c(1:59,61:366)] #exclude leap day for max potential radiation
radiation.max1<-radiation.max[c(166:365,1:165)] #to start from June 15.
###July 22.2021, it's not linear regression between SR and SRmax
cloud<-ifelse(SR<radiation.max1,((1-SR/radiation.max1)/0.72)^(1/3.2),1)
#Srmax is a useless value in simulation and I made a vector for it here
Srmax<-radiation.max1


# wind data is from daily data in Arlanda airport
temp1<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/daily data from Arlanda airport.csv",header=T)
temp1<-temp1[,c(3,17)]
wind<-rep(temp1$XVH,3) # unit, m/s at 2m so I don't need to convert to 2m later

# precipitation data is from hourly data in Bromma airport
temp1<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/raw/Precipitation hourly data_Bromma.csv",header=T)
temp1<-cbind(temp1[,3],as.numeric(temp1[,19]))
temp1[is.na(temp1)]=0
precip<-rep(tapply(temp1[,2],temp1[,1],sum),3)#unit:mm

#combine the data together, it's daily value
aa<-as.data.frame(cbind(AirTmax1,AirTmin1,AirTmin2,AirTmax0,SR,Srmax,precip,RH.6,RH.15,wind,cloud))

#To obtain the date and DOY 
temp.date<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/temp.cover.daily.csv",header=T)[,2:5]
#to obtain year, month, date.
DAY<-as.character(as.Date(0:17,origin = "2021-05-28"))
DAY<-as.data.frame(matrix(unlist(strsplit(DAY,split="-")),ncol=3,byrow=TRUE))
colnames(DAY)<-c("Year","Month","Day")
temp.date<-rbind(temp.date[1:3],DAY)[,1:3]
temp.date<-temp.date[rep(c(1:365),3),] #to obtain env input from June 15
temp.date$DOY<-rep(c(166:365,1:165),3)
temp.date$Year<-rep(c(2020,2021,2022,2023),c(200,365,365,165))
#temp.date<-temp.date[rep(c(201:365,1:200),3),] #to obtain env input from Jan 1
#temp.date$DOY<-rep(1:365,3) 
#temp.date$Year<-rep(c(2021,2022,2023),each=365)

#Need to export a csv file which has the same format in daily env input_RH.csv for project 2
#It's a three-year data from Jan 1 to Dec 31. 
env.input<-cbind(temp.date,aa)
#It's a three-year data from June 15 to June 14. 
#env.input1<-cbind(temp.date1,aa1)

#Add Date ID to the input
#Date.ID<-as.numeric(as.Date(0:1094,origin="2021-01-01"),by="days")
env.input$'Date ID'<-as.numeric(as.Date(0:1094,origin="2020-06-15"),by="days")
#Add Date ID to the input1
#Date.ID1<-rep(as.numeric(as.Date(c(165:364,0:164),origin="2021-06-15"),by="days"),3)
#env.input1<-cbind(Date.ID1,env.input1)
#names(env.input1)[names(env.input1) == "Date.ID1"] <- "Date ID"


#Export for input
write.csv(env.input,"C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/2. Method/input/daily env input_Arlanda_June15.csv",row.names = FALSE)
#write.csv(env.input1,"C:/AAFC/Project 3_Sweden/2. Method/input/daily env input1_Sweden.csv",row.names = FALSE)

#Wind speed in summer season Summer: 172-265 (June 20,2020,Sept 21)
wind.summer <- env.input[172 <= env.input$DOY & env.input$DOY <= 265,]
mean(wind.summer$wind) #2.9 m/s

#precipitation in spring and fall season spring 80-171, fall 266-355
precip.spring <- env.input[80 <= env.input$DOY & env.input$DOY <= 171,]
sum(precip.spring$precip)/3 #109 mm, 25%
precip.summer <- env.input[172 <= env.input$DOY & env.input$DOY <= 265,]
sum(precip.summer$precip)/3 #130 mm 30%
precip.fall <- env.input[266 <= env.input$DOY & env.input$DOY <= 355,]
sum(precip.fall$precip)/3 #130 mm, 30%
sum(env.input$precip)/3 #440 mm


#Obtain yearly amplitude and daily amplitude, wind speed
dailyinput<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/2. Method/input/daily env input_Arlanda_June15.csv",header=T)
yearly.amp<-(max(dailyinput$AirTmax1)-min(dailyinput$AirTmin1))/2
windspeed<-mean(dailyinput$wind)/2 #it assumed 50% because of obstruction of tank and buildings nearby 
#daily amplitude. It's an average of max-min /2
daily.amp<-mean((dailyinput$AirTmax1-dailyinput$AirTmin1)/2)
