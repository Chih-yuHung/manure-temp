#This file has all adjustable parameters that may influence our results. 

#Tank properties
Htank<-3.6            #height of tank, m, B29
ri<-11                #Inner radius of tank, m, B32
Au<-ri^2*pi           #tank area, m2, F55
Tank.v<-Au*Htank      #Total tank volume, m3, M26
#manure storage is a estimate number, maximum depth was 3.1 m in Aug
M.storage<-2800       #yearly manure storage volume, m3, M29 =P32,because total manure
                      # removals were 2800-3000 m3 in 2018-2021
M.daily<-M.storage/Au/365
#It's a swine farm need to adjust the manure input rates. 
Freeboard<-0.3        #freeboard, m, P34
sludge<-0.5           #m, P36

#Manure depth
M.depth<-0.67                  #This is the initial manure depth, m, L32
removal.depth<-c(1.87,0.18,0.15,0.93)  #the depth after removal, m, S52



#Manure properties, R26:29
Total.solid<-0.08                #It barely influences the manure temperature 
#Input manure temperature
annualT<-mean(c(Envir.daily$AirTmax1,Envir.daily$AirTmin1)) #for ini. soil temp, assume equal to mean annual air temp, B43
Avg.Barn.temp<-annualT        #degree C, avg. annual barn temp, L46, I assumed annual air temperature here 7.12
Barn.temp.amp<-10             #degree C, amplitude of annual temp, L47
Temp.cost<-4.1                #Temp phase constant, L48, barely influence the result
                              #15.2 and 3 for the last result

#Solar data
L<-59.525002                  #Latitude
alpha.s<-0.85                 #solar absorptivity, B18, 0.8 in Tim's model
Eb<-1395                      #extraterrestrial solar flux density, W m-2
tau<-0.75                     #Atmospheric transmittance, 0.75 clear, 0.4 overcast
A<-13                         #altitude, m
epsilon<-0.925                #emissivity,B26

#Soil temperature properties 
den.s<-1800                 #Soil density, kg/m3, B41,Saturated Clay = 2000,
                            #Dry clay = 1600,Saturated sand = 2000
ks<-1.7                     #soil thermal conductivity,W/mk, B42, 
                            #Saturated Clay = 1.58, Dry clay = 0.25, 
                            #Saturated sand = 2.2,Dry sand = 0.3,Oke, 1988
annualT.K<-annualT+273.15                                   #soil temp at K, B44
ini.S.Temp<-read.csv("input/Initial S Temp.csv",header=T)   #This is the soil temperature at 100 on May 1 2020
ini.S.Temp[300,1]<-annualT.K
ini.S.Temp<-na_interpolation(ini.S.Temp,option="linear")#initial soil temp was assumed to annual air
ini.S.Temp<-as.vector(ini.S.Temp[1:300,])
Cp.s<-1220                  #specific heat of soil, J/(kgK), B45,
                            #Saturated Clay = 1550,Dry clay = 890
                            #Saturated sand = 1480, Dry sand = 800, Oke, 1988
