library(REdaS); library(xlsx); library(beepr) ;library(dplyr); library(imputeTS)


parameters <- read.csv(paste("input/",Location,"/",Location,"_",test,".csv",
                             sep = ""),header = T)
#This file has all adjustable parameters that may influence our results. 
start.date <- as.character(as.Date(parameters[1,3],format = "%m/%d/%Y"))  
end.date <- as.character(as.Date(parameters[1,4],format = "%m/%d/%Y")) 
removal.start <- as.numeric(as.Date(parameters[,5],format = "%m/%d/%Y"))
removal.start <- removal.start[!is.na(removal.start)]
removal.end <- as.numeric(as.Date(parameters[,6],format = "%m/%d/%Y"))
removal.end <- removal.end[!is.na(removal.end)]

removal.day <- rep((removal.end - removal.start)[1:(length(removal.start)/4)] + 1,3)
removal.duration <- list()
for (i in (length(removal.start)/4 + 1):length(removal.start)) { 
  removal.duration[[i - (length(removal.start)/4)]] <- c(removal.start[i]:removal.end[i])
}

#Environmental input
Envir.daily <- read.csv(paste("input/daily env input_",Location,".csv",sep = ""),header = T)
#To produce an extra year for balance soil temperature
Envir.daily <- Envir.daily[c(1:365,1:1095),]
d.length <- nrow(Envir.daily)
#initial manure temp
ini.M.Temp <- read.csv("input/Initial M temp.csv",header = T)[,1]#change to vector

if (submodels == 1) {
#obtain snow accumulation, cm
source("3.1. snow depth.R", echo = FALSE)
}

mixing.day <- as.integer(parameters[1,7])
mix.place <- parameters[2,7] #it's the pipe height from the bottom (m)

#Tank properties
Htank <- parameters[1,8]       #height of tank, m
ri <- parameters[1,9]          #Inner radius of tank, m
Au <- ri^2*pi                  #tank area, m2
Tank.v <- Au*Htank             #Total tank volume, m3

#Annual manure input
if (submodels == 0) {
M.storage <- parameters[1,10]
M.daily <- rep(M.storage/365/Au,d.length)
} else {
M.storage <- parameters[1,10]  
washout <- rep(parameters[2,10] * parameters[3,10] / 1000 / Au / 2,2) 
#convert to depth m, the number in parameter is pig amount, 70 kg pig-1,  

#a vector to know the daily manure input
M.factor <- na.omit(parameters[,11])   #manure input adjust factor
f.day <- parameters[1,12]     #days in feed barn
M.daily <- c()
for (cycle in 1:length(M.factor)) {
M.daily.temp <- c(rep(M.storage*M.factor[cycle]/365/Au,f.day),washout,0)
M.daily <- c(M.daily,M.daily.temp)
  }
if (length(M.daily) < 365) {
  M.daily <- rep(M.daily,2)
 }
 M.daily <- rep(M.daily[1:365],4)
}
 
Freeboard <- parameters[1,13]   #freeboard, m
sludge <- parameters[1,14]      #m

#Manure depth
M.depth <- parameters[1,15]     #This is the initial manure depth, m,
removal.depth <- rep(na.omit(parameters[,16])/Au,3) #the depth to be removed from tank


#Manure properties
Total.solid <- parameters[1,17]  #It barely influences the manure temperature 
#Input manure temperature
annualT <- mean(c(Envir.daily$AirTmax1,Envir.daily$AirTmin1)) #for ini. soil temp, assume equal to mean annual air temp
Avg.Barn.temp <- parameters[1,18]  #degree C, avg. annual barn temp
Barn.temp.amp <- parameters[2,18]  #degree C, amplitude of annual temp
Temp.cost <- parameters[3,18]      #Temp phase constant, barely influence the result

#Solar data
L <- parameters[1,19]           #Latitude
alpha.s <- parameters[1,20]     #solar absorptivity, 
Eb <- parameters[1,21]          #extraterrestrial solar flux density, W m-2
tau <- parameters[1,22]         #Atmospheric transmittance, 0.75 clear, 0.4 overcast
A <- parameters[1,23]           #altitude, m
epsilon <- parameters[1,24]     #emissivity

#Soil temperature properties 
den.s <- parameters[1,25]   #Soil density, kg/m3, B41,Saturated Clay = 2000,
                            #Dry clay = 1600,Saturated sand = 2000
ks <- parameters[1,26]      #soil thermal conductivity,W/mk,
                            #Saturated Clay = 1.58, Dry clay = 0.25, 
                            #Saturated sand = 2.2,Dry sand = 0.3,Oke, 1988
annualT.K <- annualT + 273.15                                   #soil temp at K
ini.S.Temp <- read.csv("input/Initial S Temp.csv",header = T)   
ini.S.Temp[300,1] <- annualT.K
ini.S.Temp <- na_interpolation(ini.S.Temp,option = "linear")#initial soil temp was assumed to annual air
ini.S.Temp <- as.vector(ini.S.Temp[1:300,])
Cp.s <- parameters[1,27]      #specific heat of soil, J/(kgK),
                              #Saturated Clay = 1550,Dry clay = 890
                              #Saturated sand = 1480, Dry sand = 800, Oke, 1988

#Air constant
ka <- parameters[1,28]                #air thermal conductivity, W/(mK), 
                                      #it's sensitive in summer only; if higher -> lower manure temp
Teten.H2Oa <- parameters[1,29]        # kPa
Teten.H2Ob <- parameters[2,29]        # unitless
Teten.H2Oc <- parameters[3,29]        # degree C
Teten.Iceb <- parameters[4,29]        # unitless
Teten.Icec <- parameters[5,29]        # degree C
#Water constant
f.point <- parameters[1,30]  #freezing point
t.point <- parameters[2,30]  #thawing point
  
