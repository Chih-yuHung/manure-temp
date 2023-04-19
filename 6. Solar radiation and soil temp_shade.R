#This file calculate heat transfer and it iterate everyday

#Environmental input
ReL <- wind*ri/Vair*1.3                               #Reynold's number, unitless
Nu <- ifelse(ReL<5*10^5,0.453*(ReL^(0.5))*(Pr^(1/3))  #Nusselt Number
           ,(0.037*(ReL^(4/5))-871)*(Pr^(1/3)))    
hcv.ms <- (Nu*ka)/ri                                  #Heat transfer coefficient       

#Radiative heat transfer
declination.s <- 23.45*sin((2*pi*(284+T.day)/365))             # seasonal declination(degree)
sin.alpha <- pmax((cos(deg2rad(L))*cos(deg2rad(declination.s))
             *cos(deg2rad(H))+sin(deg2rad(L))
             *sin(deg2rad(declination.s))),0)                   # sunlight degree

#This's a part to calculate shadow area due to the tank wall, it's not in Rennie, 2017
if (submodels == 1) {
wall.h <- Htank-M.depth                              # the wall height above manure surface, m
cot.alpha <- (1-sin.alpha^2)^(1/2)/sin.alpha
cos.theta <- (wall.h*cot.alpha/2)/ri                 # the angle in the circle-circle intersection, a numeric
deg.theta <- acos(cos.theta)
Intersection.h <- ri*(1-cos.theta^2)^(1/2)           # the height of triangle in the circle-circle intersection, m
shadow <- pi*ri^2-(4*pi*ri^2*deg.theta/(2*pi)
        -4*(wall.h*cot.alpha)/2*Intersection.h/2)  # shadow area, m2
light.d <- 1-(shadow/Au)                             # the percentage that sunlight on the surface, between 0-1
light.d[is.nan(light.d)] <- 1
##End for shadow calculation
m  <- ifelse(sin.alpha>0,Pa/(101325*sin.alpha),0)      # Optical air mass number
Sb <- ifelse(sin.alpha>0, Eb*(tau^m)*sin.alpha,0)      # solar bean radiation (Wh/m2)
Sd <- ifelse(sin.alpha>0,0.3*(1-tau^m)*Eb*sin.alpha,0) # Diffusive radiation (wh/m2)
Sr.total <- sum(Sb,Sd)                                 # Total solar radiation
q.net.rad <- alpha.s*light.d*((Sb+Sd)/Sr.total)*((SR*1000*1000)/T.delta) #Net solar radiation
              #apply shade coefficient  
} else {
m <- ifelse(sin.alpha>0,Pa/(101325*sin.alpha),0)       # Optical air mass number
Sb <- ifelse(sin.alpha>0, Eb*(tau^m)*sin.alpha,0)      # solar bean radiation (Wh/m2)
Sd <- ifelse(sin.alpha>0,0.3*(1-tau^m)*Eb*sin.alpha,0) # Diffusive radiation (wh/m2)
Sr.total <- sum(Sb,Sd)                                 # Total solar radiation
q.net.rad <- alpha.s*((Sb+Sd)/Sr.total)*((SR*1000*1000)/T.delta) #Net solar radiation
}

#Relative humidity from measured data
#Rh estimated based on RH6 and RH15 with T.hour
Rh <- c(1:288)  
Rh[1:71] <- -((RH6-RH15)/2)*cos((-9-T.hour[1:71])*pi/15)+((RH6+RH15)/2)
Rh[72:180] <- ((RH6-RH15)/2)*cos((6-T.hour[72:180])*pi/9)+((RH6+RH15)/2)
Rh[181:288] <- ((RH6-RH15)/2)*cos((6-T.hour[181:288])*pi/9)+((RH6+RH15)/2)


#Estimate air temp.
sunrise <- T.hour[which(sin.alpha>0,arr.ind=TRUE)[1]] #determine sunrise time

#hr, sunrise reference, sunrise= 0, an hour before sunrise =23
sunrise.ref <- ifelse(T.hour<sunrise,24+T.hour-sunrise
       ,T.hour-sunrise)
#x, in Schaub,1991
x <- ifelse(sunrise.ref >= 0 & sunrise.ref <= 14-sunrise
         ,x <- cos(sunrise.ref*pi/(14-sunrise))
         ,x <- cos((sunrise.ref-((14-sunrise)+1))*(pi/(23-(14-sunrise)))))

#phase, to determine the phase of sunrise, 1.before sunrise, 2, 
#after sunrise before sunset, 3, after sunset
T.air <- ifelse(T.hour<sunrise,(AirTmax0-AirTmin1)/2*x+((AirTmax0+AirTmin1)/2)
       ,ifelse(sunrise.ref>=0 & sunrise.ref<=(14-sunrise),
               (-((AirTmax1-AirTmin1)/2)*x+(AirTmax1+AirTmin1)/2)
               ,(((AirTmax1-AirTmin2)/2)*x+(AirTmax1+AirTmin2)/2)))

T.air.K <- T.air+273.15


#emissivity of atmosphere
e.ac <- 1.72*(((Teten.H2Oa*exp((Teten.H2Ob*(T.air))/(Teten.H2Oc+(T.air)))*(Rh/100))/T.air.K)^(1/7))
#cloud-corrected air emissivity 
e.a <- (1-0.84*cc)*e.ac+0.84*cc

#Soil temperature, 300 cells, 2.995m
S.Temp[300,] <- annualT.K   #The deepest soil temperature was assumed to be annual T
#Need delta.z[30] source manure volume
delta.depth <- delta.z[30]/2+dep.s/2
soil.c <- T.delta/(den.s*(Au*dep.s))#constant of soil


#Thermal conductivity/specific heat correction
#Manure temperature calculation
#soil temperature cacultation,
#The process is to calculate 5 mins thermal conductivity 
#and then 5 mins Manure temperature from soil temp. and pre. manure temp
#soil temp was from pre. soil temp and manure temp.
#use the Manure temp in previous 5 mins and calculate thermal conductivity
Cp <- c(1:288)#Specific heat of manure, two values, frozen or liquid 
T.conductivity <- matrix(ncol=288,nrow=30) # Conductivity
delta.T.evap <- c(1:288) #delta T-evap
delta.T.radevap <- c(1:288)   #delta T-rad+evap
WVPD <- c(1:288)
E <- c(1:288)

for (j in 1:288) {
  if (j == 1) {
  T.conductivity[,j]<-ifelse(ini.M.Temp>=t.point|ini.M.Temp<f.point,k.m/C.pm,k.m/C.pm.fusion)
  Cp[j]<-ifelse(ini.M.Temp[j]>=t.point|ini.M.Temp[j]<f.point,C.pm,C.pm.fusion)
  WVPD[j] <- Teten.H2Oa*exp((Teten.H2Ob*(ini.M.Temp[j]-273.15))/(Teten.H2Oc+ini.M.Temp[j]-273.15)) -
    (Teten.H2Oa*exp((Teten.H2Ob*T.air[j])/(Teten.H2Oc+T.air[j]))*Rh[j]/100)
  E[j] <- rho.w*(WVPD[j])*wind.f/(24*3600*1000)*Au
  delta.T.evap[j]<-(-(E[j]*Lambda*T.delta))/(Cp[j]*(rho.m*Au*delta.z[1]))
  delta.T.radevap[j]<-(q.net.rad[j]*Au*T.delta-(e.sigma*Au*T.delta*(((epsilon*(ini.M.Temp[j])^4))-(e.a[j]*T.air.K[j]^4))))/(rho.m*Cp[j]*Au*delta.z[1])+delta.T.evap[j]
  M.Temp[1,j]<-(ini.M.Temp[j]+time.weight[1]*T.conductivity[1,j]*(Au*hcv.ms*T.air.K[j]+Au/delta.zd[1]*ini.M.Temp[j+1]))/(1+time.weight[1]*T.conductivity[1,j]*(Au*hcv.ms+Au/delta.zd[1]))+delta.T.radevap[j]
  M.Temp[2:29,j]<-(ini.M.Temp[2:29]+time.weight[2:29]*T.conductivity[2:29,j]*(Au/delta.zu[2:29]*ini.M.Temp[1:28]+Au/delta.zd[2:29]*ini.M.Temp[3:30]))/(1+time.weight[2:29]*T.conductivity[2:29,j]*(Au/delta.zu[2:29]+Au/delta.zd[2:29]))
  M.Temp[30,j]<-(ini.M.Temp[30]+time.weight[30]*T.conductivity[30,j]*(Au/delta.zu[30]*ini.M.Temp[29]+Au/delta.depth*ini.S.Temp[1]))/(1+time.weight[30]*T.conductivity[30,j]*(Au/delta.zu[30]+Au/delta.depth))
  S.Temp[1,j]<-(ini.S.Temp[1]+soil.c*ks.cp*(Au/delta.depth*ini.M.Temp[30]+Au/dep.s*ini.S.Temp[2]))/(1+soil.c*ks.cp*(Au/delta.depth+Au/dep.s))
  S.Temp[2:299,j]<-(ini.S.Temp[2:299]+soil.c*ks.cp*(Au/dep.s*ini.S.Temp[1:298]+Au/dep.s*ini.S.Temp[3:300]))/(1+soil.c*ks.cp*(Au/dep.s+Au/dep.s))
  } else {
  T.conductivity[,j]<-ifelse(M.Temp[,j-1]>=t.point|M.Temp[,j-1]<f.point,k.m/C.pm,k.m/C.pm.fusion)  
  Cp[j]<-ifelse(M.Temp[1,j-1]>=t.point|M.Temp[1,j-1]<f.point,C.pm,C.pm.fusion)
  WVPD[j] <- Teten.H2Oa*exp((Teten.H2Ob*(M.Temp[j-1]-273.15))/(Teten.H2Oc+M.Temp[j-1]-273.15)) -
    (Teten.H2Oa*exp((Teten.H2Ob*T.air[j-1])/(Teten.H2Oc+T.air[j-1]))*Rh[j-1]/100)
  E[j] <- rho.w*(WVPD[j])*wind.f/(24*3600*1000)*Au
  delta.T.evap[j]<-(-(E[j]*Lambda*T.delta)/(Cp[j]*(rho.m*Au*delta.z[1])))
  delta.T.radevap[j]<-(q.net.rad[j]*Au*T.delta-(e.sigma*Au*T.delta*((epsilon*(M.Temp[1,j-1])^4)-(e.a[j]*T.air.K[j]^4))))/(rho.m*Cp[j]*Au*delta.z[1])+delta.T.evap[j]
  M.Temp[1,j]<-(M.Temp[1,j-1]+time.weight[1]*T.conductivity[1,j]*(Au*hcv.ms*T.air.K[j]+Au/delta.zd[1]*M.Temp[2,j-1]))/(1+time.weight[1]*T.conductivity[1,j]*(Au*hcv.ms+Au/delta.zd[1]))+delta.T.radevap[j]
  M.Temp[2:29,j]<-(M.Temp[2:29,j-1]+time.weight[2:29]*T.conductivity[2:29,j]*(Au/delta.zu[2:29]*M.Temp[1:28,j-1]+Au/delta.zd[2:29]*M.Temp[3:30,j-1]))/(1+time.weight[2:29]*T.conductivity[2:29,j]*(Au/delta.zu[2:29]+Au/delta.zd[2:29]))
  M.Temp[30,j]<-(M.Temp[30,j-1]+time.weight[30]*T.conductivity[30,j]*(Au/delta.zu[30]*M.Temp[29,j-1]+Au/delta.depth*S.Temp[1,j-1]))/(1+time.weight[30]*T.conductivity[30,j]*(Au/delta.zu[30]+Au/delta.depth))
  S.Temp[1,j]<-(S.Temp[1,j-1]+soil.c*ks.cp*(Au/delta.depth*M.Temp[30,j-1]+Au/dep.s*S.Temp[2,j-1]))/(1+soil.c*ks.cp*(Au/delta.depth+Au/dep.s))
  S.Temp[2:299,j]<-(S.Temp[2:299,j-1]+soil.c*ks.cp*(Au/dep.s*S.Temp[1:298,j-1]+Au/dep.s*S.Temp[3:300,j-1]))/(1+soil.c*ks.cp*(Au/dep.s+Au/dep.s))
  }
}

Evap.depth.d <- sum(E*T.delta)/rho.w/Au #Incorporate daily evaporation, depth together
if (snow > 0) {
  Evap.depth.d <- Evap.depth.d * max(1 - (63.369 * exp(-0.307 * T.air)/100),0.4)
  #emperical model,Meira Neto, A.A et al., 2020  exponential equation in Fig 1 c. 
  #https://doi.org/10.1038/s43247-020-00056-9
}

Evap.depth.d <- max(Evap.depth.d,0)
