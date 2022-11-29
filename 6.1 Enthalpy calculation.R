#Enthalpy calculation

#Temp and depth adjustment, F200:Q238
#Current enthalpy, J209:J238
Enthalpy.c<-ifelse(M.Temp[,288]<272.15,M.Temp[,288]*rho.m*M.volume*C.pm/10^6
                   ,ifelse(M.Temp[,288]>=273.15,(272.15*rho.m*M.volume*C.pm+rho.m*M.volume*C.pm.fusion+(M.Temp[,288]-273.15)*rho.m*M.volume*C.pm)/10^6
                           ,(272.15*rho.m*M.volume*C.pm+(M.Temp[,288]-272.15)*rho.m*M.volume*C.pm.fusion)/10^6))

if (submodels == 1) {
 #In.M.temp<-annualT #not better than the original result
 #In.M.temp<-Avg.Barn.temp+Barn.temp.amp*sin(2*pi/365*T.day+Temp.cost) #Incoming manure temp, L49,L39
 In.M.temp<-ifelse(Tmean<=0,0,Tmean)
  #Assumed the M.Temp is well mixed after every 5 day
 #because of manure input
  if (i %% mixing.day == 0) {
  #incoming Manure from the sump pit  
  depthchange.d<-M.daily*mixing.day+precip.d-Evap.depth.d
  if(M.depth <= 1.5) {
  M.Temp[,288]<-mean(M.Temp[,288])
  } else {
  M.Temp[1:10,288]<-mean(M.Temp[1:10,288])  
  }
  }else{
  depthchange.d<-precip.d-Evap.depth.d      #without manure input
  }     
  #Enthalpy after manure added, N209:N238
  depth.factor<-depthchange.d/M.depth
  delta.z.new<-delta.z*(1+depth.factor)
  M.volume.new<-delta.z.new*Au
  Enthalpy.c.new<-Enthalpy.c+(M.volume.new-M.volume)*rho.m*((In.M.temp*C.pm)+272.15*C.pm+C.pm.fusion)/1000000
  Enthalpy.V<-Enthalpy.c.new/M.volume.new  #Enthalpy/V, O209:O238
} else {
#In.M.temp<-annualT #incoming manure temperature
In.M.temp<-Avg.Barn.temp+Barn.temp.amp*sin(2*pi/365*T.day+Temp.cost) #Incoming manure temp, L49,L39
depthchange.d<-M.daily+precip.d-Evap.depth.d #L34
depth.factor<-depthchange.d/M.depth                   #N204
delta.z.new<-delta.z*(1+depth.factor)                 #L209:238
M.volume.new<-delta.z.new*Au                          #new manure volume,M209:M238
#Enthalpy after manure added, N209:N238
Enthalpy.c.new<-Enthalpy.c+(M.volume.new-M.volume)*rho.m*((In.M.temp*C.pm)+272.15*C.pm+C.pm.fusion)/1000000
Enthalpy.V<-Enthalpy.c.new/M.volume.new  #Enthalpy/V, O209:O238
}


#Final temp after depth adjustment,Q209:Q238
#This is actually the manure temperature after manure addition and we used this 
#to be the new initial manure temp for the next day
#not the manure temp at the end of the day!
Final.M.Temp<-ifelse(Enthalpy.V<E.272,272.15*Enthalpy.V/E.272,
                     ifelse(Enthalpy.V>=E.273,273.15+(Enthalpy.V-E.273)*10^6/(C.pm*rho.m)
                            ,272.15+(Enthalpy.V-E.272)/fusion))

if (mean(Final.M.Temp)>=(50+273.15)) {
  cat("Manure temperature too high to be true")
  break
}



