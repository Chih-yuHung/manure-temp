#Enthalpy calculation

#Temp and depth adjustment, F200:Q238
#Current enthalpy, J209:J238
Enthalpy.c <- ifelse(M.Temp[,288] < 272.15,M.Temp[,288]*rho.m*M.volume*C.pm/10^6
                   ,ifelse(M.Temp[,288] >= 273.15,(272.15*rho.m*M.volume*C.pm + rho.m*M.volume*C.pm.fusion +  (M.Temp[,288] - 273.15)*rho.m*M.volume*C.pm)/10^6
                           ,(272.15*rho.m*M.volume*C.pm + (M.Temp[,288] - 272.15)*rho.m*M.volume*C.pm.fusion)/10^6))


if (submodels == 1) {
 if (203 <= T.day & T.day <= 220) {
   In.M.temp <- Tmean + 10
   } else {
   In.M.temp <- Avg.Barn.temp + Barn.temp.amp*sin(2*pi/365*T.day + Temp.cost) #Incoming manure temp, L49,L39
 } 
 #In.M.temp <- annualT #not better than the original result
 #In.M.temp <- Tmean # bad
 #In.M.temp <- ifelse(Tmean<=0,0,Tmean*0.6488+6.7341) #based on three meaurement, bad results.
 #Assumed the M.Temp is well mixed after every 5 day
 #because of manure input
  if (snow > 0) {
    if (i %% mixing.day == 0) {
      #incoming Manure from the sump pit  
      depthchange.d <- sum(M.daily[(i - 4):i]) + precip.d - Evap.depth.d
      if (M.depth <= 1.5) {
        M.Temp[21:30,288] <- mean(M.Temp[21:30,288])
      } else {
        M.Temp[26:30,288] <- mean(M.Temp[26:30,288])  
      }
    } else{
      depthchange.d <- precip.d - Evap.depth.d      #without manure input
    }     
    #Enthalpy after manure added, N209:N238
    depth.factor <- (depthchange.d - precip.d)/M.depth  #subtract precip.d becuase I use it to adjust manure depth
    delta.z.new <- delta.z*(1 + depth.factor)
    M.volume.new <- delta.z.new*Au
    Enthalpy.c.new <- Enthalpy.c + 
         (M.volume.new - M.volume)*rho.m*((In.M.temp*C.pm) + 272.15*C.pm + C.pm.fusion)/10^6
    Enthalpy.c.new[1] <- Enthalpy.c.new[1:2] + 
                        ((precip.d*Au)*rho.m*T.air.K[288]*C.pm/10^6/2)
    Enthalpy.V <- Enthalpy.c.new/M.volume.new  #Enthalpy/V, O209:O238
  } else { 
   if (i %% mixing.day == 0) {
  #incoming Manure from the sump pit  
  depthchange.d <- sum(M.daily[(i - 4):i]) + precip.d - Evap.depth.d
  if (M.depth <= 1.5) {
  M.Temp[21:30,288] <- mean(M.Temp[21:30,288])
  } else {
  M.Temp[26:30,288] <- mean(M.Temp[26:30,288])  
  }
  } else{
  depthchange.d <- precip.d - Evap.depth.d      #without manure input
  }     
  #Enthalpy after manure added, N209:N238
  depth.factor <- depthchange.d/M.depth
  delta.z.new <- delta.z*(1 + depth.factor)
  M.volume.new <- delta.z.new*Au
  Enthalpy.c.new <- Enthalpy.c + 
                   (M.volume.new - M.volume)*rho.m *
                  ((In.M.temp*C.pm) + 272.15*C.pm + C.pm.fusion)/1000000
  Enthalpy.V <- Enthalpy.c.new/M.volume.new  #Enthalpy/V, O209:O238
  }
} else {
#In.M.temp<-annualT #incoming manure temperature
In.M.temp <- Avg.Barn.temp + Barn.temp.amp*sin(2*pi/365*T.day + Temp.cost) #Incoming manure temp, L49,L39
depthchange.d <- M.daily[i] + precip.d - Evap.depth.d #L34
depth.factor <- depthchange.d/M.depth                   #N204
delta.z.new <- delta.z*(1 + depth.factor)                 #L209:238
M.volume.new <- delta.z.new*Au                          #new manure volume,M209:M238
#Enthalpy after manure added, N209:N238
Enthalpy.c.new <- Enthalpy.c + (M.volume.new - M.volume) *
                 rho.m*((In.M.temp*C.pm) + 272.15*C.pm + C.pm.fusion)/1000000
Enthalpy.V <- Enthalpy.c.new/M.volume.new  #Enthalpy/V, O209:O238
}


#Final temp after depth adjustment,Q209:Q238
#This is actually the manure temperature after manure addition and we used this 
#to be the new initial manure temp for the next day
#not the manure temp at the end of the day!
Final.M.Temp <- ifelse(Enthalpy.V < E.272, 272.15*Enthalpy.V/E.272,
                     ifelse(Enthalpy.V >= E.273, 
                            273.15 + (Enthalpy.V - E.273)*10^6/(C.pm*rho.m),
                            272.15 + (Enthalpy.V - E.272)/fusion))
if (submodels == 1) {
  if (M.depth <= 1.5) {
    Final.M.Temp[21:30] <- mean(Final.M.Temp[21:30])
  } else {
    Final.M.Temp[26:30] <- mean(Final.M.Temp[26:30])  
  }
}

if (mean(Final.M.Temp) >= (50 + 273.15) | mean(Final.M.Temp) <= (-10 + 273.15)) {
  cat("Manure temperature too high/low to be true")
  cat(mean(Final.M.Temp) - 273.15)
  break
}



