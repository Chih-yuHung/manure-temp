#Enthalpy calculation

#Temp and depth adjustment, F200:Q238
#Current enthalpy, J209:J238
Enthalpy.c <- ifelse(M.Temp[,288] < f.point,M.Temp[,288]*rho.m*M.volume*C.pm/10^6
                   ,ifelse(M.Temp[,288] >= t.point,(f.point*rho.m*M.volume*C.pm + rho.m*M.volume*C.pm.fusion + 
                                                      (M.Temp[,288] - t.point)*rho.m*M.volume*C.pm)/10^6
                           ,(f.point*rho.m*M.volume*C.pm + (M.Temp[,288] - f.point)*rho.m*M.volume*C.pm.fusion)/10^6))


if (submodels == 1) {
 In.M.temp <- Avg.Barn.temp + Barn.temp.amp*sin(2*pi/365*T.day + Temp.cost) #Incoming manure temp, L49,L39
 #In.M.temp <- annualT #not better than the original result
 #In.M.temp <- Tmean # bad
 #In.M.temp <- ifelse(Tmean<=0,0,Tmean*0.6488+6.7341) #based on three meaurement, bad results.
 #In.M.temp <- S.Temp[50,288]-273.15 #equal to soil temperature at 1 m depth
   #Assumed the M.Temp is well mixed after every 5 day
  #because of manure input
  if (snow > 0) {
    if (i %% mixing.day == 0) {
      #incoming Manure from the sump pit  
      depthchange.d <- sum(M.daily[(i - mixing.day + 1):i]) + precip.d - Evap.depth.d
      if (M.depth <= 1.5) {
        M.Temp[mix.pattern1,288] <- mean(M.Temp[mix.pattern1,288])
      } else {
        M.Temp[mix.pattern2,288] <- mean(M.Temp[mix.pattern2,288])  
      }
    } else{
      depthchange.d <- precip.d - Evap.depth.d      #without manure input
    }     
    #Enthalpy after manure added, N209:N238
    depth.factor <- (depthchange.d - precip.d)/M.depth  #subtract precip.d because I use it to adjust manure depth later
    delta.z.new <- delta.z*(1 + depth.factor)
    M.volume.new <- delta.z.new*Au
    #Enthalpy with incoming manure and after evaporation
    Enthalpy.c.new <- Enthalpy.c + 
         (M.volume.new - M.volume)*rho.m*((In.M.temp + f.point)*C.pm + C.pm.fusion)/10^6
    Enthalpy.c.new[1:3] <- Enthalpy.c.new[1:3] + 
                         ((precip.d*Au)*rho.m*mean(T.air.K)*C.pm/10^6)/3
    M.volume.new[1:3] <- M.volume.new[1:3] + (precip.d*Au/3) # add the precip to manure volume back
    Enthalpy.V <- Enthalpy.c.new/M.volume.new  #Enthalpy/V, O209:O238
  } else { 
   if (i %% mixing.day == 0) {
  #incoming Manure from the sump pit  
  depthchange.d <- sum(M.daily[(i - mixing.day + 1):i]) + precip.d - Evap.depth.d
  if (M.depth <= 1.5) {
  M.Temp[mix.pattern1,288] <- mean(M.Temp[mix.pattern1,288])
  } else {
  M.Temp[mix.pattern2,288] <- mean(M.Temp[mix.pattern2,288])  
  }
  } else{
  depthchange.d <- precip.d - Evap.depth.d      #without manure input
  }     
  #Enthalpy after manure added, N209:N238
   depth.factor <- (depthchange.d - precip.d)/M.depth  #subtract precip.d becuase I use it to adjust manure depth
   delta.z.new <- delta.z*(1 + depth.factor)
   M.volume.new <- delta.z.new*Au
   Enthalpy.c.new <- Enthalpy.c + 
      (M.volume.new - M.volume)*rho.m*((In.M.temp + f.point)*C.pm + C.pm.fusion)/10^6
   Enthalpy.c.new[1:3] <- Enthalpy.c.new[1:3] + 
      ((precip.d*Au)*rho.m*(mean(T.air.K)*C.pm + C.pm.fusion)/10^6)/3
   M.volume.new[1:3] <- M.volume.new[1:3] + (precip.d*Au/3) # add the precip to manure volume back
   Enthalpy.V <- Enthalpy.c.new/M.volume.new  #Enthalpy/V, O209:O238
  }
} else {
#Situation in submodel = 0
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


#Final temp after depth adjustment,
#This is the new initial manure temp for the next day
#not the manure temp at the end of the day!
Final.M.Temp <- ifelse(Enthalpy.V < E.272, f.point*Enthalpy.V/E.272,
                     ifelse(Enthalpy.V >= E.273, 
                            t.point + (Enthalpy.V - E.273)*10^6/(C.pm*rho.m),
                            f.point + (Enthalpy.V - E.272)/fusion))

#simulated possible convective heat transfer
# Final.M.Temp.t <- Final.M.Temp-273.15
# Final.M.Temp.1 <- Final.M.Temp[]
# 
if (submodels == 1 & i %% mixing.day == 0) {
  if (M.depth <= 1.5) {
    Final.M.Temp[mix.pattern1] <- mean(Final.M.Temp[mix.pattern1])
  } else {
    Final.M.Temp[mix.pattern2] <- mean(Final.M.Temp[mix.pattern2])  
  }
}

if (mean(Final.M.Temp) >= (50 + 273.15) |
    mean(Final.M.Temp[1]) >= AirTmax1 + 10 + 273.15 |
    mean(Final.M.Temp) <= (-10 + 273.15) ) {
  cat("Manure temperature too high/low to be true")
  cat(Final.M.Temp - 273.15)
  break
}



