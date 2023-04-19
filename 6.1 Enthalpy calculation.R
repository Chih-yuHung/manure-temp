#Enthalpy calculation

#Temp and depth adjustment
#Current enthalpy
Enthalpy.c <- ifelse(M.Temp[,288] < f.point,M.Temp[,288]*rho.m*M.volume*C.pm/10^6
                   ,ifelse(M.Temp[,288] >= t.point,(f.point*rho.m*M.volume*C.pm + rho.m*M.volume*C.pm.fusion + 
                                                      (M.Temp[,288] - t.point)*rho.m*M.volume*C.pm)/10^6
                           ,(f.point*rho.m*M.volume*C.pm + (M.Temp[,288] - f.point)*rho.m*M.volume*C.pm.fusion)/10^6))

if (submodels == 1) {
  #incoming manure temp.
  In.M.temp <- Avg.Barn.temp + Barn.temp.amp*sin(2*pi/365*T.day + Temp.cost) # Incoming manure temp
 
  #daily depth change
  depthchange.d <- if (i %% mixing.day == 0) {
    sum(M.daily[(i - mixing.day + 1):i]) + precip.d - Evap.depth.d
  } else {
    precip.d - Evap.depth.d
  }
  
  #determine the enthalpy before add precipitation
  depth.factor <- depthchange.d/M.depth
  delta.z.new <- delta.z * (1 + depth.factor)
  M.volume.new <- delta.z.new * Au
  Enthalpy.c.new <- Enthalpy.c + (M.volume.new - M.volume) *
    rho.m * ((In.M.temp + f.point) * C.pm + C.pm.fusion) / 10^6
  
  #determine enthalpy after precipitation
  if (melt.act[i] > 0) {
    Enthalpy.c.new[1:3] <- Enthalpy.c.new[1:3] + (precip.d * Au) * rho.m * mean(T.air.K) * C.pm / 10^6 / 3
  } else {
    Enthalpy.c.new[1:3] <- Enthalpy.c.new[1:3] + (precip.d * Au) * rho.m * (mean(T.air.K) * C.pm + C.pm.fusion) / 10^6 / 3
  }
  
  M.volume.new[1:3] <- M.volume.new[1:3] + precip.d * Au / 3
  Enthalpy.V <- Enthalpy.c.new / M.volume.new
  
} else {
  #Situation in submodel = 0
  In.M.temp <- 18  #Pexas et al. 2021
  #16 is an optimal temperature for growing pigs. 
  depthchange.d <- M.daily[i] + precip.d - Evap.depth.d 
  depth.factor <- depthchange.d/M.depth                   
  delta.z.new <- delta.z*(1 + depth.factor)               
  M.volume.new <- delta.z.new*Au                         #new manure volume
  #Enthalpy after manure added
  Enthalpy.c.new <- Enthalpy.c + (M.volume.new - M.volume) *
    rho.m*((In.M.temp*C.pm) + 272.15*C.pm + C.pm.fusion)/1000000
  Enthalpy.V <- Enthalpy.c.new/M.volume.new  #Enthalpy/V
}

#Final temp after depth adjustment,
#This is the new initial manure temp for the next day
#not the manure temp at the end of the day!
Final.M.Temp <- ifelse(Enthalpy.V < E.272, f.point*Enthalpy.V/E.272,
                     ifelse(Enthalpy.V >= E.273, 
                            t.point + (Enthalpy.V - E.273)*10^6/(C.pm*rho.m),
                            f.point + (Enthalpy.V - E.272)/fusion))

# mix manure after removal
if (submodels == 1 & i %% mixing.day == 0) {
  mix.range <- floor(min(30,round(30*(100/sum(M.volume.new)),0))/2) #the cells to be mixed
  outlet.cell <- which.min(abs(mix.place - seq(M.depth,0,length.out = 30))) #outlet place
  mix.cells <- c((outlet.cell - mix.range):(outlet.cell + mix.range))
  mix.cells <- unique(pmin(pmax(mix.cells,1),30))
  Final.M.Temp[mix.cells] <- mean(Final.M.Temp[mix.cells])  
  }

# check if temperature simulation
if (mean(Final.M.Temp) >= (50 + 273.15) |
    mean(Final.M.Temp[1]) >= AirTmax1 + 10 + 273.15 |
    mean(Final.M.Temp) <= (-10 + 273.15) ) {
  cat("Manure temperature too high/low to be true")
  cat(Final.M.Temp - 273.15)
  break
}



