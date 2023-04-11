#Create Soil and M temp matrix 
S.Temp <- matrix(nrow = 300,ncol = 288)              #Soil temp
M.Temp <- matrix(ncol = 288,nrow = 30)               #manure temp calculation


# Set Output Headers and Write parameters to Output
Output <- data.frame(matrix(ncol = 17,nrow = d.length - 365))
colnames(Output) <- c("Date ID","Year","Month","Day","DOY","Temperature.C","Depth.cm","Volume.m3"
                    ,"Evaporation.cm","Precipitation.cm","total radiation","snow depth"
                    ,"temp.05","temp.15","temp.25","In.M.temp","Solar absorb")
Output$`Date ID` <- as.numeric(seq(as.Date(start.date), as.Date(end.date), by = "days"))
Output$Year <- format(seq(as.Date(start.date),as.Date(end.date),by = "days"),"%Y")
Output$Month <- format(seq(as.Date(start.date),as.Date(end.date),by = "days"),"%m")
Output$Day <- format(seq(as.Date(start.date),as.Date(end.date),by = "days"),"%d")
Output$DOY <- as.numeric(strftime(seq(as.Date(start.date), as.Date(end.date), by = "days"),format = "%j"))
Output <- rbind(Output[1:365,],Output)
#Used for manure storage
n <- c(1:30)                    #cell numbers
delta.zu <- c(0:29)
delta.zd <- c(1:30)
zu <- c(0:29)
zd <- c(1:30)
delta.z <- c(1:30)
zp <- c(1:30)

#Tank properties
Rain.v <- (sum(Envir.daily$precip[1:365])/10^3)*Au    #yearly precipitation volume, m3
Max.storage <- M.storage + Rain.v
Rain.max <- Rain.v/Au                                 # height of max precipitation fall in a day,m
Zmmax <- M.depth                                      # Depth of manure, m
Tank.design <- M.storage/Au + Freeboard + sludge + Rain.max # m
rho.m <- 996.4 + 4.439 * Total.solid                  # Manure density, kg/m3
cell.n <- 30                                          # number of cell
cell.size <- 2                                        # cell size ratio
grid.c <- cell.size^(1/((cell.n/2) - 1)) - 1          # grid constant

#Manure constant for enthalpy
k.m <- 0.6173 - 0.0069 * Total.solid             # Manure thermal conductivity, W/(mK)
C.pm <- 4187.5 - 28.9 * Total.solid              # Manure specific heat, J/kg K
C.pm.fusion <- C.pm + 334000                     # Frozen Manure specific heat-fusion, J/kg K
E.272 <- f.point * rho.m * C.pm/10^6             # Enthalpy at freezing point (MJ/m3
E.273 <- E.272 + (1*rho.m*C.pm.fusion)/10^6      # Enthalpy at thawing point (MJ/m3)
fusion <- rho.m*C.pm.fusion/10^6                 # Fusion of manure

#Air constant
Pa <- 101325*exp(-A/8200)                     # Local air pressure, Pa
e.sigma <- 5.67*10^-8                         # Stefan-Boltzmann constant
Vair <- 14.2*10^-6                            # air kinematic viscosity, m2 s-1
Pr <- 0.714                                   # Air Prandtl Number, unitless
                                              # Evaporation rate calculation, Campbell&Norman, 1998.


rho.w <- 1000                                 # water density. kg/m
Lambda <- 2.45*10^6                           # latent heat of vaporization, J kg-1 

#Preparation for heat transfer calculation
T.delta <- 300                                          
T.step <- c(1:288)                                       
T.second <- seq(300,by = 300,length.out = length(T.step))   
T.hour <- seq(300/3600,24,length.out = length(T.step))    
H <- 15*(T.hour - 12)                                     #hour angle

#Soil temperature properties
dep.s <- 0.01                                           #depth of soil slice
ks.cp <- ks/Cp.s                                        #thermal conductivity/specific heat

