library(imputeTS) # for NA interpolation in soil temp

#Create Soil and M temp matrix 
S.Temp<-matrix(nrow=300,ncol=288)              #Soil temp
M.Temp<-matrix(ncol=288,nrow=30)               #manure temp calculation, F133:KG162

# Set Output Headers and Write parameters to Output
Output<-data.frame(matrix(ncol = 15,nrow=(d.length-365)))
colnames(Output)<-c("Date ID","Year","Month","Day","DOY","Temperature.C","Depth.cm","Volume.m3"
                    ,"Evaporation.cm","Precipitation.cm","total radiation","snow depth"
                    ,"temp.05","temp.15","temp.25")
Output$`Date ID`<-as.numeric(seq(as.Date(start.date), as.Date(end.date), by = "days"))
Output$Year<-format(seq(as.Date(start.date),as.Date(end.date),by="days"),"%Y")
Output$Month<-format(seq(as.Date(start.date),as.Date(end.date),by="days"),"%m")
Output$Day<-format(seq(as.Date(start.date),as.Date(end.date),by="days"),"%d")
Output$DOY<-as.numeric(strftime(seq(as.Date(start.date), as.Date(end.date), by = "days"),format = "%j"))
Output<-rbind(Output[1:365,],Output)
#Used for manure storage
n<-c(1:30)                    #cell numbers, P60:P89
delta.zu<-c(0:29)
delta.zd<-c(1:30)
zu<-c(0:29)
zd<-c(1:30)
delta.z<-c(1:30)
zp<-c(1:30)

#Tank properties
Rain.v<-(sum(Envir.daily$precip[1:365])/10^3)*Au    #yearly precipitation volume, m3, M28
Max.storage<-M.storage+Rain.v
Rain.max<-Rain.v/Au                                 # height of max precipitation fall in a day,m, P35
Zmmax<-M.depth                                      # Depth of manure, m, B31
Tank.design<-M.storage/Au+Freeboard+sludge+Rain.max # m, P37
rho.m<-996.4+4.439*Total.solid                      # Manure density, kg/m3, B48,R28
cell.n<-30                                          # number of cell,F59
cell.size<-2                                        # cell size ratio, F60
grid.c<-cell.size^(1/((cell.n/2)-1))-1              # grid constant, F58

#Manure constant for enthalpy
k.m <- 0.6173-0.0069*Total.solid            # Manure thermal conductivity, W/(mK), B47
C.pm<- 4187.5-28.9*Total.solid                # Manure specific heat, J/kg K, B49,F49, was 4187.5
C.pm.fusion<-C.pm+334000                    # Frozen Manure specific heat-fusion, J/kg K, F48, 334000
E.272<-272.15*rho.m*C.pm/10^6               # Enthalpy at 272.15 (MJ/m3),S202
E.273<-E.272+(1*rho.m*C.pm.fusion)/10^6     # Enthalpy at 273.15 (MJ/m3),S203
fusion<-rho.m*C.pm.fusion/10^6              # Fusion of manure
  #There is an added enthalpy per volume (S204) but wasn't used, I skipped it. 

#Air constant
Pa<-101325*exp(-A/8200)                     # Local air pressure, Pa
e.sigma<-5.67*10^-8                         # Stefan-Boltzmann constant, B25
ka<-0.025                                   # air thermal conductivity, W/(mK), B11
Vair<-14.2*10^-6                            # air kinematic viscosity, m2 s-1, B13
Pr<-0.714                                   # Air Prandtl Number, unitless, B12
                                            # Evaporation rate calculation, Campbell&Norman, 1998.
Teten.H2Oa<-0.611                           # kPa, G9
Teten.H2Ob<-17.502                          # unitless, G10
Teten.H2Oc<-240.97                          # degree C, G11
Teten.Iceb<-21.87                           # unitless, G12
Teten.Icec<-265.5                           # degree C, G13
rho.w<-1000                                 # water density. kg/m3, G14
Lambda<-2.45*10^6                           # latent heat of vaporization, J kg-1, G15  

#Preparation for heat transfer calculation
T.delta<-300                                          #F62
T.step<-c(1:288)                                      #F93:KG93   
T.second<-seq(300,by=300,length.out=length(T.step))   #F94:KG94
T.hour<-seq(300/3600,24,length.out=length(T.step))    #F95:KG95
H<-15*(T.hour-12)                                     #hour angle, F100:KG100

#Soil temperature properties
dep.s<-0.01                                           #depth of soil slice, F9101
ks.cp<-ks/Cp.s                                        #thermal conductivity/specific heat, B46,F9102

