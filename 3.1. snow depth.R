#Simulate snow pack depth, cm because rain:snow = 1:10
snow.f <- ifelse((Envir.daily$AirTmax1 + Envir.daily$AirTmin1)/2 <= 0 | 
                   Envir.daily$AirTmax1 <= 1, Envir.daily$precip,0)
#The number 1 degree for AirTamx1 from Jennings et al., 2018 
#https://doi.org/10.1038/s41467-018-03629-7
#create a vector for rainfall, cm
rain <- ifelse(snow.f == 0,Envir.daily$precip/10,0)
#from Trnka et al. 2010, doi:10.1016/j.agrformet.2010.04.012
#parameters 31 explanation, 
# 1: minimum critical temp, no melt below
# 2: max critical temp, no melt if Tmax below this and Tmin below 0
# 3: melting rate, cm, degree C-1, day-1
# 4: Sublimation threshold, sublimation ouccrs when snow depth > this, cm
# 5: sublimation amount when snow depth > the threshold, cm
melt <- ifelse(Envir.daily$AirTmin1 <= parameters[1,31] |
               (Envir.daily$AirTmax1 < parameters[2,31] & Envir.daily$AirTmin1 <= 0),
               0, (Envir.daily$AirTmin1 + abs(parameters[1,31])) * parameters[3,31])
melt <- melt[c(d.length,1:(d.length - 1))] #potential melt depth (cm)
snow.p <- 0 #snow pack depth, cm

for (i in 2:d.length) {
  if (snow.f[i] > 0 | snow.p[i-1] > 0) {
    snow.p[i] <- snow.f[i] + snow.p[i-1] - melt[i] 
    if (snow.p[i] >= (parameters[4,31])) {
      snow.p[i] <- snow.p[i] - (parameters[5,31])
    }
  } else {
    snow.p[i] <- 0
  }
}

snow.p <- ifelse(snow.p < 0, 0, snow.p) #snow pack depth on surface
melt.act <- ifelse((lag(snow.p) - snow.p) <= 0 | is.na(lag(snow.p) - snow.p),
                   0,lag(snow.p) - snow.p) # actual melted snow depth (cm)


