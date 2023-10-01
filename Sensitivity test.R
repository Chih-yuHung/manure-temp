#Sensitivity test for solar radiation with submodel of shadow effect
#latitude: 25N, 35N, 45N, and 55N
#wall height: 6, 5, 4, 3m (diameter 30m, fixed manure depth at 2 m)
#diameter: 40, 30, 20, 10m (wall height 3m, fixed manure depth at 2 m)
library(REdaS);library(devEMF)
#Parameters
Lat <- c(25,35,45,55)           #latitude
Height <- c(6,5,4,3)             #height of tank
M.depth <- 2                    #fixed manure depth
ri <- c(5,10,15,20)            #Inner radius of tank, m, B32
Eb <- 1395                      #extraterrestrial solar flux density, W m-2
tau <- 0.75                     #Atmospheric transmittance, 0.75 clear, 0.4 overcast
A <- 14                        #altitude, m
Pa <- 101325*exp(-A/8200)                     # Local air pressure, Pa
T.day.light <- matrix(1:365*4,nrow=365,ncol=4,byrow = T) #save the daily light.d
data <- list() #for the first column, diameter changes

T.delta <- 300                                          #time step, 300 sec
T.step <- c(1:288)                                      #vector for time step
T.second <-seq(300,by=300,length.out=length(T.step))   #vector for seconds
T.hour <- seq(300/3600,24,length.out=length(T.step))    #vector for hours
H <- 15*(T.hour-12)                                     #a coefficient related to angle of sunlight


#First column, four latitude and diameters
Htank <- 3
for (k in 1:4) { #latitude
  L <- Lat[k]
    for (i in 1:4) { # diameters
      for (j in 1:365) {
        Au <- ri[i]^2*pi 
        T.day <- j                                              #DOY 

      #Radiative heat transfer
        declination.s <- 23.45*sin((2*pi*(284+T.day)/365))               # seasonal declination(degree)
        sin.alpha <- pmax((cos(deg2rad(L))*cos(deg2rad(declination.s))
                 *cos(deg2rad(H))+sin(deg2rad(L))
                 *sin(deg2rad(declination.s))),0)              # sunlight degree

        #This's a part to calculate shadow area due to the tank wall, it's not in Rennie, 2017
        wall.h <- Htank-M.depth                                 # the wall height above manure surface, m
        cot.alpha <- (1-sin.alpha^2)^(1/2)/sin.alpha
        cos.theta <- (wall.h*cot.alpha/2)/ri[i]                 # the angle in the circle-circle intersection, a numeric
        deg.theta <- acos(cos.theta)
        Intersection.h <- ri[i]*(1-cos.theta^2)^(1/2)           # the height of triangle in the circle-circle intersection, m
        shadow <- pi*ri[i]^2-(4*pi*ri[i]^2*deg.theta/(2*pi)
                 -4*(wall.h*cot.alpha)/2*Intersection.h/2)  # shadow area, m2
        light.d <- 1-(shadow/Au)                                # the percentage that sunlight on the surface, between 0-1
        light.d[is.nan(light.d)] <- 0
        ###End for shadow calculation
        m <- ifelse(sin.alpha>0,Pa/(101325*sin.alpha),0)       # Optical air mass number
        Sb <- ifelse(sin.alpha>0, Eb*(tau^m)*sin.alpha,0)      # solar bean radiation (W/m2)
        Sd <- ifelse(sin.alpha>0,0.3*(1-tau^m)*Eb*sin.alpha,0) # Diffusive radiation (w/m2)
        q.net <- light.d*(Sb+Sd)
        T.day.light[j,i] <- mean(q.net)*(5/60)*288/1000*3.6 #(MJ/ m2 /d)
      }
    }
  data[[k]] <- T.day.light
}

#Second column, four latitude and wall height
ri <- 15
for (k in 1:4) { #latitude
  L <- Lat[k]
  for (i in 1:4) { # diameters
    Htank <- Height[i]
    for (j in 1:365) {
      Au <- ri^2*pi
      T.day <- j                                              #DOY 
      
      #Radiative heat transfer
      declination.s <- 23.45*sin((2*pi*(284+T.day)/365))               # seasonal declination(degree)
      sin.alpha <- pmax((cos(deg2rad(L))*cos(deg2rad(declination.s))
                         *cos(deg2rad(H))+sin(deg2rad(L))
                         *sin(deg2rad(declination.s))),0)              # sunlight degree
      
      #This's a part to calculate shadow area due to the tank wall, it's not in Rennie, 2017
      wall.h <- Htank-M.depth                                 # the wall height above manure surface, m
      cot.alpha <- (1-sin.alpha^2)^(1/2)/sin.alpha
      cos.theta <- (wall.h*cot.alpha/2)/ri                 # the angle in the circle-circle intersection, a numeric
      deg.theta <- acos(cos.theta)
      Intersection.h <- ri*(1-cos.theta^2)^(1/2)           # the height of triangle in the circle-circle intersection, m
      shadow <- pi*ri^2-(4*pi*ri^2*deg.theta/(2*pi)
                            -4*(wall.h*cot.alpha)/2*Intersection.h/2)  # shadow area, m2
      light.d <- 1-(shadow/Au)                                # the percentage that sunlight on the surface, between 0-1
      light.d[is.nan(light.d)] <- 0
      ###End for shadow calculation
      m <- ifelse(sin.alpha>0,Pa/(101325*sin.alpha),0)       # Optical air mass number, #F103-KG103
      Sb <- ifelse(sin.alpha>0, Eb*(tau^m)*sin.alpha,0)      # solar bean radiation (W/m2),F104-KG104
      Sd <- ifelse(sin.alpha>0,0.3*(1-tau^m)*Eb*sin.alpha,0) # Diffusive radiation (w/m2),F105-KG105
      #cat(c(i,j,max(light.d)))
      q.net <- light.d*(Sb+Sd)
      T.day.light[j,i] <- mean(q.net)*(5/60)*288/1000*3.6 #(MJ/ m2 /d)
      
    }
  }
  data[[k+4]] <- T.day.light
}

#data [[1:4]] for diameter, data [[5:8]] for heights
#calculate cumulative radiation
data.cum <- data.frame(row.names = c("lat25","lat35",
                                     "lat45","lat55"),
                       d10 = rep(0,4),d20 = rep(0,4),
                       d30 = rep(0,4),d40 = rep(0,4),
                       h6 = rep(0,4),h5 = rep(0,4),
                       h4 = rep(0,4),h3 = rep(0,4)
                       )
for (k in 1:4) {
data.cum[k,1:4] <- round(colSums(data[[k]]),0)
}
for (k in 5:8) {
  data.cum[k-4,5:8] <- round(colSums(data[[k]]),0)
}



#two columns, 1 col with 4 rows (4 latitudes x 4 diameters)
# 2 col with 4 rows (4 latitudes x 4  wall heights)
#cols <- c("#211d0c","#8077ff","#fcab42","#42bd42")
cols <- c("#5A4F20","#9EACD5","#F6B8A1","#94D1A9")
plot.sens <- function() {
par(mfcol=c(4,2), mar = c(2,2,1,1),oma= c(4,5,0,0))
for (k in c(5:8,1:4)){
plot(0,type="l",ylim=c(0,35),
     xlim=c(0,366),las=1,cex.axis=1,
     yaxs="i",xaxs="i",
     xlab="",ylab="")
L <- as.character(ifelse(k < 5, Lat[k], Lat[k-4]))
text(1,31,paste0(L,"°N"),
                     pos=4,cex=1.5)
ifelse(k==1,legend(40,20,c("10m","20m","30m","40m"),
                   col=cols,lty=1,cex=1.3,lwd=1,
                   title="Tank diamter",bty="n",ncol=2),NA)
ifelse(k==5,legend(40,20,c("6m","5m","4m","3m"),
                   col=cols,lty=2,cex=1.3,lwd=1,
                   title="Tank wall height ",bty="n",ncol=2),NA)
if (k <=4) {
  for (i in 1:4) {
     lines(data[[k]][,i],col=cols[i],
      lwd = 1, lty = 1)
  }
} else {
    for (i in 1:4) {
      lines(data[[k]][,i],col=cols[i],
            lwd = 1, lty = 2)
    }   
  }
par(cex.lab = 2)
title(xlab = "Day of year",
      outer = TRUE, line = 2, font = 2)
title(ylab = expression(paste("Daily irradiation (MJ ",m^-2,")")),
      outer = TRUE, line = 1, font = 2)

}
}

#Output
stest <- "C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/sensitivity test/"
emf(file = paste(stest,"sensitivity.emf",sep = "")
    ,width = 6, height = 7,emfPlus = FALSE, family = "Calibri")
plot.sens()
dev.off()

tiff(file = paste(stest,"sensitivity.tiff",sep = ""),
     units = "in", width = 6, height = 7, res = 300)
plot.sens()
dev.off()



##prepared for possible questions from reviewer
# four latitude and diameters
data <- list()
ri <- 15
for (k in 1:4) { #latitude
  L <- Lat[k]
  for (i in 1:4) { # diameters
    Htank <- Height[i]
    M.depth <- Htank*0.7
    for (j in 1:365) {
      Au <- ri^2*pi
      T.day <- j                                              #DOY 
      
      #Radiative heat transfer
      declination.s <- 23.45*sin((2*pi*(284+T.day)/365))               # seasonal declination(degree)
      sin.alpha <- pmax((cos(deg2rad(L))*cos(deg2rad(declination.s))
                         *cos(deg2rad(H))+sin(deg2rad(L))
                         *sin(deg2rad(declination.s))),0)              # sunlight degree
      
      #This's a part to calculate shadow area due to the tank wall, it's not in Rennie, 2017
      wall.h <- Htank-M.depth                                 # the wall height above manure surface, m
      cot.alpha <- (1-sin.alpha^2)^(1/2)/sin.alpha
      cos.theta <- (wall.h*cot.alpha/2)/ri                 # the angle in the circle-circle intersection, a numeric
      deg.theta <- acos(cos.theta)
      Intersection.h <- ri*(1-cos.theta^2)^(1/2)           # the height of triangle in the circle-circle intersection, m
      shadow <- pi*ri^2-(4*pi*ri^2*deg.theta/(2*pi)
                         -4*(wall.h*cot.alpha)/2*Intersection.h/2)  # shadow area, m2
      light.d <- 1-(shadow/Au)                                # the percentage that sunlight on the surface, between 0-1
      light.d[is.nan(light.d)] <- 0
      ###End for shadow calculation
      m <- ifelse(sin.alpha>0,Pa/(101325*sin.alpha),0)       # Optical air mass number, #F103-KG103
      Sb <- ifelse(sin.alpha>0, Eb*(tau^m)*sin.alpha,0)      # solar bean radiation (W/m2),F104-KG104
      Sd <- ifelse(sin.alpha>0,0.3*(1-tau^m)*Eb*sin.alpha,0) # Diffusive radiation (w/m2),F105-KG105
      #cat(c(i,j,max(light.d)))
      q.net <- light.d*(Sb+Sd)
      T.day.light[j,i] <- mean(q.net)*(5/60)*288/1000*3.6 #(MJ/ m2 /d)
      
    }
  }
  data[[k]] <- T.day.light
}

cols <- c("#5A4F20","#9EACD5","#F6B8A1","#94D1A9")
plot.sens <- function() {
  par(mfcol=c(4,1), mar = c(2,2,1,1),oma= c(4,5,0,0))
  for (k in 1:4){
    plot(0,type="l",ylim=c(0,35),
         xlim=c(0,366),las=1,cex.axis=1,
         yaxs="i",xaxs="i",
         xlab="",ylab="")
    L <- as.character(ifelse(k < 5, Lat[k], Lat[k-4]))
    text(1,31,paste0(L,"°N"),
         pos=4,cex=1.5)
    ifelse(k==1,legend(40,20,c("10m","20m","30m","40m"),
                       col=cols,lty=1,cex=1.3,lwd=1,
                       title="Tank diamter",bty="n",ncol=2),NA)
    ifelse(k==5,legend(40,20,c("6m","5m","4m","3m"),
                       col=cols,lty=2,cex=1.3,lwd=1,
                       title="Tank wall height ",bty="n",ncol=2),NA)
    if (k <=4) {
      for (i in 1:4) {
        lines(data[[k]][,i],col=cols[i],
              lwd = 1, lty = 1)
      }
    } else {
      for (i in 1:4) {
        lines(data[[k]][,i],col=cols[i],
              lwd = 1, lty = 2)
      }   
    }
    par(cex.lab = 2)
    title(xlab = "Day of year",
          outer = TRUE, line = 2, font = 2)
    title(ylab = expression(paste("Daily irradiation (MJ ",m^-2,")")),
          outer = TRUE, line = 1, font = 2)
    
  }
}
