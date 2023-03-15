#This is to know the shadow ratio in tanks of different diamters
library(REdaS)
Lat<-c(25,35,45,55)           #latitude
Htank<-5                      #height of tank
M.depth<-3                    #fixed manure depth
ri<-c(5,10,20,30,40)          #Inner radius of tank, m, B32
Eb<-1395                      #extraterrestrial solar flux density, W m-2
tau<-0.75                     #Atmospheric transimttance, 0.75 clear, 0.4 overcast
A<-7.3                        #altitude, m
Pa<-101325*exp(-A/8200)                     # Local air pressure, Pa
T.day.light<-matrix(1:365*5,nrow=365,ncol=5,byrow = T) #save the daily light.d
data<-list()
#Preparation for heat transfer calculation
for(k in 1:4){
L<-Lat[k]
for(i in 1:5){
for(j in 1:365){
Au<-ri[i]^2*pi 
T.day<-j                                              #DOY 
T.delta<-300                                          #time step, 300 sec
T.step<-c(1:288)                                      #vector for time step
T.second<-seq(300,by=300,length.out=length(T.step))   #vector for seconds
T.hour<-seq(300/3600,24,length.out=length(T.step))    #vector for hours
H<-15*(T.hour-12)                                     #a coefficient related to angle of sunlight

#Radiative heat transfer
declination.s<-23.45*sin((2*pi*(284+T.day)/365))               # seasonal declination(degree)
sin.alpha<-pmax((cos(deg2rad(L))*cos(deg2rad(declination.s))
                 *cos(deg2rad(H))+sin(deg2rad(L))
                 *sin(deg2rad(declination.s))),0)              # sunlight degree

#This's a part to calculate shadow area due to the tank wall, it's not in Rennie, 2017
wall.h<-Htank-M.depth                                 # the wall height above manure surface, m
cot.alpha<-(1-sin.alpha^2)^(1/2)/sin.alpha
cos.theta<-(wall.h*cot.alpha/2)/ri[i]                 # the angle in the circle-circle intersection, a numeric
deg.theta<-acos(cos.theta)
Intersection.h<-ri[i]*(1-cos.theta^2)^(1/2)           # the height of triangle in the circle-circle intersection, m
shadow<-pi*ri[i]^2-(4*pi*ri[i]^2*deg.theta/(2*pi)
                 -4*(wall.h*cot.alpha)/2*Intersection.h/2)  # shadow area, m2
light.d<-1-(shadow/Au)                                # the percentage that sunlight on the surface, between 0-1
light.d[is.nan(light.d)]<-0
###End for shadow calculation
m<-ifelse(sin.alpha>0,Pa/(101325*sin.alpha),0)       # Optical air mass number, #F103-KG103
Sb<-ifelse(sin.alpha>0, Eb*(tau^m)*sin.alpha,0)      # solar bean radiation (W/m2),F104-KG104
Sd<-ifelse(sin.alpha>0,0.3*(1-tau^m)*Eb*sin.alpha,0) # Diffusive radiation (w/m2),F105-KG105
#cat(c(i,j,max(light.d)))
q.net<-light.d*Sb+Sd
#q.net<-light.d*(Sb+Sd)
T.day.light[j,i]<-mean(q.net)*(5/60)*288/1000 #(kwh/ m2 /d)
data[[k]]<-T.day.light
}
}
}
#export at 1100 X 600 
par(mfrow=c(2,2))
for (k in 1:4){
ifelse(k==1| k==3,par(mar=c(5,5,4,2)),par(mar=c(5,5,4,2)))
cols<-c("#a6bddb","#74a9cf","#3690c0",
        "#0570b0","#034e7b")
plot(data[[k]][,5],type="l",col=cols[5],ylim=c(0,12),
     xlim=c(0,366),las=1,cex.axis=1.2,
     yaxs="i",xaxs="i",lwd=2,
     xlab="Day of year",ylab="",cex.lab=1.5)
text(1,11,paste("Latitude=",Lat[k],"°N",sep=""),pos=4,cex=1.5)
ifelse(k==1,legend(40,6,rev(c("5m","10m","20m","30m","40m")),
                   col=rev(cols),lty=1,cex=1.3,lwd=2,
                   title="Tank radius",bty="n",ncol=2),NA)
for (i in 1:4) {
lines(data[[k]][,i],col=cols[i])
}
if (k ==1 |k ==3){
  mtext(expression(paste("Daily irradiation (kWh ",m^-2,")")),cex=1.3,side=2,line=2.5)
}
}




