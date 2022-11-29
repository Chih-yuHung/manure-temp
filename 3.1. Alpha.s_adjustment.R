#This's part to adjust alpha.s 
#I assumed that the snow would only presence on a day of precepitation, i.e no accumulation 
#The snow is form when the temperature is below 0
Tmean<-(AirTmax1+AirTmin1)/2
snow<-c() # cm
if (Tmean <=0){
  snow     <- precip.d*100*10 #precipt.d is m, turn to cm and *10 to snow  
  albedo   <- (0.9280*snow/(0.0429+snow)) #Pervocich et al. 2017
  max(0.55,albedo) # the minimum is 0.55 follow we did in the DNDC
  alpha.s  <- 1-albedo
}else {
  snow<-0
  alpha.s<-0.9
  }
 
#Obtain a model for albedo based on Perovich et al. 2017,
#Light reflection and transmission by a temperate snow cover
# depth<-c(0,0.25,0.5,1.5,3,5,8,10,12,15) #snow depth, cm
# albe<-c(0.2,0.38,0.6,0.78,0.82,0.88,0.88,0.89,0.91,0.92) #albedo
# m<-nls(albe~a*depth/(b+depth))
# cor(albe,predict(m))
# plot(depth,albe)
# lines(depth,predict(m))    
# a= 0.9280, b=0.3152

