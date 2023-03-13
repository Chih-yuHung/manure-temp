#This's part to adjust alpha.s 
#I assumed that the snow would only presence on a day of precepitation, i.e no accumulation 
#The snow is form when the temperature is below 0
if (snow.p[i] > 0) {
  albedo <- (0.9280*snow.p[i]/(0.3152 + snow.p[i])) #Perovich et al. 2007
  albedo <- max(0.55,albedo) # the minimum is 0.55 follow we did in the DNDC
  alpha.s <- 1 - albedo
  epsilon  <- 0.98 #assume surface is frozen when snow presence
  Teten.H2Ob <- Teten.Iceb
  Teten.H2Oc <- Teten.Icec
} else {
  alpha.s <- parameters[1,20]
  epsilon <- parameters[1,24]
  Teten.H2Ob <- parameters[2,29]
  Teten.H2Oc <- parameters[3,29]
  }

#Obtain a model for albedo based on Perovich et al. 2007,
#Light reflection and transmission by a temperate snow cover
 # depth<-c(0,0.25,0.5,1.5,3,5,8,10,12,15) #snow depth, cm
 # albe<-c(0.2,0.38,0.6,0.78,0.82,0.88,0.88,0.89,0.91,0.92) #albedo
 # m<-nls(albe~a*depth/(b+depth))
 # cor(albe,predict(m))
 # plot(depth,albe)
 # lines(depth,predict(m))    
 # a= 0.9280, b=0.3152

