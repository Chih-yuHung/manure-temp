#For hourly Manure temp calculation and out final manure temperature
#It's before volume and enthalpy adjustment.
#To my understanding, this is the temperature, we used to know the manure temp after a day
#We assumed that new manure was added after this. 
#F244:AC273
M.Temp.h<-matrix(nrow=30,ncol=24) #hourly manure temp
for(j in 1:30) {
  for (k in 1:24){
   M.Temp.h[j,k]<-mean(M.Temp[j,(1+(12*(k-1))):(12*k)])
  }
}
#AD244:AD273
M.Temp.d<-rowMeans(M.Temp.h) # K
M.temp.d<-M.Temp.d-273.15 #degree C
#volume*Temp, AG244:BD273
VT.h<-matrix(nrow=30,ncol=24)
VT.h<-M.Temp.h*M.volume

Avg.VT.h<-M.Temp.d*M.volume                      #BE244:273
Avg.M.Temp.h<-colSums(VT.h)/M.volume.current     #average hourly M temp,F275:AC275
Avg.M.Temp.d<-sum(Avg.VT.h)/M.volume.current     #average daily M temp(k), AD275 = F277 
Avg.M.temp.d<-Avg.M.Temp.d-273.15                #average daily M temp(c), AE275 = F278 = daily temp into output
