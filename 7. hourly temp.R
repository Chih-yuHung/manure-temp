#For hourly Manure temp calculation and output final manure temperature
#it's the temperature, we used to know the manure temp after a day
#We assumed that new manure was added after this. 
M.Temp.h <- matrix(nrow = 30,ncol = 24) #hourly manure temp
for (j in 1:30) {
  for (k in 1:24) {
   M.Temp.h[j,k] <- mean(M.Temp[j,(1 + (12*(k - 1))):(12*k)])
  }
}
#AD244:AD273
M.Temp.d <- rowMeans(M.Temp.h) # K
M.temp.d <- M.Temp.d - 273.15 #degree C
#volume*Temp
VT.h <- matrix(nrow = 30,ncol = 24)
VT.h <- M.Temp.h*M.volume

Avg.VT.h <- M.Temp.d*M.volume                      
Avg.M.Temp.h <- colSums(VT.h)/M.volume.current     #average hourly M temp
Avg.M.Temp.d <- mean(Final.M.Temp)                 #average daily M temp(k), 
Avg.M.temp.d <- Avg.M.Temp.d - 273.15              #average daily M temp(c)