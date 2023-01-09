#This is to know the vertical temperature distribution
#of measured and simulated temperature
#observed data
obs.Fittja<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/temp.Fittja.daily.csv",header=T) 
#simulated data after calibration and modification
sim.Fittja<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/with shade/Fittja_fourdate2022-08-02.csv",header=T)
sim.Fittja.og<-read.csv("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/original/Fittja_fourdate2022-07-18.csv",header=T)

#simulated data
plot(sim.Fittja[,8],sim.Fittja[,7],xlim=c(-10,35),
     ylim=c(0,3.5),las=1,pch=5,
     xlab="Temperature(°C)",
     ylab="Manure Depth (m)")
for(i in c(0:2)) {
points(sim.Fittja[,2*i+2],sim.Fittja[,2*i+1],pch=i)
}

#Observation data, May 1, Aug 1, Nov 1, Feb1
points(obs.Fittja[1,6:8],obs.Fittja[1,c(10:12)],pch=15)
points(obs.Fittja[93,6:8],obs.Fittja[93,c(10:12)],pch=16)
points(obs.Fittja[185,6:8],obs.Fittja[185,c(10:12)],pch=17)
points(obs.Fittja[277,6:8],obs.Fittja[277,c(10:12)],pch=18)
#Jun 1, Jul 1, Aug 1, Sep 1.
points(obs.Fittja[32,6:8],obs.Fittja[32,c(10:12)],pch=15)
points(obs.Fittja[62,6:8],obs.Fittja[62,c(10:12)],pch=16)
points(obs.Fittja[93,6:8],obs.Fittja[93,c(10:12)],pch=17)
points(obs.Fittja[124,6:8],obs.Fittja[124,c(10:12)],pch=18)
#Nov 1, Dec 1, Jan 1, Feb 1.
points(obs.Fittja[185,6:8],obs.Fittja[185,c(10:12)],pch=15)
points(obs.Fittja[215,6:8],obs.Fittja[215,c(10:12)],pch=16)
points(obs.Fittja[246,6:8],obs.Fittja[246,c(10:12)],pch=17)
points(obs.Fittja[277,6:8],obs.Fittja[277,c(10:12)],pch=18)
#"2022-6-18","2022-8-24","2022-9-7","2022-10-21", these are the date we have depths
points(obs.Fittja[49,6:8],obs.Fittja[49,c(10:12)],pch=15)
points(obs.Fittja[116,6:8],obs.Fittja[116,c(10:12)],pch=16)
points(obs.Fittja[130,6:8],obs.Fittja[130,c(10:12)],pch=17)
points(obs.Fittja[174,6:8],obs.Fittja[174,c(10:12)],pch=18)



#legend(-10,3.4,c("May 1","Aug 1","Nov 1","Feb 1"),
#       pch=c(0,1,2,5),ncol=2,bty="n")

legend(-10,3.4,c("Jun 1","Jul 1","Aug 1","Sep 1"),
       pch=c(0,1,2,5),ncol=2,bty="n")

legend(-10,3.4,c("Nov 1","Dec 1","Jan 1","Feb 1"),
       pch=c(0,1,2,5),ncol=2,bty="n")


#Simulated data without shade effect
for(i in c(2,4,6,8)) {
  lines(sim.Fittja.og[,i],sim.Fittja.og[,i-1])
}

