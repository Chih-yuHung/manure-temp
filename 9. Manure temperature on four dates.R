#This part retrieves the vertical manure temperature at various depth on 
#May 1, Aug 1, Nov 1, and Feb 1. in the last year. 
#Create a dataframe
if (four.date==1){
temp.four<-data.frame(depth1=c(1:30),
                      temp1=rep(0,30),
                      depth2=c(1:30),
                      temp2=rep(0,30),
                      depth3=c(1:30),
                      temp3=rep(0,30),
                      depth4=c(1:30),
                      temp4=rep(0,30))
}
temp.four[,2]<-M.temp.d
temp.four[,2-1]<-cumsum(delta.z.new)
l<-l+2

if (submodels == 1) {
  #Shade/output to an excel file
  write.csv(temp.four,paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/with shade/"
                         ,Location,"_fourdate",Sys.Date(),".csv",sep=""),row.names = FALSE)
} else {
  #Without shade/output to an excel file
  write.csv(temp.four,paste("C:/Users/hungc/OneDrive - AGR-AGR/AAFC/Project 3_Sweden/3. Results/Fittja/original/"
                         ,Location,"_fourdate",Sys.Date(),".csv",sep=""),row.names = FALSE)
}
