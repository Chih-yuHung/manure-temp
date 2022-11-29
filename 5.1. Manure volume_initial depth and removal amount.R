#Manure depth adjustment
#Reset M.depth and ini.M.temp after soil temperature stabilization
if (sum(i==366|i==731|i==1096|i==1461)==1){
  M.depth<-0.67
  Zmmax<-M.depth
}

#determine the removal amount of manure
# If current date = removal dates then update depth and average temperature
if(sum(removal.start[c(5,9,13)] == Output$`Date ID`[i]) == 1) {
  removal.depth.d<-(M.depth-removal.depth[1])/removal.day[1] #the difference between the peak and the minimum 
  cat(paste("manure removal date =",i))
}

if(sum(removal.start[c(6,10,14)] == Output$`Date ID`[i]) == 1) {
  removal.depth.d<-(M.depth-removal.depth[2])/removal.day[2]    
  cat(paste("manure removal date =",i))
}

if(sum(removal.start[c(7,11,15)] == Output$`Date ID`[i]) == 1) {
  removal.depth.d<-(M.depth-removal.depth[3])/removal.day[3]    
  cat(paste("manure removal date =",i))
}

if(sum(removal.start[c(8,12,16)] == Output$`Date ID`[i]) == 1) {
  removal.depth.d<-(M.depth-removal.depth[4])/removal.day[4]    
  cat(paste("manure removal date =",i))
}
