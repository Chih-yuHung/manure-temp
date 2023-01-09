
#manure depth adjustment
#daily changing depth of manure for next day, L32<-L37
if (Output$`Date ID`[i] %in% removal.duration[[1]] |
    Output$`Date ID`[i] %in% removal.duration[[5]] |
    Output$`Date ID`[i] %in% removal.duration[[9]]) {
  removal.depth.d <- removal.depth[1]/removal.day[1] #the difference between the peak and the minimum 
  cat(paste("manure removal date =",i))
  M.depth <- M.depth - removal.depth.d + depthchange.d
  Zmmax <- M.depth
} else if (Output$`Date ID`[i] %in% removal.duration[[2]] |
          Output$`Date ID`[i] %in% removal.duration[[6]] |
          Output$`Date ID`[i] %in% removal.duration[[10]]) {
  removal.depth.d <- removal.depth[2]/removal.day[2]    
  cat(paste("manure removal date =",i))
  M.depth <- M.depth - removal.depth.d + depthchange.d
  Zmmax <- M.depth
} else if (Output$`Date ID`[i] %in% removal.duration[[3]] |
          Output$`Date ID`[i] %in% removal.duration[[7]] |
          Output$`Date ID`[i] %in% removal.duration[[11]]) {
  removal.depth.d <- (M.depth - removal.depth[3])/removal.day[3]    
  cat(paste("manure removal date =",i))
  M.depth <- M.depth - removal.depth.d + depthchange.d
  Zmmax <- M.depth
} else if (Output$`Date ID`[i] %in% removal.duration[[4]] |
          Output$`Date ID`[i] %in% removal.duration[[8]] |
          Output$`Date ID`[i] %in% removal.duration[[12]]) {
  removal.depth.d <- (M.depth - removal.depth[4])/removal.day[4]    
  cat(paste("manure removal date =",i))
  M.depth <- M.depth - removal.depth.d + depthchange.d
  Zmmax <- M.depth
} else {
  M.depth <- M.depth + depthchange.d
  Zmmax <- M.depth    
  print(M.depth)
}