#manure depth adjustment
#daily changing depth of manure for next day
M.depth <- M.depth + depthchange.d
Zmmax <- M.depth    
print(M.depth)
for (removal.cycle in 1:length(removal.depth)) {
  if (Output$`Date ID`[i] %in% removal.duration[[removal.cycle]]) {
  removal.depth.d <- (removal.depth[removal.cycle])/removal.day[removal.cycle]    
  cat(paste("manure removal date =",i))
  M.depth <- M.depth - removal.depth.d + depthchange.d
  Zmmax <- M.depth
  Final.M.Temp[1:30] <- mean(Final.M.Temp) # assumed manure was well-mixed after removal
  break
  } 
}


