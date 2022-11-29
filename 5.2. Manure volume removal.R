#manure depth adjustment
#daily changing depth of manure for next day, L32<-L37
if (Output$`Date ID`[i] %in% removal.duration[[1]]|
    Output$`Date ID`[i] %in% removal.duration[[5]]|
    Output$`Date ID`[i] %in% removal.duration[[9]]) {
  M.depth<-M.depth-removal.depth.d
  Zmmax<-M.depth
} else if(Output$`Date ID`[i] %in% removal.duration[[2]]|
          Output$`Date ID`[i] %in% removal.duration[[6]]|
          Output$`Date ID`[i] %in% removal.duration[[10]]) {
  M.depth<-M.depth-removal.depth.d
  Zmmax<-M.depth
} else if(Output$`Date ID`[i] %in% removal.duration[[3]]|
          Output$`Date ID`[i] %in% removal.duration[[7]]|
          Output$`Date ID`[i] %in% removal.duration[[11]]) {
  M.depth<-M.depth-removal.depth.d
  Zmmax<-M.depth
} else if(Output$`Date ID`[i] %in% removal.duration[[4]]|
          Output$`Date ID`[i] %in% removal.duration[[8]]|
          Output$`Date ID`[i] %in% removal.duration[[12]]) {
  M.depth<-M.depth-removal.depth.d
  Zmmax<-M.depth
} else {
  M.depth<-M.depth+depthchange.d
  Zmmax<-M.depth    
  print(M.depth)
}