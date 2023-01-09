# To obtain temp and depth at the end of the day
# I need manure temperature at 0.5, 1.5m and 2.5 depth every day for the last year
# make a column and divide the current depth by 30 
# compare the values, find the most close values to 0.5, 1.5 and 2.5 m
# retrieve the values as temperature at these 3 depths
M.temp.depth <- rep(0,3)
if (is.element(i,tail(1:d.length,n = 365))) {
  layers <- seq(from = 0, to = M.depth,length.out = 30)
  depth.three <- c(0.5,1.5,2.5)
  for (j in 1:3) {
  M.temp.depth[j] <- M.temp.d[which.min(abs(layers - depth.three[j]))]
  }
} 
