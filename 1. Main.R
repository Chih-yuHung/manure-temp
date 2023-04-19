library(REdaS); library(xlsx); library(beepr) ;library(dplyr); library(imputeTS)
#Set location, initial date and end time; date origin in R, 1970-1-1

test <- 0  #the test number
Location <- "OR"#VA or OR
for (submodels in 0:1) {#1 with revised submodel, 0 is without
source("3. Parameters.R",echo = F)  #Parameters we can change
source("4. Constants.R",echo = F)   #Constants no need to change
#The major loop for original model
source("2. Major loop.R",echo = F)
}
source("Result comparison.R",echo = F)
source("stat output.R", echo =  F)



