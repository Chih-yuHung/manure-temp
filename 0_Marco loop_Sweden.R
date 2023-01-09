library(REdaS); library(xlsx); library(beepr) ;library(dplyr); library(imputeTS)
#Set location, initial date and end time; date origin in R, 1970-1-1
test <- 3  #the test number


for (submodels in 0:1) {#1 with revised submodel, 0 is without
source("3. Parameters.R",echo = F)           #Parameters we can change
source("4. Constants_Fittja.R",echo = F)   #Constants no need to change
#The major loop for original model
source("0_Major loop.R",echo = F)
}

source("Result comparison_Fittja.R",echo = F)
source("stat output.R", echo =  F)



