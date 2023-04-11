# Introduction to the repo
  This repo showed our script to simulate manure temperatures in two tanks in Sweden.
The manuscript "Measuring and modelling manure temperature at Swedish swine slurry storages" is submitted to Agriculture, Ecosystems & Environment on X, 2023. (Submission# will be provided). The authors are: Chih-Yu Hung, Kristina Mjöfors, Timothy Rennie, Brian Grant, Ward Smith, and Andrew VanderZaag. All author, except Kristina Mjöfors, is affliated with Agriculture and Agri-Food Canada, and Kristina is affliated with Research Institutes of Sweden (RISE).
People who are interesting in this repo may contact Chih-Yu Hung (chih-yu.hung@agr.gc.ca) and Andrew VanderZaag (andrew.vanderzaag@agr.gc.ca). 


# Data input
  Data input are in the folder **input**. Four input files are needed for the simulation. __daily env input_*.csv__ is the environmental input file. __Initial M temp.csv__ is the initial manure temperature and __Initial S temp.csv__ is the initial soil temperature. These two initial temperature barely influence simulation results because the a four-year simulation was conducted to stabilize the result. 
  Parameters input files are named with the tank name. __VA_0.csv__ and __OR_0.csv__ are provided to obtain the result in the manuscript. The environmental input files were organized with __Organize weather data_tank name.R__. The R script organize the data to be usable in the model. More information regarding the input files can be found in the manuscript. 
  Observed manure temperatures can be found in __Measured manure temp_tank name.R__. Description of the algorithm can be foubd in the supplementary document submitted to the journal. More information regarding the measured manure temperature and tank characteristics can be found in the manuscript. 
 
# Model description
##  Over all control
  Scripts 1 to 7 are the scripts for temperature simualtion. __1. Main.R__ is the script used to provide tank name and test number, which controls the parameter inputs,i.e. __VA_0.csv__ or __OR_0.csv__. People who wants to try the influence of parameters can adjust the parameters in the csv file. 
  In the __1. Main.R__, it reads __3.Parameters.R__ and __4.Constant.R__ before running the simulation __2. Main loop.R__. The previous two script read and calculate necessary parameters and variables for the simuation. The __2. Major loop.R__ then run the simulations that calculate snow depth, heat transfer between manure, air, and soil, and manure volume. Detailed information regarding the model is described in the manuscript.
  
## function of the script
  __3.1 snow depth.R__ calculate the snow depths (work while reading the parameters) and pass the snow depth to __3.2.Alpha.s_adjustment.R__ to obtain the new daily alpha.

  __5. Manure volume.R__ and __5.1 Manure volume removal.R__ control the daily manure volume, and please note that __5.1 Manure volume removal.R__ is a added submodel in this study.
  
  __6. Solar radiation and soil temp_shade.R__ is the major part to calcualte heat transfer. It calculates solar radiation with shade revision, evaporation heat transfer, convection heat transfer between air and manure and between manure layers, conduction heat transfer between manure layers and between manure and soil. Daily evaporation depth is alos obtained in this script. __6.1 Enthalpy calculation.R__ calculate the enthalpy at the end of day that influenced by precipitation and manure removal/addition. 
  
  __7. hourly temp.R__ and __7.1 temp at three depth.R__ do not involve in manure temperature calculation, but calculate results. 

## Sensitivity analysis
  The sensitivity analysis, __Sensitivity test.R__ calculated the sensitivity of solar irradiation on latitude and manure tank design. It is an independent file that does not use any scripts for model simulation. People wants to run their own sensitivity test can change the parameters in the script. Description regarding the test can be found in the manuscript.  
  
# Results
  Figure 3 and 4 are created with __Result comparison.R__ and Table 2,3 and Table S2 are calculated and created with __stat output.R__. Figure 5 is created with __Sensitivity test.R__.
