
# title: "Water Chemistry"
# author: "Sam GUrr"
# date: "11/22/2021"


# LOAD PACKAGES
library(dplyr)
library(ggplot2)
library(kableExtra)
library(data.table)
library(stringr)
library(latex2exp)
library(Rmisc)
library(aggregate)



# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # personal computer
setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer



# LOAD DATA :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
chem    <- data.frame(read.csv(file="Data/Seawater_chemistry/Water_Chemistry_FoodxOA_Challenge.csv", header=T))[,c(1:20)] %>%  
  dplyr::filter(!(Date %in% "10/12/2021" & Food.Treatment %in% 'Low')) %>% # omit 10/12/2021 measurements for the 'Low' Food treatment (check notebook, I think CO2 turned off overnight?) 
  dplyr::filter(!X %in% 'checking the system') %>%  # ommit all occurances of 'checks' of the system
  na.omit()
chem


# Summary table chemistry
chemTable <- chem[,c(3,4,6,11,13:14,16:20)] %>% 
    group_by(pH,Food.Treatment) %>%
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
chemTable_n <- chem[,c(3,4,6,11,13:14,16:20)] %>% 
  group_by(pH,Food.Treatment)  %>% 
  dplyr::summarise(n = n())

ChemTable_MeanSE <- data.frame(matrix(nrow = 4, ncol = 1))
ChemTable_MeanSE$pCO2_level   <-  c('High','High','Low','Low')
ChemTable_MeanSE$Food_supply  <-  c('High','Low','High','Low')
ChemTable_MeanSE$pHXfood <- paste(chemTable$pH,chemTable$Food.Treatment, sep = '*')
ChemTable_MeanSE$N <- c(21,16,21,16)
ChemTable_MeanSE$Salinity <- paste(signif(chemTable$Salinity_mean,digits=3), signif(chemTable$Salinity_se,digits=3), sep=" ± ")
ChemTable_MeanSE$Temperature <- paste(signif(chemTable$t.oC..of.bucket_mean,digits=3), signif(chemTable$t.oC..of.bucket_se,digits=3), sep=" ± ")
ChemTable_MeanSE$DO <- paste(signif(chemTable$DO.mg.L_mean,digits=3), signif(chemTable$DO.mg.L_se,digits=3), sep=" ± ")
ChemTable_MeanSE$pH <- paste(signif(chemTable$pH.out_mean,digits=3), signif(chemTable$pH.out_se,digits=3), sep=" ± ")
ChemTable_MeanSE$pCO2 <- paste(signif(chemTable$pCO2.out..matm._mean,digits=3), signif(chemTable$pCO2.out..matm._se,digits=3), sep=" ± ")
# ChemTable_MeanSE$DIC <- paste(chemTable$mean.DIC, chemTable$sem.DIC, sep=" ± ")
ChemTable_MeanSE$TA <- paste(signif(chemTable$TA..mmol.kgSW._mean,digits=3), signif(chemTable$TA..mmol.kgSW._se,digits=3), sep=" ± ")
ChemTable_MeanSE$Aragonite.Sat <- paste(signif(chemTable$WAr.out_mean,digits=3), signif(chemTable$WAr.out_se,digits=3), sep=" ± ")
ChemTable_MeanSE$Calcite.Sat <- paste(signif(chemTable$WCa.out_mean,digits=3), signif(chemTable$WCa.out_se,digits=3), sep=" ± ")
ChemTable_MeanSE <- ChemTable_MeanSE[,-1] # view table
View(ChemTable_MeanSE)
