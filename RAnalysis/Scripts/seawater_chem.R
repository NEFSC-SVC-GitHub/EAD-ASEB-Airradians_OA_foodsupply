
title: "Water Chemistry"
author: "Sam GUrr"
date: "11/22/2021"


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
chem    <- data.frame(read.csv(file="Data/Seawater_chemistry/Water_Chemistry_FoodxOA_Challenge.csv", header=T))[,c(1:20)] %>%  na.omit()




# Summary table chemistry
chemTable <- chem[,c(3,4,6,11,13:14,16:20)] %>% 
    group_by(pH,Food.Treatment) %>%
    summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
View(chemTable)
names(chem)
summarySE(chem, measurevar=c("pCO2.out..matm.","WAr.out"), groupvars=c("pH", "Food.Treatment"))

