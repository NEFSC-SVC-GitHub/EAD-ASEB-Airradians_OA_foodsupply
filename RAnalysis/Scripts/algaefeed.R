# title: "Algaefeed"
# author: "Sam GUrr"
# date: "9/27/2022"


# LOAD PACKAGES
library(dplyr)
library(ggplot2)
library(kableExtra)
library(data.table)
library(stringr)
library(latex2exp)
library(Rmisc)
library(aggregate)



# SET WORKING DIRECTORY ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis") # personal computer
# setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer

# LOAD DATA  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

algaefeed  <- data.frame(read.csv(file="Data/Algae_FlowCytometry/AlgaeFeeding_master.csv", header=T)) %>% 
  dplyr::mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>% 
  dplyr::filter(!High_Chl_cell_mL > 9000) # omit the few outliers due to tank sampling error 

# DATA TABLES   :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Averages by date and food and OA treatment (high v. low chl a cells)  :::::::::::::::::::::::::::::::::::::::::::::::

# High Chlorophyll cells by pH & food treatment
High_chla_MeanSE_date <-  algaefeed %>% 
  summarySE(measurevar="High_Chl_cell_mL", groupvars=c("Date", "pH_treatment","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'High_Chl_cell_mL') %>% 
  dplyr::rename(value = High_Chl_cell_mL)

# Low Chlorophyll cells by pH & food treatment
Low_chla_MeanSE_date <-  algaefeed %>% 
  summarySE(measurevar="Low_Chl_cell_mL", groupvars=c("Date","pH_treatment","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'Low_Chl_cell_mL') %>% 
  dplyr::rename(value = Low_Chl_cell_mL)


Means_Master_plotting <- rbind(High_chla_MeanSE_date, Low_chla_MeanSE_date) %>% 
                          dplyr::filter(!(Cell_type == 'High_Chl_cell_mL' & Fed_unfed == 'unfed' & value > 2000))



# Averages by food and OA treatment (high v. low chl a cells)  :::::::::::::::::::::::::::::::::::::::::::::::


# High Chlorophyll cells by pH & food treatment
High_chla_MeanSE <-  algaefeed %>% 
  summarySE(measurevar="High_Chl_cell_mL", groupvars=c("pH_treatment","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'High_Chl_cell_mL') %>% 
  dplyr::rename(value = High_Chl_cell_mL)

# Loe Chlorophyll cells by pH & food treatment
Low_chla_MeanSE <-  algaefeed %>% 
  summarySE(measurevar="Low_Chl_cell_mL", groupvars=c("pH_treatment","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'Low_Chl_cell_mL') %>% 
  dplyr::rename(value = Low_Chl_cell_mL)



# Averages  by food treatment ONLY (high v. low chl a cells)  :::::::::::::::::::::::::::::::::::::::::::::::


# High Chlorophyll cells by food treatment
High_chla_MeanSE_foodONLY <-  algaefeed %>% 
  summarySE(measurevar="High_Chl_cell_mL", groupvars=("Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'High_Chl_cell_mL') %>% 
  dplyr::rename(value = High_Chl_cell_mL)

(High_chla_MeanSE_foodONLY[1, 3]) / (High_chla_MeanSE_foodONLY[2, 3]) # 3.341997 x MORE food under the high treatment
100-(( (High_chla_MeanSE_foodONLY[2, 3]) / (High_chla_MeanSE_foodONLY[1, 3]) )*100) # 70.07777 % MORE high chl a cells unde rht high treatment 

# Loe Chlorophyll cells by food treatment
Low_chla_MeanSE_foodONLY <-  algaefeed %>% 
  summarySE(measurevar="Low_Chl_cell_mL", groupvars=("Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'Low_Chl_cell_mL') %>% 
  dplyr::rename(value = Low_Chl_cell_mL)


# PLOTTING   :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

factor(Means_Master_plotting$Fed_unfed)
pd <- position_dodge(0.1) # adjust the jitter for the different treatments   

Masterplot_pH8 <- Means_Master_plotting %>% 
  dplyr::filter(Cell_type %in% "High_Chl_cell_mL") %>% 
  ggplot(aes(x=Date, y=value, group=Fed_unfed, colour=factor(Fed_unfed))) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  scale_colour_manual(values = c("fed " = "orange",
                                "unfed"="grey")) +
  theme_classic() +
  geom_point(position=pd, size=3) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)) +
  facet_wrap(~pH_treatment, scales = "free")
Masterplot_pH8

Masterplot_pH7.5 <- Means_Master_plotting %>% 
  dplyr::filter(Cell_type %in% "Low_Chl_cell_mL") %>% 
  ggplot(aes(x=Date, y=value, group=Fed_unfed, colour=factor(Fed_unfed))) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  scale_colour_manual(values = c("fed " = "orange",
                                 "unfed"="grey")) +
  theme_classic() +
  geom_point(position=pd, size=3) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300000)) +
  facet_wrap(~pH_treatment, scales = "free")
Masterplot_pH7.5


library(ggpubr)

pdf("C:/Users/samjg/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis/Output/AlgaeFeed_supplement.pdf", width=12, height=8)
print(ggarrange(Masterplot_pH8, Masterplot_pH7.5,ncol = 1,nrow = 2))
graphics.off()


pdf("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Water_Chemistry/CarbChem_MeanSE_Timeseries.pdf", width=12, height=8)


# FOOD  OA CHALLENGE ONLY  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# LOAD DATA 
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
ChemTable_MeanSE$Salinity <- paste(signif(chemTable$Salinity_mean,digits=3), signif(chemTable$Salinity_se,digits=3), sep="? ")
ChemTable_MeanSE$Temperature <- paste(signif(chemTable$t.oC..of.bucket_mean,digits=3), signif(chemTable$t.oC..of.bucket_se,digits=3), sep=" ? ")
ChemTable_MeanSE$DO <- paste(signif(chemTable$DO.mg.L_mean,digits=3), signif(chemTable$DO.mg.L_se,digits=3), sep=" ? ")
ChemTable_MeanSE$pH <- paste(signif(chemTable$pH.out_mean,digits=3), signif(chemTable$pH.out_se,digits=3), sep=" ? ")
ChemTable_MeanSE$pCO2 <- paste(signif(chemTable$pCO2.out..matm._mean,digits=3), signif(chemTable$pCO2.out..matm._se,digits=3), sep=" ? ")
# ChemTable_MeanSE$DIC <- paste(chemTable$mean.DIC, chemTable$sem.DIC, sep=" ± ")
ChemTable_MeanSE$TA <- paste(signif(chemTable$TA..mmol.kgSW._mean,digits=3), signif(chemTable$TA..mmol.kgSW._se,digits=3), sep=" ? ")
ChemTable_MeanSE$Aragonite.Sat <- paste(signif(chemTable$WAr.out_mean,digits=3), signif(chemTable$WAr.out_se,digits=3), sep=" ? ")
ChemTable_MeanSE$Calcite.Sat <- paste(signif(chemTable$WCa.out_mean,digits=3), signif(chemTable$WCa.out_se,digits=3), sep=" ? ")
ChemTable_MeanSE <- ChemTable_MeanSE[,-1] # view table
View(ChemTable_MeanSE)


#write.csv(ChemTable_MeanSE, "C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Water_Chemistry/SummaryTable_meanSE.csv")
write.csv(ChemTable_MeanSE, "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Water_Chemistry/SummaryTable_meanSE.csv")
