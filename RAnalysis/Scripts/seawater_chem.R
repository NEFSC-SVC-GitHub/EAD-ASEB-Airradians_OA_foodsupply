# title: "seawater_chem"
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



# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # personal computer
# setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer

# ALL CHEMISTRY DATA  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# LOAD DATA 

chem    <- data.frame(read.csv(file="Data/Seawater_chemistry/Water_Chemistry_Scallops_2021.csv", header=T)) %>%  
  dplyr::filter(!X  %in% c('Checking the system', 'RESPO', 'Tank Farm', 'Blue bucket check')) %>%  # ommit all occurances of 'checks' of the system
  dplyr::rename(Date = ?..Date)
chem$Date <- as.factor(gsub("/2021.*","", chem$Date))


# Total Alkalinity (Mean St Error)
TA_MeanSE <-  chem[!is.na(chem$TA_.mmol.kgSW.),] %>% 
  summarySE(measurevar="TA_.mmol.kgSW.", groupvars=c("Date","OA_Treatment")) %>% 
  dplyr::mutate(Measurement = 'TA_mmol.kgSW') %>% 
  dplyr::rename(value = TA_.mmol.kgSW.)
TA_MeanSE <- TA_MeanSE[-(68),] # remove a single row - TA from 9/9/ 'High' has a HUGE standard error value!
# Total CO2 (Mean St Error)
TACO2_MeanSE <-  chem[!is.na(chem$TCO2_.mmol.kgSW.),] %>% 
  summarySE(measurevar="TCO2_.mmol.kgSW.", groupvars=c("Date","OA_Treatment")) %>% 
  dplyr::mutate(Measurement = 'TCO2_mmol.kgSW') %>% 
  dplyr::rename(value = TCO2_.mmol.kgSW.)
#  pH Chosen scale (Mean St Error)
pChosenScale_MeanSE <-  chem[!is.na(chem$pH_Chosen_scale_.4_decimals.),] %>% 
  summarySE(measurevar="pH_Chosen_scale_.4_decimals.", groupvars=c("Date","OA_Treatment")) %>% 
  dplyr::mutate(Measurement = 'pH_Chosen_scale') %>% 
  dplyr::rename(value = pH_Chosen_scale_.4_decimals.)
# pH out  (Mean St Error) - calculated at the measured temperature 
pHout_MeanSE <-  chem[!is.na(chem$pH_out_MeasTemp),] %>% 
  summarySE(measurevar="pH_out_MeasTemp", groupvars=c("Date","OA_Treatment")) %>% 
  dplyr::mutate(Measurement = 'pH out') %>% 
  dplyr::rename(value = pH_out_MeasTemp) 
# pCO2  (Mean St Error) - calculated at the measured temperature 
pCO2out_MeanSE <-  chem[!is.na(chem$pCO2_out_.matm._MeasTemp),] %>% 
  summarySE(measurevar="pCO2_out_.matm._MeasTemp", groupvars=c("Date","OA_Treatment")) %>% 
  dplyr::mutate(Measurement = 'pCO2 out') %>% 
  dplyr::rename(value = pCO2_out_.matm._MeasTemp)
# Temperature  (Mean St Error) 
temperature_MeanSE <-  chem[!is.na(chem$t.oC._spec),] %>% 
  summarySE(measurevar="t.oC._spec", groupvars=c("Date","OA_Treatment")) %>% 
  dplyr::mutate(Measurement = 'Temperature (C)') %>% 
  dplyr::rename(value = t.oC._spec)

master <- rbind(TA_MeanSE, TACO2_MeanSE, pChosenScale_MeanSE, pHout_MeanSE, pCO2out_MeanSE, temperature_MeanSE)

master$Date <- as.Date(master$Date,'%m/%d') # reformat the date here

pd <- position_dodge(0.1) # adjust the jitter for the different treatments   

Masterplot <- ggplot(master, aes(x=Date, y=value, colour=OA_Treatment, group=OA_Treatment)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), colour="black", width=.1, position=pd) +
  geom_line(position=pd) +
  scale_color_manual(values = c("Ambient" = "steelblue",
                                "Mid"="orange",
                                "High"="red")) +
  theme_classic() +
  geom_point(position=pd, size=3) +
  facet_wrap(~Measurement, scales = "free")
Masterplot
pdf("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Water_Chemistry/CarbChem_MeanSE_Timeseries.pdf", width=12, height=8)
print(Masterplot)
graphics.off()

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
