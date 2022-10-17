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
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis") # personal computer
# setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis") # Work computer
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis") # Work computer

# ALL CHEMISTRY DATA  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# LOAD DATA 

chem    <- data.frame(read.delim(file="Data/Seawater_chemistry/Water_Chemistry_Scallops_2021.csv", header=T)) %>%  
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



# PLOTTING ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# mean plot for all data within Date
wideCHEM.MEANS <- as.data.table(chem[,c(1, 3,4,6,11,13:14,16:20)] %>% 
                                 group_by(Date,pH,Food.Treatment) %>%
                                 summarise_each(funs(mean)))
longHCHEM.MEANS <- melt(setDT(wideCHEM.MEANS %>% dplyr::select(-Date)), 
                        id.vars = c("pH", "Food.Treatment"), 
                        variable.name = "measurement") %>% 
                        dplyr::rename(MEAN = value)
CHEM_DOTPLOT <- longHCHEM.MEANS %>% 
                    dplyr::mutate(pH_food= paste(pH, Food.Treatment, sep = "_")) %>% 
                    ggplot(aes(x = Food.Treatment, y = MEAN, shape = as.factor(pH))) +
                    geom_point()+
                    theme_classic() +
                    facet_wrap(~measurement, scales = "free")
CHEM_DOTPLOT


# mean ST error plot
# furtehr average the data across dates and by the four treatments 
longHCHEM.MEANS.SUMMARY <- as.data.table(longHCHEM.MEANS %>% 
                                           group_by(pH,Food.Treatment,measurement) %>%
                                           summarise_each(funs(mean,se=sd(.)/sqrt(n()))))

CHEM_MEANSEPLOT <- longHCHEM.MEANS.SUMMARY %>% 
  dplyr::mutate(pH_food= paste(pH, Food.Treatment, sep = "_")) %>% 
  ggplot(aes(x = Food.Treatment, y = mean, shape = as.factor(pH))) +
  geom_point() +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2,
                position=position_dodge(0.05)) +
  theme_classic()+
  facet_wrap(~measurement, scales = "free")
CHEM_MEANSEPLOT


# save plot 
pdf(paste0(filename = "C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis/Output/Water_Chemistry/MeanStError_plot.pdf"), width = 10, height = 6)
print(CHEM_MEANSEPLOT)
dev.off()


# STATISTICAL TESTING ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# ANOVA tests of chemistry data 
chemTable <- as.data.table(chem[,c(1, 3,4,6,11,13:14,16:20)] %>% 
                             group_by(Date,pH,Food.Treatment) %>%
                             summarise_each(funs(mean,sd,se=sd(.)/sqrt(n()))) %>% 
                             mutate(pH_food= paste(pH, Food.Treatment, sep = "_")) %>% 
                             na.omit())
# pCO2
modpCO2 <- (lm(pCO2.out..matm._mean ~as.factor(pH)*Food.Treatment, data = chemTable))
# pH and food significant, no interaction 
modpCO2 <- (lm(pCO2.out..matm._mean ~pH_food, data = chemTable))
summary(aov(modpCO2)) # 0.000248 sig effect
shapiro.test(resid(aov(modpCO2))) # p-value = 0.07265 - normal! 
emmeans(modpCO2, ~ pH_food)
TukeyHSD(aov(modpCO2))
# $pH_food
#                   diff       lwr        upr      p adj
# 7.5_Low-7.5_High -136.11500 -311.5340   39.30396 0.1563058
# 8_High-7.5_High  -275.56000 -440.9466 -110.17342 0.0013171 ** pCO2 8 < 7.5 wihtin high food
# 8_Low-7.5_High   -333.32125 -508.7402 -157.90229 0.0003875 ** pCO2 8 < 7.5 across food
# 8_High-7.5_Low   -139.44500 -314.8640   35.97396 0.1426774
# 8_Low-7.5_Low    -197.20625 -382.1141  -12.29843 0.0349779 ** # pCO2 8 < 7.5 within low food
# 8_Low-8_High      -57.76125 -233.1802  117.65771 0.7752134


# Aragonite sat ------------------------------------------------------ #
modARAG <- (lm(WAr.out_mean ~as.factor(pH)*Food.Treatment, data = chemTable))
# pH is significant but NOT food 
modARAG <- (lm(WAr.out_mean ~pH_food, data = chemTable))
summary(aov(modARAG)) # 0.000248 sig effect
shapiro.test(resid(aov(modARAG))) # p-value = 0.003882 - non-normal! 
emmeans(modARAG, ~ pH_food)
TukeyHSD(aov(modARAG))
# $pH_food
# diff         lwr       upr     p adj
# 7.5_Low-7.5_High 0.065300 -0.03004637 0.1606464 0.2375499
# 8_High-7.5_High  0.146850  0.05695658 0.2367434 0.0015677 **
# 8_Low-7.5_High   0.183175  0.08782863 0.2785214 0.0003481 **
# 8_High-7.5_Low   0.081550 -0.01379637 0.1768964 0.1059281
# 8_Low-7.5_Low    0.117875  0.01737110 0.2183789 0.0195736 **
# 8_Low-8_High     0.036325 -0.05902137 0.1316714 0.6910534

# pH ------------------------------------------------------ #
modpH <- (lm(pH.out_mean ~as.factor(pH)*Food.Treatment, data = chemTable))
# pH and food significant, no interaction 
modpH <- (lm(pH.out_mean ~pH_food, data = chemTable))
summary(aov(modpH)) # 0.000248 sig effect
shapiro.test(resid(aov(lm(pH.out_mean ~pH_food, data = chemTable)))) # p-value = 0.5409 - normal! 
emmeans(modpH, ~ pH_food)
TukeyHSD(aov(modpH))
# $pH_food
# diff         lwr       upr     p adj
# 7.5_Low-7.5_High 0.065300 -0.03004637 0.1606464 0.2375499
# 8_High-7.5_High  0.146850  0.05695658 0.2367434 0.0015677 **
# 8_Low-7.5_High   0.183175  0.08782863 0.2785214 0.0003481 **
# 8_High-7.5_Low   0.081550 -0.01379637 0.1768964 0.1059281
# 8_Low-7.5_Low    0.117875  0.01737110 0.2183789 0.0195736 **
# 8_Low-8_High     0.036325 -0.05902137 0.1316714 0.6910534

















# Summary table chemistry ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
