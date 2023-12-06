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
library(car)


# SET WORKING DIRECTORY ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis") # personal computer
# setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis") # Work computer
# setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer

# LOAD DATA  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

algaefeed  <- data.frame(read.csv(file="Data/Algae_FlowCytometry/cumulative_raw/AlgaeFeeding_master.csv", header=T)) %>% 
  dplyr::mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>% 
  dplyr::filter(!High_Chl_cell_mL > 9000) # omit the few outliers due to tank sampling error 

# DATA TABLES & STATS   :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# averaged by rep to run stats - note theres an N of 17-19 for reps, these are sampling dates (time points!)
# High Chlorophyll cells by pH & food treatment
High_chla_MeanSE_rep <-  algaefeed %>% 
  summarySE(measurevar="High_Chl_cell_mL", groupvars=c("Replicate", "pH_treatment","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'High_Chl_cell_mL') %>% 
  dplyr::rename(value = High_Chl_cell_mL)

# Low Chlorophyll cells by pH & food treatment
Low_chla_MeanSE_rep <-  algaefeed %>% 
  summarySE(measurevar="Low_Chl_cell_mL", groupvars=c("Replicate", "pH_treatment","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'Low_Chl_cell_mL') %>% 
  dplyr::rename(value = Low_Chl_cell_mL)

# merge to a amster file and ommit occurance of error
Means_Master_Rep <- rbind(High_chla_MeanSE_rep, Low_chla_MeanSE_rep)

# isolate the high chl data (majority the Chaet-B and Tet supplemented algae)
High_chla_Means_rep <- Means_Master_Rep %>% 
                          dplyr::filter(Cell_type %in% 'High_Chl_cell_mL') %>% 
                          dplyr::mutate(pH_treatment = as.factor(pH_treatment)) # %>%  # make num var as factor for AOV 
                          #summarySE(measurevar="value", # a mean of means - initally by rep, now by treatment, N = 4 for reps
                          #          groupvars=c("pH_treatment","Fed_unfed")) 
# pH_treatment Fed_unfed N     value        sd        se        ci
# 1          7.5      fed  4 1474.3768 271.34019 135.67009 431.76279
# 2          7.5     unfed 4  396.3165  20.97520  10.48760  33.37622
# 3            8      fed  4 1363.2153 259.14356 129.57178 412.35524
# 4            8     unfed 4  451.1789  85.96238  42.98119 136.78533

# isolate the low chl data (majority seston, the majority of  algae present for the low food treatment)
Low_chla_Means_rep <- Means_Master_Rep %>% 
                          dplyr::filter(Cell_type %in% 'Low_Chl_cell_mL') %>% 
                          dplyr::mutate(pH_treatment = as.factor(pH_treatment)) # %>%  # make num var as factor for AOV 
                          #summarySE(measurevar="value", # a mean of means - initally by rep, now by treatment, N = 4 for reps
                          #          groupvars=c("pH_treatment","Fed_unfed")) 
# pH_treatment Fed_unfed N    value        sd       se        ci
# 1          7.5      fed  4 99685.98 10556.581 5278.290 16797.876
# 2          7.5     unfed 4 73389.63  9852.721 4926.360 15677.878
# 3            8      fed  4 74773.99  4602.060 2301.030  7322.904
# 4            8     unfed 4 78431.82 14330.385 7165.192 22802.840

# run stats - 
# Question, did the supplemented treatment differ from the unfed
# regarding the presence of high chl a and low chla particles? - NOTE our N per foodx OA treatment is 4! 

# high chl particles
high_chla_mod <- lm(value ~ pH_treatment*Fed_unfed, data= High_chla_Means_rep)
shapiro.test(resid(high_chla_mod)) # 0.7276
leveneTest(high_chla_mod) # 0.005261 ** - need to run non-parametric
high_chla_SRH <- scheirerRayHare(value ~ pH_treatment*Fed_unfed, data= High_chla_Means_rep)
pander(print(high_chla_SRH), style='rmarkdown') # table for SRH test
# |           &nbsp;           | Df | Sum Sq |    H    |  p.value  |
# |:--------------------------:|:--:|:------:|:-------:|:---------:|
# |      **pH_treatment**      | 1  |  0.25  | 0.01103 |  0.9164   |
# |       **Fed_unfed**        | 1  |  256   |  11.29  | 0.0007775 |
# | **pH_treatment:Fed_unfed** | 1  |  6.25  | 0.2757  |  0.5995   |
# |       **Residuals**        | 12 |  77.5  |   NA    |    NA     |
High_chla_Means_rep  %>% summarySE(measurevar="value",groupvars=("Fed_unfed")) 
# Fed_unfed N     value        sd       se        ci
#      fed  8 1418.7961 252.71566 89.34848 211.27558
#     unfed 8  423.7477  64.92659 22.95502  54.27999


# low chl particles
low_chla_mod <- lm(value ~ pH_treatment*Fed_unfed, data= Low_chla_Means_rep)
shapiro.test(resid(low_chla_mod)) # 0.05165
leveneTest(low_chla_mod) # 0.7038 
pander(print(anova(low_chla_mod)), style='rmarkdown') 
# |           &nbsp;           | Df |  Sum Sq   |  Mean Sq  | F value | Pr(>F)  |
# |:--------------------------:|:--:|:---------:|:---------:|:-------:|:-------:|
# |      **pH_treatment**      | 1  | 394808992 | 394808992 |  3.63   | 0.08099 |
# |       **Fed_unfed**        | 1  | 512502491 | 512502491 |  4.712  | 0.05073 |
# | **pH_treatment:Fed_unfed** | 1  | 897252622 | 897252622 |  8.25   | 0.01403 |
# |       **Residuals**        | 12 | 1.305e+09 | 108764099 |   NA    |   NA    |
Low_chla_Means_rep  %>% summarySE(measurevar="value",groupvars=("Fed_unfed")) 
# Fed_unfed N    value       sd       se        ci
# 1      fed  8 87229.98 15302.08 5410.102 12792.858
# 2     unfed 8 75910.73 11699.54 4136.414  9781.064










# Averages by date and food and OA treatment (high v. low chl a cells)  :::::::::::::::::::::::::::::::::::::::::::::::



# Means_Master_plotting <- rbind(High_chla_MeanSE_date, Low_chla_MeanSE_date) %>% 
#                           dplyr::filter(!(Cell_type == 'High_Chl_cell_mL' & Fed_unfed == 'unfed' & value > 2000))
# 

# ANOVA to test the differences in feeding treatments 

# (1) First, run anova within date for all records (for looped!)
ANOVA_Dates <- as.data.table(unique(algaefeed$Date)) # call a list to loop in 
AOVdf_total          <- data.frame() # start dataframe, this will be the master output
DF_MODHIGH           <- data.frame(matrix(nrow = 1, ncol = 12)) # create dataframe to save during for loop
colnames(DF_MODHIGH) <- c('Date', 'Cell_type', 'model', 'DF.num' , 'DF.denom', 'F_val','P_val', 'SigDif', 'ShapiroWilk', 'ResidNorm', 'Levenes', 'HomogVar') # names for comuns in the for loop
DF_MODLOW            <- data.frame(matrix(nrow = 1, ncol = 12)) # create dataframe to save  during for loop
colnames(DF_MODLOW)  <- c('Date', 'Cell_type', 'model', 'DF.num' , 'DF.denom', 'F_val','P_val', 'SigDif','ShapiroWilk', 'ResidNorm', 'Levenes', 'HomogVar') # names for comuns in the for loop

for (i in 1:nrow(ANOVA_Dates)) {
  
    LOOPDATE <- ANOVA_Dates$V1[i] 
    # high cholorphyll model
    datMODHIGH    <- aov(lm(High_Chl_cell_mL ~Fed_unfed, data = (algaefeed %>% dplyr::filter(Date == LOOPDATE))))
      DF_MODHIGH$Date        <- LOOPDATE
      DF_MODHIGH$Cell_type   <- 'HIGH_chl_cells'
      DF_MODHIGH$model       <- 'one-way AOV; High_Chl_cell_mL ~ Fed_unfed'
      DF_MODHIGH$DF.num      <- summary(datMODHIGH)[[1]][["Df"]][1]
      DF_MODHIGH$DF.denom    <- summary(datMODHIGH)[[1]][["Df"]][2]
      DF_MODHIGH$F_val       <- summary(datMODHIGH)[[1]][["F value"]][1]
      DF_MODHIGH$P_val       <- summary(datMODHIGH)[[1]][["Pr(>F)"]][1]
      DF_MODHIGH$SigDif      <- if( (summary(datMODHIGH)[[1]][["Pr(>F)"]][1]) > 0.05) {
                                'NO'} else {'YES'}
      DF_MODHIGH$ShapiroWilk <- shapiro.test(resid(datMODHIGH))[[2]]
      DF_MODHIGH$ResidNorm   <- if( shapiro.test(resid(datMODHIGH))[[2]] > 0.05) {
        'YES'} else {'NO'}
      DF_MODHIGH$Levenes     <- leveneTest(datMODHIGH)[[3]][[1]]
      DF_MODHIGH$HomogVar    <- if( leveneTest(datMODHIGH)[[3]][[1]] > 0.05) {
        'YES'} else {'NO'}
      
    #low cholorphyll model
    datMODLOW    <- aov(lm(Low_Chl_cell_mL ~Fed_unfed, data = (algaefeed %>% dplyr::filter(Date == LOOPDATE))))
      DF_MODLOW$Date        <- LOOPDATE
      DF_MODLOW$Cell_type   <- 'LOW_chl_cells'
      DF_MODLOW$model       <- 'one-way AOV; LOW_Chl_cell_mL ~ Fed_unfed'
      DF_MODLOW$DF.num      <- summary(datMODLOW)[[1]][["Df"]][1]
      DF_MODLOW$DF.denom    <- summary(datMODLOW)[[1]][["Df"]][2]
      DF_MODLOW$F_val       <- summary(datMODLOW)[[1]][["F value"]][1]
      DF_MODLOW$P_val       <- summary(datMODLOW)[[1]][["Pr(>F)"]][1]
      DF_MODLOW$SigDif      <- if( (summary(datMODLOW)[[1]][["Pr(>F)"]][1]) > 0.05) {
                                  'NO'} else {'YES'}  
      DF_MODLOW$ShapiroWilk <- shapiro.test(resid(datMODLOW))[[2]]
      DF_MODLOW$ResidNorm  <- if( shapiro.test(resid(datMODLOW))[[2]] > 0.05) {
        'NO'} else {'YES'}
      DF_MODLOW$Levenes     <- leveneTest(datMODLOW)[[3]][[1]]
      DF_MODLOW$HomogVar    <- if( leveneTest(datMODLOW)[[3]][[1]] > 0.05) {
        'YES'} else {'NO'}

      # asign loop and cumulative output table
      df       <- data.frame(rbind(DF_MODHIGH,DF_MODLOW)) # name dataframe for this single row
      AOVdf_total <- rbind(AOVdf_total,df) #bind to a cumulative list dataframe
      print(AOVdf_total) # print to monitor progress
      
}
View(AOVdf_total) # view all the anova tests within data 

# (2) Run anova tests for ALL data, averged for each date 
# High Chlorophyll cells by pH & food treatment
High_chla_MeanSE <-  algaefeed %>% 
  summarySE(measurevar="High_Chl_cell_mL", groupvars=c("Date","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'High_Chl_cell_mL') %>% 
  dplyr::rename(value = High_Chl_cell_mL)
summary(aov(lm(value ~Fed_unfed, data = High_chla_MeanSE)))
#              Df     Sum Sq      Mean Sq   F value  Pr(>F)    
# Fed_unfed     1     8887257     8887257   30.11    4e-06 ***
# Residuals     34    10036568    295193 
shapiro.test(resid(aov(lm(value ~Fed_unfed, data = High_chla_MeanSE)))) # p-value = 0.07265 - normal! 
qqnorm(resid(aov(lm(value ~Fed_unfed, data = High_chla_MeanSE)))) # qq nrom plot of model residuals
leveneTest(aov(lm(value ~Fed_unfed, data = High_chla_MeanSE))) # 0.004876 ** - variance is unequal
hist(resid(aov(lm(value ~Fed_unfed, data = High_chla_MeanSE))))


# Low Chlorophyll cells by pH & food treatment
Low_chla_MeanSE <-  algaefeed %>% 
  summarySE(measurevar="Low_Chl_cell_mL", groupvars=c("Date","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'Low_Chl_cell_mL') %>% 
  dplyr::rename(value = Low_Chl_cell_mL)
summary(aov(lm(value ~Fed_unfed, data = Low_chla_MeanSE)))
#              Df     Sum Sq      Mean Sq     F value  Pr(>F)    
# Fed_unfed     1     1.051e+09   1.051e+09   0.441    0.511
# Residuals     34    8.105e+10   2.384e+09    
shapiro.test(resid(aov(lm(value ~Fed_unfed, data = Low_chla_MeanSE)))) # p-value = 0.02404 - not normal! 
qqnorm(resid(aov(lm(value ~Fed_unfed, data = Low_chla_MeanSE)))) # qq nrom plot of model residuals
leveneTest(aov(lm(value ~Fed_unfed, data = Low_chla_MeanSE))) # 0.1996 ** - variance is unequal
hist(resid(aov(lm(value ~Fed_unfed, data = Low_chla_MeanSE))))

#NOTE: in each case above, the log model 'log(value)' resolves either normality (low chl model) 
# or homogeneity of variance (the high chl model) BUT DOES NOT CHANGE THE MODEL OUTCOME 
# outcome = high chl a cells significantly different between treaments 
# outcome = low ch l cells NOT significnatly different between treatments


# Averages by food and OA treatment (high v. low chl a cells)  :::::::::::::::::::::::::::::::::::::::::::::::


# High Chlorophyll cells by pH & food treatment
High_chla_MeanSE <-  algaefeed %>% 
  summarySE(measurevar="High_Chl_cell_mL", groupvars=c("pH_treatment","Fed_unfed")) %>% 
  dplyr::mutate(Cell_type = 'High_Chl_cell_mL') %>% 
  dplyr::rename(value = High_Chl_cell_mL)

# Low Chlorophyll cells by pH & food treatment
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
