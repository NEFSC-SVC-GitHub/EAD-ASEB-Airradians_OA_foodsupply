# Purpose: Bay Scallop Project - Respiration rate data 
# analysis of respiration rate data

# Written by: Sam J Gurr (last edit 9/15/2021)

# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(dplyr)
library(ggplot2)
library(forcats)
library(lmer4)
library(lmerTest)
library(car)
# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # personal computer 
# setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer

# LOAD DATA :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
resp.data    <- read.csv(file="Output/Respiration/Cumulative_resp_alpha0.4_15sectrunc40min.csv", header=T) %>% dplyr::filter(!Filename %in% 'Run_1_raw.txt') # read the calculate raw rates from 'resp_LoLin' script - contains the calculated rate (not normalized for blanks) for each sensor-channel

exp_metadata <- read.csv(file="Data/ExperimentMetadata.csv", header=T) # treatment assignments to 'Chamber_Tank'
lengths      <- read.csv(file="Data/Respiration/Lengths_resp.csv", header=T)
resp.ref     <- read.csv(file="Data/Respiration/Reference_master.csv", header=T) %>% 
  dplyr::filter(!Filename %in% 'Run_1_raw.txt') %>% # reference for the respirometry data - contains the 'Chamber_Tank' for each sensor channel (whether an animal or a blank)
  dplyr::mutate(Run = gsub("[- .)_(+]|[a-zA-Z]*:?","", Filename))

# merge the exp_metadata with the resp.data
resp.ref_merged_1                 <- merge(exp_metadata, resp.ref, by = 'Chamber_tank', all=TRUE) # all TRUE allows us to keep the blanks
resp.ref_merged_2                 <- merge(resp.ref_merged_1, lengths, by = c('Run', 'Chamber_tank'), all=TRUE) # all TRUE allows us to keep the blanks
resp.data_merged                  <- merge(resp.data, resp.ref_merged_2, by = c('Date', 'Channel','Filename')) # out master file moving forward....

# CALCULATE RESPIRATION RATES :::::::::::::::::::::::::::::::::::::::::::::::

# ---------------------- (1) --------------------------------#
# get a summary table of blanks to normalize respiration rates

dates.runs <- resp.data_merged %>%  # call table
  dplyr::distinct(Date, Filename) # call all unique values for date run and sw condition
dates.runs <- na.omit(dates.runs)
# call dataframe and build table to rbind in for loop
blanks_total <- data.frame() # start dataframe 
blanks.table <- data.frame(matrix(nrow = 1,ncol = 5)) # make a table template
colnames(blanks.table)<-c('Date', 'Channel', 'mean_Lpc', 'mean_Leq' , 'mean_Lz') # names for comuns in the for loop


data <- resp.data_merged %>% 
    dplyr::select(Date, Chamber_tank, Filename, Channel, Lpc,  Leq, Lz) %>% 
    dplyr::filter(!is.na(Lpc)) # ommits empty resp channels (if any)

  
blanks <- data.frame(data %>%
    dplyr::filter(! Filename %in% 'Run_1_raw.txt') %>% # run 1 on 9/14 was restarted - I assume to ommit the previous from analysis
    dplyr::group_by(Channel, Date) %>% 
    dplyr::filter(Chamber_tank == "blank") %>% 
    dplyr::summarise(BLANK.mean_Lpc = mean(abs(Lpc)),
                     BLANK.mean_Leq = mean(abs(Leq)), 
                     BLANK.mean_Lz = mean(abs(Lz))) %>% 
    dplyr::mutate(pCO2 = case_when(
      Channel == "CH8" ~ "H",
      Channel == "CH4"  ~ "L")))
blanks <- blanks %>% dplyr::select(!Channel) # ommit channel to reduce confusion when merged - treatment will be merged with the resp.data.merged' dataframe

# ---------------------- (2) --------------------------------#
# merge blanks with the summary table and calculate the normalized rates 

Resp.Master <- merge(resp.data_merged, blanks, by=c("Date", "pCO2")) %>% # NOTE: this repeats for every distinct length value
  dplyr::mutate(resp_norm = Lpc - BLANK.mean_Lpc) # ommits respiration rate values showing an increase in O2 over time 

Resp.Master_OM <- Resp.Master %>% dplyr::filter(!resp_norm > 0) # ommit respiration values that are positive

# NOTE: look at the following table to troubleshoot if needed
Resp.outliers <- Resp.Master %>% dplyr::filter(resp_norm > 0) # call the values with positive resp rates, measing they were slower than the blank!

# calculate resp rates
vial.vol <- 0.8 # milliliters (ml) - to nomalize by volume (L)
Resp.Master_OM$resp_ug_L_hr <- ( (abs(Resp.Master_OM$resp_norm)*1000)* # convert mg/L to ug/L
                                   (vial.vol/1000)* # normalize by volume
                                   (60)) # convert per hour from per minute

Resp.Master_OM$resp_ng_L_umlLength_hr <- ( 
  ( ( (abs(Resp.Master_OM$resp_norm)*1000000) * # call absolute value of resp in mg per minute - convert to ng min-1
        (vial.vol/1000) ) / # correct ng minute-1 to ng liter-1 by multiplying by the resp vial in liters
      Resp.Master_OM$Length.um.) * # normalize by individual or larvae count - as to ng L-1 individual-1
    (60)) # correct for the time; final value is ng Liter-1 individual-1 hour-1


# ANALYSIS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# model effect of treatment on resp rate 20210507
Resp_0914 <- Resp.Master_OM %>% 
              dplyr::filter(Date %in% '9/14/2021')  # call only 9/14


Resp_0914 %>% dplyr::group_by(Chamber_tank) %>% summarise(n()) # tank replication

LMmod_0914 <- aov(lm(resp_ng_L_umlLength_hr~pCO2,data=Resp_0914))
summary(LMmod_0914)
check_model(LMmod_0914) # observe the diagnostics of the model
shapiro.test(residuals(LMmod_0914)) # non normal
leveneTest(LMmod_0914) # good

MEmod_0914 <- lmer(resp_ng_L_umlLength_hr~pCO2 + (1|Chamber_tank),REML=TRUE, data=Resp_0914)
summary(MEmod_0914) # sig intercept just means the grand mean is different from 0 - not meaningful here.. 
check_model(MEmod_0914)
shapiro.test(residuals(MEmod_0914)) # non normal
leveneTest(MEmod_0914) # good


DF   <- paste( (summary(LMmod_0914)[[1]][["Df"]])[1], (summary(LMmod_0914)[[1]][["Df"]])[2], sep = '/')
Fval <- (summary(LMmod_0914)[[1]][["F value"]])[1]
pval <- (summary(LMmod_0914)[[1]][["Pr(>F)"]])[1]

pdf(paste0("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis/Output/Respiration/20210914_respiration.pdf"))
ggplot(Resp_0914, aes(pCO2 , resp_ng_L_umlLength_hr , fill = pCO2)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pCO2)) +
  scale_fill_manual(values=c("grey50","white")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  scale_x_discrete(labels= c('Elevated (H)', 'Ambient (L)')) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title = "F1 Scallops: pediveliger respiration rates on 20210914", 
       y = expression(Respiration~rate~"("~ng~L^{-1}~O[2]%.%mu*m^{-1}%.% hr^{-1}~")"), 
       x = expression(italic(p)*CO[2]~Treatment~"("~mu*atm~")")) + 
  annotate("text", x=1.5, y=0.2, size = 4, label = "aov(Resp~Treatment + (1|Tank))") +
  annotate("text", x=1.5, y=0.16, size = 4, label= paste('DF =',DF,'F =', signif(Fval, digits=3), 'p value =', signif(pval, digits=3), sep=" ")) 
dev.off()
