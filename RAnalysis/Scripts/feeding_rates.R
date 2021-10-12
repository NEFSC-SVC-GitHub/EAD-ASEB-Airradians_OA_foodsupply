# Purpose: Bay Scallop Project - Feeding Rates
# analysis of respiration rate data

# Written by: Sam J Gurr (last edit 9/15/2021)

# Review Riisgard 2001 defining the clearance rate of bivalves 
# NOTE: clearance rate is defined as the volume of water cleared of suspended particles per unit time, and only equals filtration rate when 
# 100% of suspended particles are efficiently retained

# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(dplyr)
library(ggplot2)
library(forcats)
library(lme4)
library(lmerTest)
library(see)
library(performance)
library(car)
library(reshape2)
library(lubridate)
library(SciViews)
library(reshape2)
library(SciViews)

# SET WORKING DIRECTORY :::::::::::::::::::::::::::::::::::::::::::::::

#setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # personal computer 
setwd("C:/Users/samuel.gurr/Documents/Github_repositories/Airradians_OA/RAnalysis") # Work computer
setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA/RAnalysis") # personal computer

# LOAD DATA :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
clear.rate_914    <- read.csv(file="Data/Feeding_rates/Feeding_9.14.21_formatted.csv", header=T)  %>% dplyr::rename(Date = ï..Date)# 9/14 data - need to merge the 9.30 data
clear.rate_930    <- read.csv(file="Data/Feeding_rates/Feeding_9.30.21_worksheet_R.csv", header=T) %>% 
  dplyr::mutate(Date = '20210930') %>% # 9/14 data - need to merge the 9.30 data
  dplyr::mutate(Chamber_tank = paste(pH, Replicate, sep=''))
length.resp.clear <- read.csv(file="Data/Respiration/Lengths_resp.csv", header=T) %>% 
  dplyr::select(!c('Instrument','Center')) %>% 
  dplyr::mutate(Date = paste("20",(format(as.Date(length.resp.clear$Date, "%m/%d/%Y"), "%y%m%d")), sep =''))
length.resp.clear <- length.resp.clear[!is.na(length.resp.clear$Length.um.),]

# formatting for merge

# call the 914 data and remove blanks
clear.rate_914_Scallops <- merge( # note - merging with th elengths removed the blanks from the clearance rate data (no lengths for them! )
  (clear.rate_914 %>% dplyr::filter(!Chamber_tank %in% 'Blank')),
  (length.resp.clear %>% 
     dplyr::filter(Date %in% 20210914) %>% 
     dplyr::mutate(Chamber_tank = sub("_", "", Chamber_tank))) ) %>% 
  dplyr::select(!c('Food'))

  
# a bit more work with the 930 data to merge with the 914

clear.rate_930_Scallops <- merge( (data.frame(melt(clear.rate_930, id.vars = c('Date', 'Chamber_tank','Run', 'Plate', 'Sample.ID', 'Replicate', 'Number',  'pH', 'Fed_Unfed', 'Notes'))) %>% 
  dplyr::filter(!Fed_Unfed %in% 'blank') %>% 
  dplyr::mutate(Time._min = (gsub(".*_","", variable))) %>% 
  dplyr::rename(Count = value) %>% 
  dplyr::arrange(Run,Plate,pH,Replicate,Number,Fed_Unfed,Time._min) ), #merge with...
    (length.resp.clear %>% 
       dplyr::filter(Date %in% 20210930) %>% 
       dplyr::mutate(Chamber_tank = sub("_", "", Chamber_tank)) %>% 
       dplyr::mutate(Fed_Unfed = ifelse(Food == 1, 'fed', 'unfed')))
  ) %>% 
  dplyr::select(!c('variable', 'Food', 'Sample.ID'))



ClearRate_Master          <- rbind(clear.rate_914_Scallops, clear.rate_930_Scallops)

# calculate the clearace rate normalized for shell length 
ClearRate_Master$Cells_ml <- (ClearRate_Master$Count)*(1000/33)

# Initial plots to visualize the data a bit....
ClearRate_Master[!is.na(ClearRate_Master$Cells_ml),] %>% 
  dplyr::filter(Date %in% 20210914) %>% 
  dplyr::mutate(Time._min = as.numeric(as.character(Time._min))) %>% 
  ggplot(aes(Time._min, Cells_ml, color=Replicate)) + 
  geom_point(shape=1, color = "black")+ 
  ggtitle("F1 Scallops: Feed trials raw data 20210914")+
  labs(y = expression(Live~algae~cells~"("~cells~mL^{-1}~")"), 
       x = expression(Time~"("~minutes~")")) + 
  theme(plot.title= element_text(size =16, face ="bold", 
                                 lineheight = 8, vjust=1), aspect.ratio=1)+
  stat_smooth(method="lm", se = F) + 
  theme_bw() +
  scale_shape_identity() + 
  facet_wrap( ~ pH, scales = "free_x" )


ClearRate_Master[!is.na(ClearRate_Master$Cells_ml),] %>% 
  dplyr::filter(Date %in% 20210930) %>% 
  dplyr::mutate(pH_feed = paste(pH, Fed_Unfed, sep='_')) %>% 
  dplyr::mutate(Time._min = as.numeric(as.character(Time._min))) %>% 
  ggplot(aes(Time._min, Cells_ml, color=Replicate)) + 
  geom_point(shape=1, fill = "white", color = "black")+ 
  ggtitle("F1 Scallops: Feed trials raw data 20210930")+
  labs(y = expression(Live~algae~cells~"("~cells~mL^{-1}~")"), 
       x = expression(Time~"("~minutes~")")) + 
  theme(plot.title= element_text(size =16, face ="bold", 
                                 lineheight = 8, vjust=1), aspect.ratio=1)+
  stat_smooth(method="lm", se = F) + 
  theme_bw() +
  scale_shape_identity() + 
  facet_wrap( ~ pH_feed, scales = "free_x" )














# BLANKS average cell loss acrosss the control vessels for the specified time interval



clear.rate.914.Blanks <- clear.rate_914 %>% 
                                dplyr::filter(Chamber_tank %in% 'Blank') %>% 
                                dplyr::mutate(unique_identifier = paste(pH, "Run", Run, sep ='_')) %>% 
                                dplyr::mutate(Cells_ml = ((Count)*(1000/33)))

loop_914_BLANKS       <- as.data.frame(unique(clear.rate.914.Blanks$unique_identifier)) %>%
                            dplyr::rename(ID = "unique(clear.rate.914.Blanks$unique_identifier)")

meanBLANKS.914        <- data.frame() 
for (i in 1:nrow(loop_914_BLANKS)) {
  dat <- clear.rate.914.Blanks %>% 
          dplyr::filter(unique_identifier == loop_914_BLANKS[i,])
    for (j in 2:nrow(dat)) {
      dat2 <- dat %>% 
        dplyr::mutate(diff = as.numeric(dat$Time._min) - as.numeric(dat$Time._min[1]) ) %>% 
        dplyr::mutate(AlgaeLossRatio = as.numeric(dat$Cells_ml[1]) / as.numeric(dat$Cells_ml) ) %>% 
        dplyr::filter(!AlgaeLossRatio < 1) %>% 
        dplyr::mutate(ln_AlgaeLossRatio = ln(AlgaeLossRatio)) %>% 
        dplyr::mutate(Time_period = paste((as.numeric(substr(Time._min,1,1)) -1), "0-", Time._min, sep =''))
      dat2OM <- dat2 %>% dplyr::filter(!Time._min == 0)
    }
  if (nrow(dat2OM) > 0) {
    df       <- data.frame(dat2OM) # name dataframe for this single row
    meanBLANKS.914 <- rbind(meanBLANKS.914,df) #bind to a cumulative list dataframe
    print(meanBLANKS.914) # print to monitor progress
  } else {}
}

min(meanBLANKS.914$ln_AlgaeLossRatio)
max(meanBLANKS.914$ln_AlgaeLossRatio)
data.frame( meanBLANKS.914[!is.na(meanBLANKS.914$ln_AlgaeLossRatio),] %>% 
             dplyr::group_by(pH, substr(Time._min,1,1)) %>% 
             dplyr::summarise(
               meanBlank_ln_AlgaeLoss = mean(ln_AlgaeLossRatio),
               sdBlank_ln_AlgaeLoss   = sd(ln_AlgaeLossRatio),
               seBlank_ln_AlgaeLoss   = sd(ln_AlgaeLossRatio) / sqrt(length(ln_AlgaeLossRatio)), 
               n= n()))



  # FR =( V/t * (ln(C_0/C_t ) - A) )/ L











clear.rate_930.BLANKS <- (data.frame(melt(clear.rate_930, id.vars = c('Date', 'Chamber_tank','Run', 'Plate', 'Sample.ID', 'Replicate', 'Number',  'pH', 'Fed_Unfed', 'Notes'))) %>% 
                            dplyr::filter(Fed_Unfed %in% 'blank')  %>% 
                            dplyr::select(!'Chamber_tank') %>% 
                            dplyr::mutate(Time._min = (gsub(".*_","", variable))) %>% 
                            dplyr::rename(Count = value) %>% 
                            dplyr::arrange(Run,Plate,pH,Replicate,Number,Fed_Unfed,Time._min)  %>% 
                            dplyr::mutate(unique_identifier = paste(pH, "SampleID", Sample.ID, sep ='_')) %>% 
                            dplyr::mutate(Cells_ml = ((Count)*(1000/33))))

loop_930_BLANKS       <- as.data.frame(unique(clear.rate_930.BLANKS$unique_identifier)) %>%
                            dplyr::rename(ID = "unique(clear.rate_930.BLANKS$unique_identifier)")
meanBLANKS.930        <- data.frame() 
for (i in 1:nrow(loop_930_BLANKS)) {
  dat <- clear.rate_930.BLANKS %>% 
          dplyr::filter(unique_identifier == loop_930_BLANKS[i,])
  for (j in 2:nrow(dat)) {
    # ( (4/1000)* (lag(dat$Time._min, n = 1, default = first(Time._min))) ) ln(Cells_ml - (lag(Cells_ml, n = 1, default = first(Cells_ml))))
    dat2 <- dat %>% 
      dplyr::mutate(diff = as.numeric(dat$Time._min) - as.numeric(dat$Time._min[1]) ) %>% 
      dplyr::arrange(Time._min) %>% 
      dplyr::mutate(AlgaeLossRatio = as.numeric(dat$Cells_ml[1]) / as.numeric(dat$Cells_ml) ) %>% 
      dplyr::filter(!AlgaeLossRatio < 1) %>% 
      dplyr::mutate(ln_AlgaeLossRatio = ln(AlgaeLossRatio)) %>% 
      dplyr::mutate(Time_period = paste((as.numeric(substr(Time._min,1,1)) -1), "0-", Time._min, sep =''))
    dat2OM <- dat2 %>% dplyr::filter(!Time._min == 0)
  }
  if (nrow(dat2OM) > 0) {
    df       <- data.frame(dat2OM) # name dataframe for this single row
    meanBLANKS.930 <- rbind(meanBLANKS.930,df) #bind to a cumulative list dataframe
    print(meanBLANKS.930) # print to monitor progress
  } else {}
}

min(meanBLANKS.930$ln_AlgaeLossRatio)
max(meanBLANKS.930$ln_AlgaeLossRatio)
A_930_BlankMeans <- data.frame( meanBLANKS.930[!is.na(meanBLANKS.930$ln_AlgaeLossRatio),] %>% 
                      dplyr::group_by(pH, 
                                      Time._min) %>% 
                      dplyr::summarise(
                        meanBlank_ln_AlgaeLoss = mean(ln_AlgaeLossRatio),
                        sdBlank_ln_AlgaeLoss   = sd(ln_AlgaeLossRatio),
                        seBlank_ln_AlgaeLoss   = sd(ln_AlgaeLossRatio) / sqrt(length(ln_AlgaeLossRatio)), 
                        n= n()))
A_930_BlankMeans



















# Clearance rate anlaysis for 9/30 data
ClearRate_Master.914 <- ClearRate_Master %>% 
  dplyr::filter(Date %in% 20210914) %>% 
  dplyr::mutate(uniq_Identifier = paste(pH, "Run", Run, "Rep",Replicate, "Num", Number, sep='_'))

df_total.914              <- data.frame() # start dataframe 
loop_914 <- as.data.frame(unique(ClearRate_Master.914$uniq_Identifier)) %>% dplyr::rename(ID = "unique(ClearRate_Master.914$uniq_Identifier)")



SlopeTable_914 <- data.frame() # run this before the loop
for(i in 1:nrow(loop_914)){
  dat <- ClearRate_Master.914 %>%  filter(uniq_Identifier %in% loop_914[i,])
    slope<- summary(lm((dat$Cells_ml) ~ as.numeric(dat$Time._min)))$coef[2,"Estimate"]
    SLOPE  <- summary(lm((dat$Cells_ml) ~ as.numeric(dat$Time._min)))$r.squared
    pval <- summary(lm((dat$Cells_ml) ~ as.numeric(dat$Time._min)))$coef[2,"Pr(>|t|)"]
    mod  <- lm(as.numeric(dat$Time._min) ~ dat$Cells_ml)
    norm_assum <- shapiro.test(resid(mod))
    shapiro_pval <- norm_assum$p.value
    # assign the data table 
    SLOPE.loop <- data.frame(matrix(nrow = 1, ncol = 5)) # create a new data table
    colnames(SLOPE.loop) <- c('pH', 'Replicate', 'slope', 'SLOPE', 'pval') # assign headers
    SLOPE.loop$pH        <- gsub("_.*", "\\1", loop_914[i,])
    SLOPE.loop$Replicate <- gsub("^(?:[^_]+_){4}([^_]+).*", "\\1", loop_914[i,])
    SLOPE.loop$slope     <- slope * 60  # cells per mL per hour  
    SLOPE.loop$Number    <- gsub("^(?:[^_]+_){6}([^_]+).*", "\\1", loop_914[i,])
    SLOPE.loop$SLOPE       <- SLOPE
    SLOPE.loop$pval      <- pval
    SLOPE.loop$shapiro_pval <- shapiro_pval
    # loop additions 
    df <- data.frame(SLOPE.loop) # name dataframe for this single row
    SlopeTable_914 <- rbind(SlopeTable_914,df) # bind to a cumulative list dataframe
  print(SlopeTable_914) # show loop progress in the console
}# outside loo
SlopeTable_914
SLOPE_mod_914 <- aov(lm(slope ~ pH, data= SlopeTable_914))
DF   <- paste( (summary(SLOPE_mod_914)[[1]][["Df"]])[1], (summary(SLOPE_mod_914)[[1]][["Df"]])[2], sep = '/')
Fval <- (summary(SLOPE_mod_914)[[1]][["F value"]])[1]
pval <- (summary(SLOPE_mod_914)[[1]][["Pr(>F)"]])[1]

ggplot(SlopeTable_914, aes(pH , abs(slope) , fill = pH)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pH)) +
  scale_fill_manual(values=c("grey50","white")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  scale_x_discrete(labels= c('Elevated (H)', 'Ambient (L)')) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title = "F1 Scallops: Slope algae cells/time - feeding rate trials 20210914", 
       y = expression(Slope~"="~absolute~value~"("~Live~algae~cells~mL^{-1}~hour^{-1}~")"), 
       x = expression(italic(p)*CO[2]~Treatment~"("~mu*atm~")")) + 
  annotate("text", x=2, y= 25000, size = 4, label = "aov(slope~pH Treatment)") +
  annotate("text", x=2, y= 24300, size = 4, label= paste('DF =',DF,'F =', signif(Fval, digits=3), 'p value =', signif(pval, digits=3), sep=" ")) 





for (i in 1:nrow(loop_914)) {
  dat <- ClearRate_Master.914 %>% 
    dplyr::filter(uniq_Identifier == loop_914[1,]) %>% 
    dplyr::arrange(Time._min)
  dat2 <- dat %>% 
    #dplyr::mutate(diff = as.numeric(Time._min) - lag(as.numeric(Time._min), default = first(as.numeric(Time._min)))) %>% 
    dplyr::mutate(diff = as.numeric(dat$Time._min) - as.numeric(dat$Time._min[1]) ) %>% 
    dplyr::mutate(AlgaeLossRatio = as.numeric(dat$Cells_ml[1]) / as.numeric(dat$Cells_ml) ) %>% 
    dplyr::filter(!AlgaeLossRatio < 1) %>% 
    dplyr::mutate(ln_AlgaeLossRatio = ln(AlgaeLossRatio)) %>% 
   # dplyr::mutate(ClearanceRate =  ( (25/1000) / (diff/60)  * # V / t == Volume of the vessel (in Liters as 25 ml / 1000 ml L-1) and t = time in hours as the diff between the interval sin minutes / 60 mins hour-1
   #                                   ( ln_AlgaeLossRatio ) / Length.um. ) ) %>% 
    dplyr::mutate(ClearanceRate_L_hour_meter = ( (25/1000) * # V / t == Volume of the vessel (in Liters as 25 ml / 1000 ml L-1) and t = time in hours as the diff between the interval sin minutes / 60 mins hour-1
                                       (( ln_AlgaeLossRatio /  (diff/60) - 0))) / (Length.um./1000000) ) %>% 
    dplyr::mutate(Time_period = paste((as.numeric(substr(Time._min,1,1)) -1), "0-", Time._min, sep =''))
    dat2OM <- dat2 %>% dplyr::filter(!Time._min == 0)
    
  if (nrow(dat2OM) > 0) {
    ClearRate.table           <- data.frame(matrix(nrow = nrow(dat2OM), ncol = 9)) # create dataframe to save cumunalitively during for loop
    colnames(ClearRate.table) <- c('Date', 'ID', 'pH', 'Replicate', 'Num','Run','Time_period', 'AlgaeLossRatio', 'ClearanceRate_L_hour_meter') # names for comuns in the for loop
    
    ClearRate.table$Date                       <- dat2OM$Date
    ClearRate.table$ID                         <- loop_914[i,]
    ClearRate.table$pH                         <- gsub("_.*", "\\1", ClearRate.table$ID)
    ClearRate.table$Replicate                  <- gsub("^(?:[^_]+_){4}([^_]+).*", "\\1", ClearRate.table$ID)
    ClearRate.table$Num                        <- gsub("^(?:[^_]+_){6}([^_]+).*", "\\1", ClearRate.table$ID)
    ClearRate.table$Run                        <- gsub("^(?:[^_]+_){2}([^_]+).*", "\\1", ClearRate.table$ID)
    ClearRate.table$Time_period                <- paste((as.numeric(substr(dat2OM$Time._min,1,1)) -1), "0-", dat2OM$Time._min, sep ='')
    ClearRate.table$AlgaeLossRatio             <- dat2OM$AlgaeLossRatio
    ClearRate.table$ClearanceRate_L_hour_meter <- dat2OM$ClearanceRate_L_hour_meter
    
    df       <- data.frame(ClearRate.table) # name dataframe for this single row
    df_total.914 <- rbind(df_total.914,df) #bind to a cumulative list dataframe
    print(df_total.914) # print to monitor progress
  }
  else {}
}

ClearRates_914_Means <- df_total.914 %>% 
  dplyr::filter(!ClearanceRate_L_hour_meter %in% '-Inf') %>% 
  dplyr::group_by(pH, substr(Time_period,4,4)) %>% 
  dplyr::summarise(
    meanCR = mean(abs(ClearanceRate_L_hour_meter)),
    sdCR   = sd(abs(ClearanceRate_L_hour_meter)), 
    seCR   = sd(abs(ClearanceRate_L_hour_meter)) / sqrt(length(abs(ClearanceRate_L_hour_meter))), 
    n = n()) %>% 
  na.omit()
colnames(ClearRates_914_Means)[2] <- 'Time_period'

summary(lmer(meanCR~pH+ (1|Time_period), data=ClearRates_914_Means))
summary(aov(lm(meanCR~pH*Time_period, data=ClearRates_914_Means)))

ClearRates_914_Means %>% 
  #dplyr::filter(!Time_period %in% c('40-50', '50-60')) %>% 
  ggplot(aes(x=pH , y=meanCR, fill = pH)) +
  geom_bar(position=position_dodge(), aes(y=meanCR), stat="identity", alpha=0.5) +
  scale_fill_manual("Treatment", values = c("8" = "grey50", "7.5" = "black")) +
  geom_errorbar(position=position_dodge(width=0.9), aes(ymin=meanCR+seCR, ymax=meanCR+seCR), width=0.2, colour="black", alpha=0.9, size=0.2) +
  geom_linerange(aes(ymin = meanCR, ymax = meanCR+seCR)) + 
  geom_point(position=position_dodge(width=0.9), aes(y=meanCR)) +
  theme_classic() +
  ggtitle("Clearance Rate F1 Scallops 20210914") + 
  facet_wrap(~Time_period)





















# Clearance rate anlaysis for 9/30 data
ClearRate_Master.930       <- ClearRate_Master %>% 
  dplyr::filter(Date %in% 20210930) %>% 
  dplyr::mutate(uniq_Identifier = paste(pH, Fed_Unfed, "Run", Run, "Rep",Replicate, "Num", Number, sep='_'))
df_total.930              <- data.frame() # start dataframe 
loop_930 <- as.data.frame(unique(ClearRate_Master.930$uniq_Identifier)) %>% dplyr::rename(ID = "unique(ClearRate_Master.930$uniq_Identifier)")






SlopeTable_930 <- data.frame() # run this before the loop
for(i in 1:nrow(loop_930)){
  dat <- ClearRate_Master.930 %>%  filter(uniq_Identifier %in% loop_930[i,])
  slope<- summary(lm((dat$Cells_ml) ~ as.numeric(dat$Time._min)))$coef[2,"Estimate"]
  SLOPE  <- summary(lm((dat$Cells_ml) ~ as.numeric(dat$Time._min)))$r.squared
  pval <- summary(lm((dat$Cells_ml) ~ as.numeric(dat$Time._min)))$coef[2,"Pr(>|t|)"]
  mod  <- lm(as.numeric(dat$Time._min) ~ dat$Cells_ml)
  norm_assum <- shapiro.test(resid(mod))
  shapiro_pval <- norm_assum$p.value
  # assign the data table 
  SLOPE.loop <- data.frame(matrix(nrow = 1, ncol = 6)) # create a new data table
  colnames(SLOPE.loop) <- c('pH', 'Replicate', 'Fed_Unfed','slope', 'SLOPE', 'pval') # assign headers
  SLOPE.loop$pH        <- gsub("_.*", "\\1", loop_930[i,])
  SLOPE.loop$Replicate <- gsub("^(?:[^_]+_){5}([^_]+).*", "\\1", loop_930[i,])
  SLOPE.loop$Fed_Unfed <- gsub("^(?:[^_]+_){1}([^_]+).*", "\\1", loop_930[i,])
  SLOPE.loop$slope     <- slope * 60  # cells per mL per hour  
  SLOPE.loop$Number    <- gsub("^(?:[^_]+_){7}([^_]+).*", "\\1", loop_930[i,])
  SLOPE.loop$SLOPE       <- SLOPE
  SLOPE.loop$pval      <- pval
  SLOPE.loop$shapiro_pval <- shapiro_pval
  # loop additions 
  df <- data.frame(SLOPE.loop) # name dataframe for this single row
  SlopeTable_930 <- rbind(SlopeTable_930,df) # bind to a cumulative list dataframe
  print(SlopeTable_930) # show loop progress in the console
}# outside loo
SlopeTable_930
SLOPE_mod_930 <- aov(lm(slope ~ pH*Fed_Unfed, data= SlopeTable_930))
summary(SLOPE_mod_930)
DF.930   <- paste( (summary(SLOPE_mod_930)[[1]][["Df"]])[1], (summary(SLOPE_mod_930)[[1]][["Df"]])[2], sep = '/')
Fval.930 <- (summary(SLOPE_mod_930)[[1]][["F value"]])[1]
pval.930 <- (summary(SLOPE_mod_930)[[1]][["Pr(>F)"]])[1]

SlopeTable_930 %>%  mutate(pH_feed = paste(pH, Fed_Unfed, sep = '_')) %>% 
ggplot(aes(pH , abs(slope) , fill = pH)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pH)) +
  scale_fill_manual(values=c("grey50","white")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  scale_x_discrete(labels= c('Elevated (H)', 'Ambient (L)')) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title = "F1 Scallops: Slope algae cells/time - feeding rate trials 20210930", 
       y = expression(Slope~"="~absolute~value~"("~Live~algae~cells~mL^{-1}~hour^{-1}~")"), 
       x = expression(italic(p)*CO[2]~Treatment~"("~mu*atm~")")) + 
  facet_wrap(~ Fed_Unfed)





for (i in 1:nrow(loop_930)) {
    dat        <- ClearRate_Master.930 %>% 
      dplyr::filter(uniq_Identifier == loop_930[i,]) %>% 
      dplyr::arrange(Time._min)
    
    C0 <- as.numeric(dat$Cells_ml)[1]
    
    dat_blanks <- merge(dat, A_930_BlankMeans)
    dat2       <- dat_blanks %>% 
        #dplyr::mutate(diff = as.numeric(Time._min) - lag(as.numeric(Time._min), default = first(as.numeric(Time._min)))) %>% 
        # dplyr::mutate(diff = as.numeric(dat_blanks$Time._min) - as.numeric(dat$Time._min[1]) ) %>% 
        dplyr::mutate(AlgaeLossRatio = C0 / as.numeric(dat_blanks$Cells_ml) ) %>% 
        dplyr::filter(!AlgaeLossRatio < 1) %>% 
        dplyr::mutate(ln_AlgaeLossRatio = ln(AlgaeLossRatio)) %>% 
        # dplyr::mutate(ClearanceRate =  ( (25/1000) / (diff/60)  * # V / t == Volume of the vessel (in Liters as 25 ml / 1000 ml L-1) and t = time in hours as the diff between the interval sin minutes / 60 mins hour-1
        #                                   ( ln_AlgaeLossRatio ) / Length.um. ) ) %>% 
        dplyr::mutate(ClearanceRate_L_hour_mm = ( (25/1000) * # V / t == Volume of the vessel (in Liters as 25 ml / 1000 ml L-1) and t = time in hours as the diff between the interval sin minutes / 60 mins hour-1
                                                        ( ( ln_AlgaeLossRatio /  ((as.numeric(Time._min))/60)) - meanBlank_ln_AlgaeLoss)  ) / (Length.um./1000) ) %>% 
        dplyr::mutate(Time_period = paste((as.numeric(substr(Time._min,1,1)) -1), "0-", Time._min, sep =''))
    dat2OM <- dat2 %>% dplyr::filter(!Time._min == 0)
    
    if (nrow(dat2OM) > 0) {
    ClearRate.table           <- data.frame(matrix(nrow = nrow(dat2OM), ncol = 10)) # create dataframe to save cumunalitively during for loop
    colnames(ClearRate.table) <- c('Date', 'ID', 'pH', 'Fed_Unfed', 'Replicate', 'Num','Run','Time_period', 'AlgaeLossRatio', 'ClearanceRate_L_hour_mm') # names for comuns in the for loop
    
    ClearRate.table$Date                       <- dat2OM$Date
    ClearRate.table$ID                         <- loop_930[i,]
    ClearRate.table$pH                         <- gsub("_.*", "\\1", ClearRate.table$ID)
    ClearRate.table$Fed_Unfed                  <- gsub("^(?:[^_]+_){1}([^_]+).*", "\\1", ClearRate.table$ID)
    ClearRate.table$Replicate                  <- gsub("^(?:[^_]+_){5}([^_]+).*", "\\1", ClearRate.table$ID)
    ClearRate.table$Num                        <- gsub("^(?:[^_]+_){7}([^_]+).*", "\\1", ClearRate.table$ID)
    ClearRate.table$Run                        <- gsub("^(?:[^_]+_){3}([^_]+).*", "\\1", ClearRate.table$ID)
    ClearRate.table$Time_period                <- paste((as.numeric(substr(dat2OM$Time._min,1,1)) -1), "0-", dat2OM$Time._min, sep ='')
    ClearRate.table$AlgaeLossRatio             <- dat2OM$AlgaeLossRatio
    ClearRate.table$ClearanceRate_L_hour_mm <- dat2OM$ClearanceRate_L_hour_mm
    
    df       <- data.frame(ClearRate.table) # name dataframe for this single row
    df_total.930 <- rbind(df_total.930,df) #bind to a cumulative list dataframe
    print(df_total.930) # print to monitor progress
    }
    else {}
}

ClearRates_930_Means <- df_total.930 %>% 
  dplyr::filter(!ClearanceRate_L_hour_mm %in% 'NaN') %>% 
  dplyr::group_by(pH, Fed_Unfed, Time_period) %>% 
  dplyr::summarise(
    meanCR = mean(ClearanceRate_L_hour_mm),
    sdCR   = sd(ClearanceRate_L_hour_mm), 
    seCR   = sd(ClearanceRate_L_hour_mm) / sqrt(length(ClearanceRate_L_hour_mm)), 
    n = n()) %>% 
  na.omit()
ClearRates_930_Means

summary(lmer(meanCR~pH*Fed_Unfed + (1|Time_period), data=ClearRates_930_Means))
summary(aov(lm(meanCR~pH*Fed_Unfed, data=ClearRates_930_Means)))

ClearRates_930_Means$pH_feed <- paste(ClearRates_930_Means$pH, ClearRates_930_Means$Fed_Unfed, sep='_')
ClearRates_930_Means %>% 
#dplyr::filter(!Time_period %in% c('40-50', '50-60')) %>% 
ggplot(aes(x=pH_feed , y=meanCR, fill = pH_feed)) +
  geom_bar(position=position_dodge(), aes(y=meanCR), stat="identity", alpha=0.5) +
  scale_fill_manual("Treatment", values = c("8_fed" = "grey70", "7.5_fed" = "grey 10", "8_unfed" = "grey 70", "7.5_unfed" = "black")) +
  geom_errorbar(position=position_dodge(width=0.9), aes(ymin=meanCR+seCR, ymax=meanCR+seCR), width=0.2, colour="black", alpha=0.9, size=0.2) +
  geom_linerange(aes(ymin = meanCR, ymax = meanCR+seCR)) + 
  geom_point(position=position_dodge(width=0.9), aes(y=meanCR)) +
  theme_classic() +
  ggtitle("Clearance Rate F1 Scallops 20210930") + 
  facet_wrap(~ Time_period)


df_total.930 %>% 
  dplyr::mutate(pH_feed = paste(pH, Fed_Unfed, sep ='_')) %>% 
  #dplyr::filter(!ClearanceRate_L_hour_mm %in% '-Inf') %>% 
  #dplyr::filter(!Time_period %in% c('40-50', '50-60')) %>% 
  ggplot(aes(pH_feed , ClearanceRate_L_hour_mm , fill = pH_feed)) +
  theme(panel.grid=element_blank()) +
  geom_boxplot(size=0.2, alpha=0.1, aes(fill=pH_feed)) +
  scale_fill_manual(values=c("white", "grey70","grey40", "grey20")) +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(jitter.width = 0.1)) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  # labs(title = "F1 Scallops: Slope algae cells/time - feeding rate trials 20210930", 
  #      y = expression(Slope~"="~absolute~value~"("~Live~algae~cells~mL^{-1}~hour^{-1}~")"), 
  #      x = expression(italic(p)*CO[2]~Treatment~"("~mu*atm~")")) +
  ggtitle("Clearance Rate F1 Scallops 20210930") +
  facet_wrap(~ Time_period)

