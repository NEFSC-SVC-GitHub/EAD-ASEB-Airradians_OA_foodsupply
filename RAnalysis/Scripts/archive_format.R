

setwd("C:/Users/samjg/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis/")

# LOAD PACKAGES :::::::::::::::::::::::::::::::::::::::::::::::::::::::

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(reshape2)

# LOAD DATA :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

Master.resp    <- read.csv(file="Output/Respiration/Calculated_Resp_Master.csv", header=T)  %>% 
                        dplyr::select(c('Date','pH','Food','Replicate','Length_um','Dry_Shell_weight','Dry_Tissue_weight','whole_Dry_weight','resp_umol_L_hr','resp_µmol_L_mm_Length_hr','resp_µmol_mg_hr', 'Number')) %>% 
                        dplyr::mutate(animal_age = case_when(Date == '9/14/2021' ~ 50, 
                                                             Date == '9/30/2021' ~ 66, 
                                                             Date == '10/26/2021' ~ 92)) %>% 
                        dplyr::mutate(pCO2_treatment = case_when(pH == 8 ~ 'low', 
                                                                 pH == 7.5 ~ 'mid')) %>% 
                        dplyr::relocate(c(animal_age, pCO2_treatment), .after = Date) %>% 
                        dplyr::rename('Dry_Shell_weight_g' = 'Dry_Shell_weight', 
                                      'Dry_Tissue_weight_g' = 'Dry_Tissue_weight',
                                      'Whole_Dry_weight_g' = 'whole_Dry_weight',
                                      'Respiration_umol.L.hr' = 'resp_umol_L_hr',
                                      'Respiration_umol.L.mm.hr' = 'resp_µmol_L_mm_Length_hr',
                                      'Respiration_umol.L.mgWholeDW.hr' = 'resp_µmol_mg_hr') %>% 
                        dplyr::mutate(Food = replace_na(Food, 'fed')) %>% 
                        dplyr::arrange(mdy(Date))
nrow(Master.resp) # 116

Master.clearance <- read.csv(file="Output/ClearanceRates/ClearanceRate_Master.csv", header=T) %>% 
                        dplyr::select(c('Date','pH', 'Replicate', 'Fed_Unfed','Number','Length_um','CR_mL.hr.mmlength','type')) %>% 
                        reshape(idvar = c('Date', 'pH', 'Replicate', 'Fed_Unfed', 'Number', 'Length_um'),timevar = 'type', direction = 'wide')  %>% 
                        dplyr::rename('ClearanceRate_mL.hr.mmlength.ChaetB'       = 'CR_mL.hr.mmlength.chaet', 
                                       'ClearanceRate_mL.hr.mmlength.Tetraselmis' = 'CR_mL.hr.mmlength.ply',
                                       'ClearanceRate_mL.hr.mmlength.seston'      = 'CR_mL.hr.mmlength.seston',
                                       'Food' = 'Fed_Unfed',
                                       'Length_um_CR' = 'Length_um') %>% 
                       dplyr::mutate(Date = ymd(as.character(Date))) %>% 
                       dplyr::mutate(Date = as.character(format(as.Date(Date, '%Y-%m-%d'), "%m/%d/%Y"))) %>% 
                       dplyr::mutate(Date = case_when(Date == '09/14/2021' ~ '9/14/2021', 
                                                      Date == '09/30/2021' ~ '9/30/2021',
                                                      Date == '10/26/2021' ~ '10/26/2021'))

nrow(Master.clearance) # 128

MasterRespClearnace <- merge(Master.resp, Master.clearance, by = c('Date', 'pH', 'Food', 'Replicate', 'Number'))
write.csv(MasterRespClearnace, "C:/Users/samjg/Documents/Github_repositories/Airradians_OA-foodsupply/RAnalysis/Output/Phys_Master_archive_format.csv")
