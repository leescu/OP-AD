# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
setwd("M:/OP&AZ/Cohort data")
library(data.table)
library(dplyr)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 1 Dementia & alzheimer arrangement ####
Data_participant<-fread("original data/data_participant.csv")
Data_death<-fread("original data/data_death.csv")
head(Data_participant)
colnames(Data_participant)
#### Date data ####
Date_data<-as.data.frame(Data_participant$eid)
colnames(Date_data)<-"eid"
#Center_date
Date_data$Center_date<-as.Date(Data_participant$p53_i0)
#Death_date
Data_death$dnx_death_id<-NULL
colnames(Data_death)[2]<-"Death_date"
Data_death$Death_date<-as.Date(Data_death$Death_date)
Data_death<-Data_death[order(Data_death$Death_date),]

Data_death <- unique(Data_death)
Date_data<-merge(Date_data,Data_death,by="eid",all.x = T)
#Dementia_date
date_Dem<-Data_participant[,c("eid","p21003_i0","p130836","p130838",
                              "p130840","p130842",
                              "p131036","p42018",
                              "p42020","p42022",
                              "p42024","p191")]
Date_data<-merge(Date_data,date_Dem,by = "eid",all.x = T)
Date_data[Date_data==""] <- NA
Date_data$p42018[Date_data$p42018=="Date is unknown"] <- NA	
#Date F00 first reported (dementia in alzheimer's disease)
Date_data$Dementia_F00_date<-as.Date(Date_data$p130836)
Date_data$Dementia_F00<-ifelse(is.na(Date_data$Dementia_F00_date),0,1)
#130838 Date F01 first reported (vascular dementia)
Date_data$Dementia_F01_date<-as.Date(Date_data$p130838)
Date_data$Dementia_F01<-ifelse(is.na(Date_data$Dementia_F01_date),0,1)
#130840 Date F02 first reported (dementia in other diseases classified elsewhere)
Date_data$Dementia_F02_date<-as.Date(Date_data$p130840)
Date_data$Dementia_F02<-ifelse(is.na(Date_data$Dementia_F02_date),0,1)
#130842 Date F03 first reported (unspecified dementia)
Date_data$Dementia_F03_date<-as.Date(Date_data$p130842)
Date_data$Dementia_F03<-ifelse(is.na(Date_data$Dementia_F03_date),0,1)
#131036 Date G30 first reported (alzheimer's disease)
Date_data$Dementia_G30_date<-as.Date(Date_data$p131036)
Date_data$Dementia_G30<-ifelse(is.na(Date_data$Dementia_G30_date),0,1)
#42018 Date of all cause dementia report
Date_data$Dementia_all_date<-as.Date(Date_data$p42018)
Date_data$Dementia_all<-ifelse(is.na(Date_data$Dementia_all_date),0,1)
#42020 Date of alzheimer's disease report
Date_data$Dementia_ad_date<-as.Date(Date_data$p42020)
Date_data$Dementia_ad<-ifelse(is.na(Date_data$Dementia_ad_date),0,1)
#42022 Date of vascular dementia report
Date_data$Dementia_vd_date<-as.Date(Date_data$p42022)
Date_data$Dementia_vd<-ifelse(is.na(Date_data$Dementia_vd_date),0,1)
#42024 Date of frontotemporal dementia report
Date_data$Dementia_fd_date<-as.Date(Date_data$p42024)
Date_data$Dementia_fd<-ifelse(is.na(Date_data$Dementia_fd_date),0,1)
Dementia_date<-Date_data %>%
  transmute(earliest_date = pmin(Date_data$Dementia_F00_date,
                                 Date_data$Dementia_F01_date,
                                 Date_data$Dementia_F02_date,
                                 Date_data$Dementia_F03_date,
                                 Date_data$Dementia_G30_date,
                                 Date_data$Dementia_all_date,
                                 Date_data$Dementia_ad_date,
                                 Date_data$Dementia_vd_date,
                                 Date_data$Dementia_fd_date,
                                 
                                 na.rm = T))
Date_data$Dementia_date<-as.Date(Dementia_date$earliest_date)
#vascular dementia
Vascular_Dementia_date<-Date_data %>%
  transmute(earliest_date = pmin(Date_data$Dementia_F01_date,
                                 Date_data$Dementia_vd_date,
                                 Date_data$Dementia_all_date,
                                 na.rm = T))
Date_data$Vascular_Dementia_date<-as.Date(Vascular_Dementia_date$earliest_date)
Vascular_Dementia<-Date_data %>%
  transmute(earliest_date = pmin(Date_data$Dementia_F01_date,
                                 Date_data$Dementia_vd_date,
                                 na.rm = T))
Date_data$Vascular_Dementia<-ifelse(is.na(Vascular_Dementia$earliest_date),0,1)
#alzheimer's disease
Alzheimer_Dementia_date<-Date_data %>%
  transmute(earliest_date = pmin(Date_data$Dementia_F00_date,
                                 Date_data$Dementia_G30_date,
                                 Date_data$Dementia_all_date,
                                 Date_data$Dementia_ad_date,
                                 na.rm = T))
Date_data$Alzheimer_Dementia_date<-as.Date(Alzheimer_Dementia_date$earliest_date)

Alzheimer_Dementia<-Date_data %>%
  transmute(earliest_date = pmin(Date_data$Dementia_F00_date,
                                 Date_data$Dementia_G30_date,
                                 Date_data$Dementia_ad_date,
                                 na.rm = T))
Date_data$Alzheimer_Dementia<-ifelse(is.na(Alzheimer_Dementia$earliest_date),0,1)


#frontotemporal dementia
#Frontotemporal_Dementia_date<-Date_data %>%
#  transmute(earliest_date = pmin(Date_data$Dementia_F02_date,
#                                 Date_data$Dementia_F03_date,
#                                 Date_data$Dementia_all_date,
#                                 Date_data$Dementia_fd_date,
#                                 na.rm = T))
#Date_data$Frontotemporal_Dementia_date<-as.Date(Frontotemporal_Dementia_date$earliest_date)
#Frontotemporal_Dementia<-Date_data %>%
#  transmute(earliest_date = pmin(Date_data$Dementia_fd_date,
#                                 na.rm = T))
#Date_data$Frontotemporal_Dementia<-ifelse(is.na(Frontotemporal_Dementia$earliest_date),0,1)


#other dementia
Other_Dementia_date<-Date_data %>%
  transmute(earliest_date = pmin(Date_data$Dementia_F02_date,
                                 Date_data$Dementia_F03_date,
                                 Date_data$Dementia_fd_date,
                                 Date_data$Dementia_all_date,
                                 na.rm = T))
Date_data$Other_Dementia_date<-as.Date(Other_Dementia_date$earliest_date)
Other_Dementia<-Date_data %>%
  transmute(earliest_date = pmin(Date_data$Dementia_F02_date,
                                 Date_data$Dementia_F03_date,
                                 Date_data$Dementia_fd_date,
                                 Date_data$Dementia_all_date,
                                 na.rm = T))
Date_data$Other_Dementia_original<-ifelse(is.na(Other_Dementia$earliest_date),0,1)
Date_data$Other_Dementia[Date_data$Other_Dementia_original==0]<-0
Date_data$Other_Dementia[Date_data$Other_Dementia_original==1&
                           (Date_data$Alzheimer_Dementia==1|Date_data$Vascular_Dementia==1)]<-0
Date_data$Other_Dementia[Date_data$Other_Dementia_original==1&
                           (Date_data$Alzheimer_Dementia==0&Date_data$Vascular_Dementia==0)]<-1
table(Date_data$Other_Dementia,useNA="ifany")

Date_data$Lost_date<-as.Date(Date_data$p191)
Date_data$Last_date<-as.Date(ifelse(is.na(Date_data$Dementia_date),"2023-09-30",NA))
Finally<-Date_data %>%
  transmute(earliest_date = pmin(Date_data$Dementia_date,Date_data$Death_date,Date_data$Lost_date,Date_data$Last_date, na.rm = T))
Date_data$Finally_date<-as.Date(Finally$earliest_date)
Date_data$days<-Date_data$Finally_date-Date_data$Center_date
Date_data$days_dementia<-Date_data$Finally_date-Date_data$Dementia_date
Date_data$days[Date_data$days_dementia<0]<-NA
#501928 miss
Date_data<-subset(Date_data,days>0)
Date_data$Dementia=ifelse(is.na(Date_data$Dementia_date),0,1)
table(Date_data$Dementia)
#Vascular Dementia
Date_data$days_vd<-Date_data$Dementia_date-Date_data$Vascular_Dementia_date
Date_data$Vascular_dementia[Date_data$Dementia==1]<-2
Date_data$Vascular_dementia[(Date_data$days_vd==0)&(Date_data$Vascular_Dementia==1)]<-1
Date_data$Vascular_dementia[is.na(Date_data$Dementia_date)]<-0
table(Date_data$Vascular_dementia)
#Alzheimer Dementia
Date_data$days_ad<-Date_data$Dementia_date-Date_data$Alzheimer_Dementia_date
Date_data$Alzheimer_dementia[Date_data$Dementia==1]<-2
Date_data$Alzheimer_dementia[(Date_data$days_ad==0)&(Date_data$Alzheimer_Dementia==1)]<-1
Date_data$Alzheimer_dementia[is.na(Date_data$Dementia_date)]<-0
table(Date_data$Alzheimer_dementia)
#Frontotemporal dementia
#Date_data$days_fd<-Date_data$Dementia_date-Date_data$Frontotemporal_Dementia_date
#Date_data$Frontotemporal_dementia[Date_data$Dementia==1]<-2
#Date_data$Frontotemporal_dementia[(Date_data$days_fd==0)&(Date_data$Frontotemporal_Dementia==1)]<-1
#Date_data$Frontotemporal_dementia[is.na(Date_data$Dementia_date)]<-0
#table(Date_data$Frontotemporal_dementia)
#Other dementia
Date_data$days_od<-Date_data$Dementia_date-Date_data$Other_Dementia_date
Date_data$Other_dementia[Date_data$Dementia==1]<-2
Date_data$Other_dementia[(Date_data$days_od==0)&(Date_data$Other_Dementia==1)]<-1
Date_data$Other_dementia[is.na(Date_data$Dementia_date)]<-0
table(Date_data$Other_dementia)
Date_data$years<-Date_data$days/365
Date_data$Age_dem<-as.integer(Date_data$p21003_i0+Date_data$years)
Date_data$Alzheimer_type[(Date_data$Alzheimer_dementia==1)&(Date_data$Age_dem>=65)]<-1
Date_data$Alzheimer_type[(Date_data$Alzheimer_dementia==1)&(Date_data$Age_dem<65)]<-0
table(Date_data$Alzheimer_type)
colnames(Date_data)
Date_data_final<-Date_data[,c("eid","days","Center_date","Death_date","Lost_date","Last_date","Finally_date",
                              "Dementia_date","Dementia","Vascular_Dementia_date","Vascular_dementia",
                              "Alzheimer_Dementia_date","Alzheimer_dementia",
                              "Other_Dementia_date","Other_dementia","Alzheimer_type")]
save(Date_data_final,file ="original data/Date_data_final.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
setwd("M:/OP&AZ/Cohort data")
library(data.table)
library(dplyr)
# >>>>> section 2 Cognitive domains arrangement ####
#### cognitive domains.csv ####
Data_cognitive<-fread("original data/cognitive domains.csv")
Cognitive_data<-as.data.frame(Data_cognitive$eid)
colnames(Cognitive_data)<-"eid"
colnames(Data_cognitive)
Data_cognitive[Data_cognitive==""] <- NA
Data_cognitive[Data_cognitive=="Trail not completed"] <- NA

table(Data_cognitive$p20018_i2,useNA = "ifany")
#prospective memory
Cognitive_data$Prospective_memory_1[Data_cognitive$p20018_i0=="Correct recall on first attempt"]<-1
Cognitive_data$Prospective_memory_1[Data_cognitive$p20018_i0=="Correct recall on second attempt"|
                                      Data_cognitive$p20018_i0=="Instruction not recalled, either skipped or incorrect"]<-0
table(Cognitive_data$Prospective_memory_1,useNA = "ifany")

Cognitive_data$Prospective_memory_2[Data_cognitive$p20018_i2=="Correct recall on first attempt"]<-1
Cognitive_data$Prospective_memory_2[Data_cognitive$p20018_i2=="Correct recall on second attempt"|
                                    Data_cognitive$p20018_i2=="Instruction not recalled, either skipped or incorrect"]<-0
table(Cognitive_data$Prospective_memory_2,useNA = "ifany")

#Fluid intelligence score
Cognitive_data$Fluid_intelligence_1<-Data_cognitive$p20016_i0
table(Cognitive_data$Fluid_intelligence_1,useNA = "ifany")

Cognitive_data$Fluid_intelligence_2<-Data_cognitive$p20016_i2
table(Cognitive_data$Fluid_intelligence_2,useNA = "ifany")



#Reaction time 
Cognitive_data$Reaction_time_1<-Data_cognitive$p20023_i0
summary(Cognitive_data$Reaction_time_1,useNA = "ifany")

Cognitive_data$Reaction_time_2<-Data_cognitive$p20023_i2
summary(Cognitive_data$Reaction_time_2,useNA = "ifany")


#Trail Making Test A 
Cognitive_data$Trail_Making_Test_A<-as.numeric(Data_cognitive$p6348_i2)
table(Cognitive_data$Trail_Making_Test_A,useNA = "ifany")
summary(Cognitive_data$Trail_Making_Test_A)
#Trail Making Test B
Cognitive_data$Trail_Making_Test_B<-as.numeric(Data_cognitive$p6350_i2)
table(Data_cognitive$p6350_i2,useNA = "ifany")
summary(Cognitive_data$Trail_Making_Test_B)
#Matrix pattern completion
Cognitive_data$Matrix_pattern_completion<-Data_cognitive$p6373_i2
table(Cognitive_data$Matrix_pattern_completion,useNA = "ifany")
#Broken letter recognition
Cognitive_data$Broken_letter_recognition<-Data_cognitive$p20139_i2
table(Cognitive_data$Broken_letter_recognition,useNA = "ifany")
#Picture vocabulary
Cognitive_data$Picture_vocabulary<-Data_cognitive$p26302_i2
table(Cognitive_data$Picture_vocabulary,useNA = "ifany")
summary(Cognitive_data$Picture_vocabulary)
#Tower rearranging
Cognitive_data$Tower_rearranging<-Data_cognitive$p21004_i2
table(Cognitive_data$Tower_rearranging,useNA = "ifany")
summary(Cognitive_data$Tower_rearranging)
Cognitive_data_final<-Cognitive_data
save(Cognitive_data_final,file = "original data/Cognitive_data_final.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 3 Blood samples arrangement ####
# #### Blood samples.csv ####
# Blood_samples<-fread("Blood samples.csv")
# colnames(Blood_samples)
# 
# colnames(Blood_samples)[2:31]<-c("Alanine_aminotransferase","Albumin","Alkaline_phosphatase",
#                                  "Apolipoprotein_A","Apolipoprotein_B","Aspartate_aminotransferase",
#                                  "C_reactive_protein","Calcium","Cholesterol","Creatinine","Cystatin_C",
#                                  "Direct_bilirubin","Gamma_glutamyltransferase","Glucose","HbA1c",
#                                  "HDL_cholesterol","IGF_1","LDL_direct","Lipoprotein_A","Oestradiol",
#                                  "Phosphate","Rheumatoid_factor","SHBG","Testosterone","Total_bilirubin",
#                                  "Total_protein","Triglycerides","Urate","Urea","Vitamin_D")
# 
# Blood_data_final<-Blood_samples
# save(Blood_data_final,file = "original data/Blood_data_final.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 4 Image arrangement ####
Data_participant<-fread("original data/Brain image.csv")
head(Data_participant)
Data_participant<-na.omit(Data_participant)
Image_data<-as.data.frame(Data_participant$eid)
colnames(Image_data)<-"eid"
#### 	Regional grey matter volumes (FAST) ####
#Frontal Pole
Image_data$Frontal_Pole_left<-Data_participant$p25782_i2
Image_data$Frontal_Pole_right<-Data_participant$p25783_i2

#Insular Cortex
Image_data$Insular_Cortex_left<-Data_participant$p25784_i2
Image_data$Insular_Cortex_right<-Data_participant$p25785_i2

#Superior Frontal Gyrus
Image_data$Superior_Frontal_Gyrus_left<-Data_participant$p25786_i2
Image_data$Superior_Frontal_Gyrus_right<-Data_participant$p25787_i2
#Middle Frontal Gyrus 
Image_data$Middle_Frontal_Gyrus_left<-Data_participant$p25788_i2
Image_data$Middle_Frontal_Gyrus_right<-Data_participant$p25789_i2
#Inferior Frontal Gyrus, pars triangularis 
Image_data$Inferior_Frontal_Gyrus_pars_triangularis_left<-Data_participant$p25790_i2
Image_data$Inferior_Frontal_Gyrus_pars_triangularis_right<-Data_participant$p25791_i2

#Inferior Frontal Gyrus, pars opercularis
Image_data$Inferior_Frontal_Gyrus_pars_opercularis_left<-Data_participant$p25792_i2
Image_data$Inferior_Frontal_Gyrus_pars_opercularis_right<-Data_participant$p25793_i2
#Precentral Gyrus
Image_data$Precentral_Gyrus_left<-Data_participant$p25794_i2
Image_data$Precentral_Gyrus_right<-Data_participant$p25795_i2
#Temporal Pole 
Image_data$Temporal_Pole_left<-Data_participant$p25796_i2
Image_data$Temporal_Pole_right<-Data_participant$p25797_i2
#Superior Temporal Gyrus, anterior division
Image_data$Superior_Temporal_Gyrus_anterior_division_left<-Data_participant$p25798_i2
Image_data$Superior_Temporal_Gyrus_anterior_division_right<-Data_participant$p25799_i2
#Superior Temporal Gyrus, posterior division
Image_data$Superior_Temporal_Gyrus_posterior_division_left<-Data_participant$p25800_i2
Image_data$Superior_Temporal_Gyrus_posterior_division_right<-Data_participant$p25801_i2
#Middle Temporal Gyrus, anterior division
Image_data$Middle_Temporal_Gyrus_anterior_division_left<-Data_participant$p25802_i2
Image_data$Middle_Temporal_Gyrus_anterior_division_right<-Data_participant$p25803_i2
#Middle Temporal Gyrus, posterior division
Image_data$Middle_Temporal_Gyrus_posterior_division_left<-Data_participant$p25804_i2
Image_data$Middle_Temporal_Gyrus_posterior_division_right<-Data_participant$p25805_i2
#Middle Temporal Gyrus, temporooccipital part
Image_data$Middle_Temporal_Gyrus_temporooccipital_part_left<-Data_participant$p25806_i2
Image_data$Middle_Temporal_Gyrus_temporooccipital_part_right<-Data_participant$p25807_i2
#Inferior Temporal Gyrus, anterior division
Image_data$Inferior_Temporal_Gyrus_anterior_division_left<-Data_participant$p25808_i2
Image_data$Inferior_Temporal_Gyrus_anterior_division_right<-Data_participant$p25809_i2
#Inferior Temporal Gyrus, posterior division
Image_data$Inferior_Temporal_Gyrus_posterior_division_left<-Data_participant$p25810_i2
Image_data$Inferior_Temporal_Gyrus_posterior_division_right<-Data_participant$p25811_i2
#Inferior Temporal Gyrus, temporooccipital part
Image_data$IInferior_Temporal_Gyrus_temporooccipital_left<-Data_participant$p25812_i2
Image_data$IInferior_Temporal_Gyrus_temporooccipital_right<-Data_participant$p25813_i2
#Postcentral Gyrus
Image_data$Postcentral_Gyrus_left<-Data_participant$p25814_i2
Image_data$Postcentral_Gyrus_right<-Data_participant$p25815_i2
#Superior Parietal Lobule
Image_data$Superior_Parietal_Lobule_left<-Data_participant$p25816_i2
Image_data$Superior_Parietal_Lobule_right<-Data_participant$p25817_i2
#Supramarginal Gyrus, anterior division
Image_data$Supramarginal_Gyrus_anterior_division_left<-Data_participant$p25818_i2
Image_data$Supramarginal_Gyrus_anterior_division_right<-Data_participant$p25819_i2
#Supramarginal Gyrus, posterior division
Image_data$Supramarginal_Gyrus_posterior_division_left<-Data_participant$p25820_i2
Image_data$Supramarginal_Gyrus_posterior_division_right<-Data_participant$p25821_i2
#Angular Gyrus
Image_data$Angular_Gyrus_left<-Data_participant$p25822_i2
Image_data$Angular_Gyrus_right<-Data_participant$p25823_i2
#Lateral Occipital Cortex, superior division
Image_data$Lateral_Occipital_Cortex_superior_division_left<-Data_participant$p25824_i2
Image_data$Lateral_Occipital_Cortex_superior_division_right<-Data_participant$p25825_i2
#Lateral Occipital Cortex, inferior division
Image_data$Lateral_Occipital_Cortex_inferior_division_left<-Data_participant$p25826_i2
Image_data$Lateral_Occipital_Cortex_inferior_division_right<-Data_participant$p25827_i2
#Intracalcarine Cortex
Image_data$Intracalcarine_Cortex_left<-Data_participant$p25828_i2
Image_data$Intracalcarine_Cortex_right<-Data_participant$p25829_i2
#Frontal Medial Cortex
Image_data$Frontal_Medial_Cortex_left<-Data_participant$p25830_i2
Image_data$Frontal_Medial_Cortex_right<-Data_participant$p25831_i2
#Juxtapositional Lobule Cortex
Image_data$Juxtapositional_Lobule_Cortex_left<-Data_participant$p25832_i2
Image_data$Juxtapositional_Lobule_Cortex_right<-Data_participant$p25833_i2
#Subcallosal Cortex 
Image_data$Subcallosal_Cortex_left<-Data_participant$p25834_i2
Image_data$Subcallosal_Cortex_right<-Data_participant$p25835_i2
#Paracingulate Gyrus
Image_data$Paracingulate_Gyrus_left<-Data_participant$p25836_i2
Image_data$Paracingulate_Gyrus_right<-Data_participant$p25837_i2
#Cingulate Gyrus, anterior division
Image_data$Cingulate_Gyrus_anterior_division_left<-Data_participant$p25838_i2
Image_data$Cingulate_Gyrus_anterior_division_right<-Data_participant$p25839_i2
#Cingulate Gyrus, posterior division
Image_data$Cingulate_Gyrus_posterior_division_left<-Data_participant$p25840_i2
Image_data$Cingulate_Gyrus_posterior_division_right<-Data_participant$p25841_i2
#Precuneous Cortex
Image_data$Precuneous_Cortex_left<-Data_participant$p25842_i2
Image_data$Precuneous_Cortex_right<-Data_participant$p25843_i2
#Cuneal Cortex
Image_data$Cuneal_Cortex_left<-Data_participant$p25844_i2
Image_data$Cuneal_Cortex_right<-Data_participant$p25845_i2
#Frontal Orbital Cortex
Image_data$Frontal_Orbital_Cortex_left<-Data_participant$p25846_i2
Image_data$Frontal_Orbital_Cortex_right<-Data_participant$p25847_i2
#Parahippocampal Gyrus, anterior division
Image_data$Parahippocampal_Gyrus_anterior_division_left<-Data_participant$p25848_i2
Image_data$Parahippocampal_Gyrus_anterior_division_right<-Data_participant$p25849_i2
#Parahippocampal Gyrus, posterior division
Image_data$Parahippocampal_Gyrus_posterior_division_left<-Data_participant$p25850_i2
Image_data$Parahippocampal_Gyrus_posterior_division_right<-Data_participant$p25851_i2
#Lingual Gyrus
Image_data$Lingual_Gyrus_left<-Data_participant$p25852_i2
Image_data$Lingual_Gyrus_right<-Data_participant$p25853_i2
#Temporal Fusiform Cortex, anterior division
Image_data$Fusiform_Cortex_anterior_division_left<-Data_participant$p25854_i2
Image_data$Fusiform_Cortex_anterior_division_right<-Data_participant$p25855_i2
#Temporal Fusiform Cortex, posterior division
Image_data$Fusiform_Cortex_posterior_division_left<-Data_participant$p25856_i2
Image_data$Fusiform_Cortex_posterior_division_right<-Data_participant$p25857_i2
#Temporal Occipital Fusiform Cortex
Image_data$Temporal_Occipital_Fusiform_Cortex_left<-Data_participant$p25858_i2
Image_data$Temporal_Occipital_Fusiform_Cortex_right<-Data_participant$p25859_i2
#Occipital Fusiform Gyrus
Image_data$Occipital_Fusiform_Gyrus_left<-Data_participant$p25860_i2
Image_data$Occipital_Fusiform_Gyrus_right<-Data_participant$p25861_i2
#Frontal Operculum Cortex
Image_data$Frontal_Operculum_Cortex_left<-Data_participant$p25862_i2
Image_data$Frontal_Operculum_Cortex_right<-Data_participant$p25863_i2
#Central Opercular Cortex
Image_data$Central_Opercular_Cortex_left<-Data_participant$p25864_i2
Image_data$Central_Opercular_Cortex_right<-Data_participant$p25865_i2
#Parietal Operculum Cortex
Image_data$Parietal_Operculum_Cortex_left<-Data_participant$p25866_i2
Image_data$Parietal_Operculum_Cortex_right<-Data_participant$p25867_i2
#Planum Polare 
Image_data$Planum_Polare_left<-Data_participant$p25868_i2
Image_data$Planum_Polare_right<-Data_participant$p25869_i2
#Heschl's Gyrus 
Image_data$Heschls_Gyrus_left<-Data_participant$p25870_i2
Image_data$Heschls_Gyrus_right<-Data_participant$p25871_i2
#Planum Temporale  
Image_data$Planum_Temporale_left<-Data_participant$p25872_i2
Image_data$Planum_Temporale_right<-Data_participant$p25873_i2
#Supracalcarine Cortex 
Image_data$Supracalcarine_Cortex_left<-Data_participant$p25874_i2
Image_data$Supracalcarine_Cortex_right<-Data_participant$p25875_i2
#Occipital Pole
Image_data$Occipital_Pole_left<-Data_participant$p25876_i2
Image_data$Occipital_Pole_right<-Data_participant$p25877_i2
#Thalamus
Image_data$Thalamus_left<-Data_participant$p25878_i2
Image_data$Thalamus_right<-Data_participant$p25879_i2
#Caudate
Image_data$Caudate_left<-Data_participant$p25880_i2
Image_data$Caudate_right<-Data_participant$p25881_i2
#Putamen
Image_data$Putamen_left<-Data_participant$p25882_i2
Image_data$Putamen_right<-Data_participant$p25883_i2
#Pallidum
Image_data$Pallidum_left<-Data_participant$p25884_i2
Image_data$Pallidum_right<-Data_participant$p25885_i2
#Hippocampus
Image_data$Hippocampus_left<-Data_participant$p25886_i2
Image_data$Hippocampus_right<-Data_participant$p25887_i2
#Amygdala
Image_data$Amygdala_left<-Data_participant$p25888_i2
Image_data$Amygdala_right<-Data_participant$p25889_i2
#Ventral Striatum
Image_data$Ventral_Striatum_left<-Data_participant$p25890_i2
Image_data$Ventral_Striatum_right<-Data_participant$p25891_i2
#Brain-Stem
Image_data$Brain_Stem<-Data_participant$p25892_i2
#I-IV Cerebellum
Image_data$I_IV_Cerebellum_left<-Data_participant$p25893_i2
Image_data$I_IV_Cerebellum_right<-Data_participant$p25894_i2
#V Cerebellum
Image_data$V_Cerebellum_left<-Data_participant$p25895_i2
Image_data$V_Cerebellum_right<-Data_participant$p25896_i2
#VI Cerebellum
Image_data$VI_Cerebellum_left<-Data_participant$p25897_i2
Image_data$VI_Cerebellum_vermis<-Data_participant$p25898_i2
Image_data$VI_Cerebellum_right<-Data_participant$p25899_i2
#Crus I Cerebellum
Image_data$Crus_I_Cerebellum_left<-Data_participant$p25900_i2
Image_data$Crus_I_Cerebellum_vermis<-Data_participant$p25901_i2
Image_data$Crus_I_Cerebellum_right<-Data_participant$p25902_i2
#Crus II Cerebellum
Image_data$Crus_II_Cerebellum_left<-Data_participant$p25903_i2
Image_data$Crus_II_Cerebellum_vermis<-Data_participant$p25904_i2
Image_data$Crus_II_Cerebellum_right<-Data_participant$p25905_i2
#VIIb Cerebellum
Image_data$VIIb_Cerebellum_left<-Data_participant$p25906_i2
Image_data$VIIb_Cerebellum_vermis<-Data_participant$p25907_i2
Image_data$VIIb_Cerebellum_right<-Data_participant$p25908_i2
#VIIIa Cerebellum
Image_data$VIIIa_Cerebellum_left<-Data_participant$p25909_i2
Image_data$VIIIa_Cerebellum_vermis<-Data_participant$p25910_i2
Image_data$VIIIa_Cerebellum_right<-Data_participant$p25911_i2
#VIIIb Cerebellum
Image_data$VIIIb_Cerebellum_left<-Data_participant$p25912_i2
Image_data$VIIIb_Cerebellum_vermis<-Data_participant$p25913_i2
Image_data$VIIIb_Cerebellum_right<-Data_participant$p25914_i2
#IX Cerebellum
Image_data$IX_Cerebellum_left<-Data_participant$p25915_i2
Image_data$IX_Cerebellum_vermis<-Data_participant$p25916_i2
Image_data$IX_Cerebellum_right<-Data_participant$p25917_i2
#X Cerebellum 
Image_data$X_Cerebellum_left<-Data_participant$p25918_i2
Image_data$X_Cerebellum_vermis<-Data_participant$p25919_i2
Image_data$X_Cerebellum_right<-Data_participant$p25920_i2

#### Subcortical volumes (FIRST) #### 
#thalamus 
Image_data$thalamus_left<-Data_participant$p25011_i2
Image_data$thalamus_right<-Data_participant$p25012_i2
#caudate
Image_data$caudate_left<-Data_participant$p25013_i2
Image_data$caudate_right<-Data_participant$p25014_i2
#putamen
Image_data$putamen_left<-Data_participant$p25015_i2
Image_data$putamen_right<-Data_participant$p25016_i2
#pallidum
Image_data$pallidum_left<-Data_participant$p25017_i2
Image_data$pallidum_right<-Data_participant$p25018_i2
#hippocampus
Image_data$hippocampus_left<-Data_participant$p25019_i2
Image_data$hippocampus_right<-Data_participant$p25020_i2
#amygdala
Image_data$amygdala_left<-Data_participant$p25021_i2
Image_data$amygdala_right<-Data_participant$p25022_i2
#accumbens
Image_data$accumbens_left<-Data_participant$p25023_i2
Image_data$accumbens_right<-Data_participant$p25024_i2


#Frontal Pole
Image_data$Frontal_Pole<-Data_participant$p25782_i2+Data_participant$p25783_i2
#Insular Cortex
Image_data$Insular_Cortex<-Data_participant$p25784_i2+Data_participant$p25785_i2
#Superior Frontal Gyrus
Image_data$Superior_Frontal_Gyrus<-Data_participant$p25786_i2+Data_participant$p25787_i2
#Middle Frontal Gyrus 
Image_data$Middle_Frontal_Gyrus<-Data_participant$p25788_i2+Data_participant$p25789_i2
#Inferior Frontal Gyrus
Image_data$Inferior_Frontal_Gyrus<-Data_participant$p25790_i2+Data_participant$p25791_i2+Data_participant$p25792_i2+Data_participant$p25793_i2
#Precentral Gyrus
Image_data$Precentral_Gyrus<-Data_participant$p25794_i2+Data_participant$p25795_i2
#Temporal Pole 
Image_data$Temporal_Pole<-Data_participant$p25796_i2+Data_participant$p25797_i2
#Superior Temporal Gyrus
Image_data$Superior_Temporal_Gyrus<-Data_participant$p25798_i2+Data_participant$p25799_i2+Data_participant$p25800_i2+Data_participant$p25801_i2
#Middle Temporal Gyrus
Image_data$Middle_Temporal_Gyrus<-Data_participant$p25802_i2+Data_participant$p25803_i2+Data_participant$p25804_i2+Data_participant$p25805_i2+Data_participant$p25806_i2+Data_participant$p25807_i2
#Inferior Temporal Gyrus
Image_data$Inferior_Temporal_Gyrus<-Data_participant$p25808_i2+Data_participant$p25809_i2+Data_participant$p25810_i2+Data_participant$p25811_i2+Data_participant$p25812_i2+Data_participant$p25813_i2
#Postcentral Gyrus
Image_data$Postcentral_Gyrus<-Data_participant$p25814_i2+Data_participant$p25815_i2
#Superior Parietal Lobule
Image_data$Superior_Parietal_Lobule<-Data_participant$p25816_i2+Data_participant$p25817_i2
#Supramarginal Gyrus
Image_data$Supramarginal_Gyrus_<-Data_participant$p25818_i2+Data_participant$p25819_i2+Data_participant$p25820_i2+Data_participant$p25821_i2
#Angular Gyrus
Image_data$Angular_Gyrus<-Data_participant$p25822_i2+Data_participant$p25823_i2
#Lateral Occipital Cortex
Image_data$Lateral_Occipital_Cortex<-Data_participant$p25824_i2+Data_participant$p25825_i2+Data_participant$p25826_i2+Data_participant$p25827_i2
#Intracalcarine Cortex
Image_data$Intracalcarine_Cortex<-Data_participant$p25828_i2+Data_participant$p25829_i2
#Frontal Medial Cortex
Image_data$Frontal_Medial_Cortex<-Data_participant$p25830_i2+Data_participant$p25831_i2
#Juxtapositional Lobule Cortex
Image_data$Juxtapositional_Lobule_Cortex<-Data_participant$p25832_i2+Data_participant$p25833_i2
#Subcallosal Cortex 
Image_data$Subcallosal_Cortex<-Data_participant$p25834_i2+Data_participant$p25835_i2
#Paracingulate Gyrus
Image_data$Paracingulate_Gyrus<-Data_participant$p25836_i2+Data_participant$p25837_i2
#Cingulate Gyrus
Image_data$Cingulate_Gyrus<-Data_participant$p25838_i2+Data_participant$p25839_i2+Data_participant$p25840_i2+Data_participant$p25841_i2
#Precuneous Cortex
Image_data$Precuneous_Cortex<-Data_participant$p25842_i2+Data_participant$p25843_i2
#Cuneal Cortex
Image_data$Cuneal_Cortex<-Data_participant$p25844_i2+Data_participant$p25845_i2
#Frontal Orbital Cortex
Image_data$Frontal_Orbital_Cortex<-Data_participant$p25846_i2+Data_participant$p25847_i2
#Parahippocampal Gyrus
Image_data$Parahippocampal_Gyrus<-Data_participant$p25848_i2+Data_participant$p25849_i2+Data_participant$p25850_i2+Data_participant$p25851_i2
#Lingual Gyrus 
Image_data$Lingual_Gyrus<-Data_participant$p25852_i2+Data_participant$p25853_i2
#Temporal Fusiform Cortex
Image_data$Fusiform_Cortex<-Data_participant$p25854_i2+Data_participant$p25855_i2+Data_participant$p25856_i2+Data_participant$p25857_i2
#Temporal Occipital Fusiform Cortex
Image_data$Temporal_Occipital_Fusiform_Cortex<-Data_participant$p25858_i2+Data_participant$p25859_i2
#Occipital Fusiform Gyrus
Image_data$Occipital_Fusiform_Gyrus<-Data_participant$p25860_i2+Data_participant$p25861_i2
#Frontal Operculum Cortex
Image_data$Frontal_Operculum_Cortex<-Data_participant$p25862_i2+Data_participant$p25863_i2
#Central Opercular Cortex
Image_data$Central_Opercular_Cortex<-Data_participant$p25864_i2+Data_participant$p25865_i2
#Parietal Operculum Cortex
Image_data$Parietal_Operculum_Cortex<-Data_participant$p25866_i2+Data_participant$p25867_i2
#Planum Polare 
Image_data$Planum_Polare<-Data_participant$p25868_i2+Data_participant$p25869_i2
#Heschl's Gyrus 
Image_data$Heschls_Gyrus<-Data_participant$p25870_i2+Data_participant$p25871_i2
#Planum Temporale  
Image_data$Planum_Temporale<-Data_participant$p25872_i2+Data_participant$p25873_i2
#Supracalcarine Cortex 
Image_data$Supracalcarine_Cortex<-Data_participant$p25874_i2+Data_participant$p25875_i2
#Occipital Pole
Image_data$Occipital_Pole<-Data_participant$p25876_i2+Data_participant$p25877_i2
#Thalamus
Image_data$Thalamus<-Data_participant$p25878_i2+Data_participant$p25879_i2
#Caudate
Image_data$Caudate<-Data_participant$p25880_i2+Data_participant$p25881_i2
#Putamen
Image_data$Putamen<-Data_participant$p25882_i2+Data_participant$p25883_i2
#Pallidum
Image_data$Pallidum<-Data_participant$p25884_i2+Data_participant$p25885_i2
#Hippocampus
Image_data$Hippocampus<-Data_participant$p25886_i2+Data_participant$p25887_i2
#Amygdala
Image_data$Amygdala<-Data_participant$p25888_i2+Data_participant$p25889_i2
#Ventral Striatum
Image_data$Ventral_Striatum<-Data_participant$p25890_i2+Data_participant$p25891_i2
#Brain-Stem
Image_data$Brain_Stem<-Data_participant$p25892_i2

#I-IV Cerebellum
Image_data$I_IV_Cerebellum<-Data_participant$p25893_i2+Data_participant$p25894_i2
#V Cerebellum
Image_data$V_Cerebellum<-Data_participant$p25895_i2+Data_participant$p25896_i2
#VI Cerebellum
Image_data$VI_Cerebellum<-Data_participant$p25897_i2+Data_participant$p25898_i2+Data_participant$p25899_i2
#Crus I Cerebellum
Image_data$Crus_I_Cerebellum<-Data_participant$p25900_i2+Data_participant$p25901_i2+Data_participant$p25902_i2
#Crus II Cerebellum
Image_data$Crus_II_Cerebellum<-Data_participant$p25903_i2+Data_participant$p25904_i2+Data_participant$p25905_i2
#VIIb Cerebellum
Image_data$VIIb_Cerebellum<-Data_participant$p25906_i2+Data_participant$p25907_i2+Data_participant$p25908_i2
#VIIIa Cerebellum
Image_data$VIIIa_Cerebellum<-Data_participant$p25909_i2+Data_participant$p25910_i2+Data_participant$p25911_i2
#VIIIb Cerebellum
Image_data$VIIIb_Cerebellum<-Data_participant$p25912_i2+Data_participant$p25913_i2+Data_participant$p25914_i2
#IX Cerebellum
Image_data$IX_Cerebellum<-Data_participant$p25915_i2+Data_participant$p25916_i2+Data_participant$p25917_i2
#X Cerebellum 
Image_data$X_Cerebellum<-Data_participant$p25918_i2+Data_participant$p25919_i2+Data_participant$p25920_i2

#### Subcortical volumes (FIRST) #### 
#thalamus 
Image_data$thalamus<-Data_participant$p25011_i2+Data_participant$p25012_i2
#caudate
Image_data$caudate<-Data_participant$p25013_i2+-Data_participant$p25014_i2
#putamen
Image_data$putamen<-Data_participant$p25015_i2+Data_participant$p25016_i2
#pallidum
Image_data$pallidum<-Data_participant$p25017_i2+Data_participant$p25018_i2
#hippocampus
Image_data$hippocampus<-Data_participant$p25019_i2+Data_participant$p25020_i2
#amygdala
Image_data$amygdala<-Data_participant$p25021_i2+Data_participant$p25022_i2
#accumbens
Image_data$accumbens<-Data_participant$p25023_i2+Data_participant$p25024_i2
Image_data_final<-Image_data
save(Image_data_final,file = "original data/Image_data_final.Rdata")


