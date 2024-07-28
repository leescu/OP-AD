# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
setwd("M:/OP&AZ/Cohort data")
library(data.table)
library(tidyverse)
library(mice)
Data_participant<-fread(file="original data/data_participant.csv")
head(Data_participant)
colnames(Data_participant)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 1 Covariates arrangement ####
Covariate_data<-as.data.frame(Data_participant$eid)
colnames(Covariate_data)<-"eid"
rownames(Covariate_data)<-Covariate_data$eid

#### Age ####
Covariate_data$Age<-Data_participant$p21003_i0
summary(Covariate_data$Age)

#### Sex ####
Covariate_data$Sex<-Data_participant$p31
table(Covariate_data$Sex)
Covariate_data$Sex[Covariate_data$Sex=="Female"]<-0
Covariate_data$Sex[Covariate_data$Sex=="Male"]<-1
Covariate_data$Sex<-as.factor(Covariate_data$Sex)
table(Covariate_data$Sex,useNA="ifany")

#### Ethnicity ####
table(Data_participant$p21000_i0,useNA="ifany")
Covariate_data$Ethnicity[Data_participant$p21000_i0=="White"|
                          Data_participant$p21000_i0=="British"|
                          Data_participant$p21000_i0=="Irish"|
                          Data_participant$p21000_i0=="Any other white background"]<-0

#Asian or Asian British
Covariate_data$Ethnicity[Data_participant$p21000_i0=="Asian or Asian British"|
                          Data_participant$p21000_i0=="Indian"|
                          Data_participant$p21000_i0=="Pakistani"|
                          Data_participant$p21000_i0=="Bangladeshi"|
                          Data_participant$p21000_i0=="Chinese"|
                          Data_participant$p21000_i0=="Any other Asian background"]<-1

#Black or Black British
Covariate_data$Ethnicity[Data_participant$p21000_i0=="Black or Black British"|
                          Data_participant$p21000_i0=="Caribbean"|
                          Data_participant$p21000_i0=="African"|
                          Data_participant$p21000_i0=="Any other Black background"]<-2

#mixed or others
Covariate_data$Ethnicity[Data_participant$p21000_i0=="Mixed"|
                          Data_participant$p21000_i0=="White and Asian"|
                          Data_participant$p21000_i0=="White and Black African"|
                          Data_participant$p21000_i0=="White and Black Caribbean"|
                          Data_participant$p21000_i0=="Other ethnic group"|
                          Data_participant$p21000_i0=="Any other mixed background"]<-3
Covariate_data$Ethnicity<-as.factor(Covariate_data$Ethnicity)
table(Covariate_data$Ethnicity,useNA="ifany")
#Nat Commun 2022 https://doi.org/10.1038/s41467-022-33628-8
#Lancet Public Health 2023 https://doi.org/10.1016/S2468-2667(23)00048-8

#### Education ####
table(Data_participant$p6138_i0,useNA="ifany")
Covariate_data$Education<-NA

#<10 years
Covariate_data$Education[str_which(Data_participant$p6138_i0,'None of the above') ] <- 0
#10-19 years
Covariate_data$Education[str_which(Data_participant$p6138_i0,'NVQ or HND or HNC or equivalent') ] <- 1
Covariate_data$Education[str_which(Data_participant$p6138_i0,'Other professional qualifications eg: nursing, teaching') ] <-1
Covariate_data$Education[str_which(Data_participant$p6138_i0,'A levels/AS levels or equivalent') ] <- 1
Covariate_data$Education[str_which(Data_participant$p6138_i0,'O levels/GCSEs or equivalent') ] <- 1
Covariate_data$Education[str_which(Data_participant$p6138_i0,'CSEs or equivalent') ] <-1
#≥20 years
Covariate_data$Education[str_which(Data_participant$p6138_i0,'College or University degree') ] <- 2
Covariate_data$Education<-as.factor(Covariate_data$Education)

table(Covariate_data$Education,useNA="ifany")

#Nature human behaviour 2023 https://doi.org/10.1038/s41562-023-01544-6
#Okbay, A. et al. Genome-wide association study identifies 74 loci associated with educational attainment. Nature 533, 539–542 (2016).
#Lee, J. J. et al. Gene discovery and polygenic prediction from a genome-wide association study of educational attainment in 1.1 million individuals. Nat. Genet. 50, 1112–1121 (2018).
#Rietveld, C. A. et al. GWAS of 126,559 individuals identifies genetic variants associated with educational attainment. Science 340, 1467–1471 (2013).

#### Townsend deprivation index ####
colnames(Data_participant)
summary(Data_participant$p189_i0)
Covariate_data$TDI<-Data_participant$p189_i0


#### Smoking status ####
table(Data_participant$p20116_i0,useNA="ifany")
Covariate_data$Smoke[Data_participant$p20116_i0=="Current"]<-2
Covariate_data$Smoke[Data_participant$p20116_i0=="Never"]<-0
Covariate_data$Smoke[Data_participant$p20116_i0=="Previous"]<-1
Covariate_data$Smoke<-as.factor(Covariate_data$Smoke)
table(Covariate_data$Smoke,useNA="ifany")


#### Alcohol drinker status ####
colnames(Data_participant)
Alcohol_data<-Data_participant[,c("eid","p20117_i0","p1558_i0","p4407_i0",
                                  "p4418_i0","p4429_i0","p4440_i0",
                                  "p4451_i0","p4462_i0", "p1568_i0",
                                  "p1578_i0","p1588_i0","p1598_i0",
                                  "p1608_i0","p5364_i0")]
Alcohol_data[Alcohol_data==""] <- 0
Alcohol_data[Alcohol_data=="Do not know"] <- 0	
Alcohol_data[Alcohol_data=="Prefer not to answer"] <- 0	

#Standard glass of red/white/rosé wine (175ml, ABV 12%) 2.1 units
Alcohol_data$`red_wine_low` =as.numeric(Alcohol_data$p4407_i0)*2.1
Alcohol_data$`champagne/white wine low`=as.numeric(Alcohol_data$p4418_i0)*2.1
#Pint of lower-strength lager/beer/cider (ABV 3.6%) 2 units
#Pint of higher-strength lager/beer/cider (ABV 5.2%) 3 units
Alcohol_data$`beer/cider low` =as.numeric(Alcohol_data$p4429_i0)*2.5
#Single small shot of spirits* (25ml, ABV 40%) 1 unit
Alcohol_data$`spirits low` =as.numeric(Alcohol_data$p4440_i0)*1
#Small glass of red/white/rosé wine (125ml, ABV 12%) 1.5 units
Alcohol_data$`fortified wine low`=as.numeric(Alcohol_data$p4451_i0)*1.5
#Alcopop (275ml, ABV 5.5%) 1.5 units
Alcohol_data$`other alcoholic low` =as.numeric(Alcohol_data$p4462_i0)*1.5
#Standard glass of red/white/rosé wine (175ml, ABV 12%) 2.1 units
Alcohol_data$`red wine high` =as.numeric(Alcohol_data$p1568_i0)*2.1
Alcohol_data$`champagne/white wine high`=as.numeric(Alcohol_data$p1578_i0)*2.1
#Pint of lower-strength lager/beer/cider (ABV 3.6%) 2 units
#Pint of higher-strength lager/beer/cider (ABV 5.2%) 3 units
Alcohol_data$`beer/cider high`=as.numeric(Alcohol_data$p1588_i0)*2.5
#Single small shot of spirits* (25ml, ABV 40%) 1 unit
Alcohol_data$`spirits high` =as.numeric(Alcohol_data$p1598_i0)*1
#Small glass of red/white/rosé wine (125ml, ABV 12%) 1.5 units
Alcohol_data$`fortified wine high` =as.numeric(Alcohol_data$p1608_i0)*1.5
#Alcopop (275ml, ABV 5.5%) 1.5 units
Alcohol_data$`other alcoholic high`=as.numeric(Alcohol_data$p5364_i0)*1.5  
table(Alcohol_data$p1558_i0)
Alcohol_data$Alcohol_low_value<-0
Alcohol_data$Alcohol_low_value[(Alcohol_data$p1558_i0=='Special occasions only')|
                    (Alcohol_data$p1558_i0=='One to three times a month')]<-(8*7/30)
Alcohol_data$Alcohol_high_value<-0
Alcohol_data$Alcohol_high_value[(Alcohol_data$p1558_i0=='Once or twice a week')|
                                (Alcohol_data$p1558_i0=='Daily or almost daily')|
                                (Alcohol_data$p1558_i0=='Three or four times a week')]<-8
Alcohol_data$Alcohol_intake=(Alcohol_data$`red_wine_low`+Alcohol_data$`champagne/white wine low`+
                               Alcohol_data$`beer/cider low`+Alcohol_data$`spirits low`+
                               Alcohol_data$`fortified wine low`+Alcohol_data$`other alcoholic low`)*Alcohol_data$Alcohol_low_value+
                            (Alcohol_data$`red wine high`+Alcohol_data$`champagne/white wine high`+
                               Alcohol_data$`beer/cider high`+Alcohol_data$`spirits high`+
                               Alcohol_data$`fortified wine high`+Alcohol_data$`other alcoholic high`)*Alcohol_data$Alcohol_high_value

Alcohol_data$sex<-Data_participant$p31
table(Alcohol_data$p1558_i0)

#Never alcohol use

summary(Alcohol_data$Alcohol_intake)
#light-to-moderate alcohol use
Alcohol_data$Alcohol[(((Alcohol_data$Alcohol_intake<196)&(Alcohol_data$sex=="Male"))|
                        ((Alcohol_data$Alcohol_intake<98)&(Alcohol_data$sex=="Female")))] =1
#Heavy alcohol use
Alcohol_data$Alcohol[(((Alcohol_data$Alcohol_intake>=196)&(Alcohol_data$sex=="Male"))|
                       ((Alcohol_data$Alcohol_intake>=98)&(Alcohol_data$sex=="Female")))] =2
Alcohol_data$Alcohol[(Alcohol_data$p1558_i0=='Never')]=0
#NA
Alcohol_data$Alcohol[(Alcohol_data$p1558_i0==0)]<-NA
Covariate_data$Alcohol<-Alcohol_data$Alcohol
table(Covariate_data$Alcohol,useNA="ifany")

#Nauffal, V., Di Achille, P., Klarqvist, M.D.R. et al. Genetics of myocardial interstitial fibrosis in the human heart and association with disease. Nat Genet (2023). https://doi.org/10.1038/s41588-023-01371-5
#https://www.nhs.uk/live-well/alcohol-advice/calculating-alcohol-units/
####  Physical activity ####
PA_data<-Data_participant[,c("eid","p884_i0","p894_i0","p904_i0","p914_i0")]
PA_data[PA_data==""] <- NA
PA_data[PA_data=="Do not know"] <- NA	
PA_data[PA_data=="Prefer not to answer"] <- NA
PA_data$PA<-"Unknow"
PA_data$PA[is.na(PA_data$p884_i0)&is.na(PA_data$p894_i0)&is.na(PA_data$p904_i0)&is.na(PA_data$p914_i0)]<-NA
table(PA_data$PA,useNA="ifany")
PA_data[is.na(PA_data)]<-0
PA_data$PA_na[PA_data$PA==0]<-1
#Regular physical activity
PA_data$PA[(as.numeric(PA_data$p884_i0)*as.numeric(PA_data$p894_i0))>=150|as.numeric(PA_data$p884_i0)>=5|
                  (as.numeric(PA_data$p904_i0)*as.numeric(PA_data$p914_i0))>=75|as.numeric(PA_data$p904_i0)>=1]<-0
#Irregular physical activity
PA_data$PA[(as.numeric(PA_data$p884_i0)*as.numeric(PA_data$p894_i0))<150&as.numeric(PA_data$p884_i0)<5&
                   (as.numeric(PA_data$p904_i0)*as.numeric(PA_data$p914_i0))<75&as.numeric(PA_data$p904_i0)<1]<-1
#NA
PA_data$PA[PA_data$PA_na==1]<-NA
PA_data<-PA_data[,c("eid","PA")]
Covariate_data<-merge(Covariate_data,PA_data,by = "eid",all.x = T)
table(Covariate_data$PA,useNA="ifany")

#Lourida I, Hannon E, Littlejohns TJ, et al. Association of lifestyle and genetic risk with incidence of Covariate [published online July 14, 2019]. JAMA. doi:10.1001/jama.2019.9879

#### Body mass index (BMI) ####
summary(Data_participant$p21001_i0)
Covariate_data$BMI<-Data_participant$p21001_i0


#### Blood pressure ####
BP_data<-Data_participant[,c("p4080_i0_a0","p4080_i0_a1",
                             "p4079_i0_a0","p4079_i0_a1",
                             "p94_i0_a0","p94_i0_a1",
                             "p93_i0_a0","p93_i0_a1")]
BP_au_Systolic<-PA_data<-Data_participant[,c("p4080_i0_a0","p4080_i0_a1")]
BP_au_Diastolic<-PA_data<-Data_participant[,c("p4079_i0_a0","p4079_i0_a1")]
BP_ma_Systolic<-PA_data<-Data_participant[,c("p93_i0_a0","p93_i0_a1")]
BP_ma_Diastolic<-PA_data<-Data_participant[,c("p94_i0_a0","p94_i0_a1")]

BP_data$Systolic_au<-(rowSums(BP_au_Systolic, na.rm=F)/2)
BP_data$Diastolic_au<-(rowSums(BP_au_Diastolic, na.rm=F)/2)
BP_data$Systolic_ma<-(rowSums(BP_ma_Systolic, na.rm=F)/2)
BP_data$Diastolic_ma<-(rowSums(BP_ma_Diastolic, na.rm=F)/2)
Covariate_data$Systolic=ifelse(is.na(BP_data$Systolic_au),BP_data$Systolic_ma,BP_data$Systolic_au)
Covariate_data$Diastolic=ifelse(is.na(BP_data$Diastolic_au),BP_data$Diastolic_ma,BP_data$Diastolic_au)

summary(Covariate_data$Diastolic)



#### Diabetes mellitus ####
table(Data_participant$p2443_i0,useNA="ifany")
table(Data_participant$p6177_i0,useNA="ifany")
Covariate_data$Diabetes[Data_participant$p2443_i0=="No"]<-0
Covariate_data$Diabetes[str_which(Data_participant$p6177_i0,'Insulin')]<-1
Covariate_data$Diabetes[Data_participant$p2443_i0=="Yes"]<-1
table(Covariate_data$Diabetes,useNA="ifany")
#Covariate 1.pdf

#### Medications ####
Medication<-fread("Medication.csv")

colnames(Medication)
Medications<-Medication[,c(1,2,4,6,8)]
colnames(Medications)<-c("eid","Med1","Med2","Med3","Med4")
Medications[Medications==""] <- NA
Medications[Medications=="Do not know"] <- NA	
Medications[Medications=="Prefer not to answer"] <- NA
table(Medications$Med1,useNA="ifany")
table(Medications$Med2,useNA="ifany")
table(Medications$Med3,useNA="ifany")
table(Medications$Med4,useNA="ifany")
Medications$M1<-1
Medications$M1[Medications$Med1=="None of the above"]<-0
Medications$M1[is.na(Medications$Med1)]<-NA
table(Medications$M1,useNA="ifany")
Medications$M2<-1
Medications$M2[Medications$Med2=="None of the above"]<-0
Medications$M2[is.na(Medications$Med2)]<-NA
table(Medications$M2,useNA="ifany")
Medications$M3<-1
Medications$M3[Medications$Med3=="No"]<-0
Medications$M3[is.na(Medications$Med3)]<-NA
table(Medications$M3,useNA="ifany")
#Medications$M4<-1
#Medications$M4[Medications$Med4=="None of the above"]<-0
#Medications$M4[is.na(Medications$Med4)]<-NA
#table(Medications$M4,useNA="ifany")
Medications$SEX=Data_participant$p31
table(Medications$SEX,Medications$M2)
Medications$Medication[(Medications$M1==0&Medications$M3==0)|(Medications$M2==0&Medications$M3==0)]<-0
Medications$Medication[Medications$M1==1|Medications$M2==1|Medications$M3==1]<-1
table(Medications$Medication,useNA="ifany")
Medications<-Medications[,c("eid","Medication")]
Covariate_data<-merge(Covariate_data,Medications,by = "eid",all.x = T)

#### as.factor ####
Covariate_data$Sex<-as.factor(Covariate_data$Sex)
Covariate_data$Ethnicity<-as.factor(Covariate_data$Ethnicity)
Covariate_data$Education<-as.factor(Covariate_data$Education)
Covariate_data$PA<-as.factor(Covariate_data$PA)
Covariate_data$Smoke<-as.factor(Covariate_data$Smoke)
Covariate_data$Alcohol<-as.factor(Covariate_data$Alcohol)
Covariate_data$Diabetes<-as.factor(Covariate_data$Diabetes)
Covariate_data$Medication<-as.factor(Covariate_data$Medication)
Covariate_data_orginal<-Covariate_data
#Depression
#Depression<-fread("Depression.csv")

#colnames(Depression)
#Depressions<-Depression[,c(1,2,4,6,8)]
#Depressions[Depressions==""] <- NA
#Depressions[Depressions=="Do not know"] <- NA	
#Depressions[Depressions=="Prefer not to answer"] <- NA
#Depressions[Depressions=="Nearly every day"] <- 3	
#Depressions[Depressions=="More than half the days"] <- 2
# Depressions[Depressions=="Several days"] <-1
# Depressions[Depressions=="Not at all"] <- 0
# colnames(Depressions)<-c("eid","Dep1","Dep2","Dep3","Dep4")
# table(Depressions$Dep1,useNA="ifany")
# table(Depressions$Dep2,useNA="ifany")
# table(Depressions$Dep3,useNA="ifany")
# table(Depressions$Dep4,useNA="ifany")
# rownames(Depressions)<-Depressions$eid
# Depressions$eid<-NULL
# Depressions$Dep1<-as.numeric(Depressions$Dep1)
# Depressions$Dep2<-as.numeric(Depressions$Dep2)
# Depressions$Dep3<-as.numeric(Depressions$Dep3)
# Depressions$Dep4<-as.numeric(Depressions$Dep4)
# Dep1<-Depressions[,c(1,2)]
# Dep2<-Depressions[,c(3,4)]
# Dep1$Depression_score<-rowSums(Dep1,na.rm = TRUE )
# Dep2$Depression_score<-rowSums(Dep2,na.rm = TRUE )
# Dep1$eid<-rownames(Depressions)
# Dep2$eid<-rownames(Depressions)
# 
# Dep1$Depression[Dep1$Depression_score<=2]<-0
# Dep1$Depression[is.na(Dep1$Dep1)|is.na(Dep1$Dep2)]<-NA
# Dep1$Depression[Dep1$Depression_score>2]<-1
# table(Dep1$Depression,useNA="ifany")
# 
# Dep2$Depression[Dep2$Depression_score<=2]<-0
# Dep2$Depression[is.na(Dep2$Dep3)|is.na(Dep2$Dep4)]<-NA
# Dep2$Depression[Dep2$Depression_score>2]<-1
# table(Dep2$Depression,useNA="ifany")
# 
# Depressions$Depression[Dep1$Depression==0&Dep2$Depression==0]<-0
# Depressions$Depression[is.na(Dep1$Depression)|is.na(Dep2$Depression)]<-NA
# Depressions$Depression[Dep1$Depression==1|Dep2$Depression==1]<-1
# table(Depressions$Depression,useNA="ifany")
# Depressions$eid<-rownames(Depressions)
# Depressions<-Depressions[,c("eid","Depression")]
# Covariate_data<-merge(Covariate_data,Depressions,by = "eid",all.x = T)
# Covariate_data_orginal<-Covariate_data
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 2 Orginal cohort data define ####

#### TDI quantile ####
Quantile<-quantile(Covariate_data_orginal$TDI, probs=c(0.25,0.5,0.75),  na.rm=TRUE)
Covariate_data_orginal$TDI_quantile[Covariate_data_orginal$TDI<=Quantile[1]]<-1
Covariate_data_orginal$TDI_quantile[Covariate_data_orginal$TDI<=Quantile[2]&Covariate_data_orginal$TDI>Quantile[1]]<-2
Covariate_data_orginal$TDI_quantile[Covariate_data_orginal$TDI<=Quantile[3]&Covariate_data_orginal$TDI>Quantile[2]]<-3
Covariate_data_orginal$TDI_quantile[Covariate_data_orginal$TDI>Quantile[3]]<-4
Covariate_data_orginal$TDI_quantile<-as.factor(Covariate_data_orginal$TDI_quantile)
table(Covariate_data_orginal$TDI_quantile,useNA="ifany")
#Lancet Public Health 2023 https://doi.org/10.1016/S2468-2667(23)00048-8
#BMJ 2021 http://dx.doi.org/10.1136/bmj.n604
#Townsend P, Phillimore P, Beattie A. Health and deprivation: inequality and the North. Croom Helm, 1988.

#### BMI status ####
#Underweight/normal weight (<25)
Covariate_data_orginal$BMI_status[Data_participant$p21001_i0<25] <- 0
#Overweight (25-30)
Covariate_data_orginal$BMI_status[Data_participant$p21001_i0>=25&Data_participant$p21001_i0<30] <-1
#Obese (≥30)
Covariate_data_orginal$BMI_status[Data_participant$p21001_i0>=30] <- 2
table(Covariate_data_orginal$BMI_status,useNA="ifany")
#Covariate 1.pdf

#### Hypertension ####
#table(Data_participant$p6177_i0,useNA="ifany")
Covariate_data_orginal$Hypertension[(Covariate_data_orginal$Systolic<140&Covariate_data_orginal$Diastolic<90)]<-0
Covariate_data_orginal$Hypertension[Covariate_data_orginal$Systolic>=140|Covariate_data_orginal$Diastolic>=90]<-1
table(Covariate_data_orginal$Hypertension,useNA="ifany")

#### as.factor ####
Covariate_data_orginal$TDI_quantile<-as.factor(Covariate_data_orginal$TDI_quantile)
Covariate_data_orginal$BMI_status<-as.factor(Covariate_data_orginal$BMI_status)
Covariate_data_orginal$Hypertension<-as.factor(Covariate_data_orginal$Hypertension)
save(Covariate_data_orginal,file = "original data/Covariate_data_orginal.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 3 Complete cohort data define ####
Covariate_data_complete<-na.omit(Covariate_data)
Quantile<-quantile(Covariate_data_complete$TDI, probs=c(0.25,0.5,0.75),  na.rm=TRUE)
Covariate_data_complete$TDI_quantile[Covariate_data_complete$TDI<=Quantile[1]]<-1
Covariate_data_complete$TDI_quantile[Covariate_data_complete$TDI<=Quantile[2]&Covariate_data_complete$TDI>Quantile[1]]<-2
Covariate_data_complete$TDI_quantile[Covariate_data_complete$TDI<=Quantile[3]&Covariate_data_complete$TDI>Quantile[2]]<-3
Covariate_data_complete$TDI_quantile[Covariate_data_complete$TDI>Quantile[3]]<-4
Covariate_data_complete$TDI_quantile<-as.factor(Covariate_data_complete$TDI_quantile)
table(Covariate_data_complete$TDI_quantile,useNA="ifany")
#Lancet Public Health 2023 https://doi.org/10.1016/S2468-2667(23)00048-8
#BMJ 2021 http://dx.doi.org/10.1136/bmj.n604
#Townsend P, Phillimore P, Beattie A. Health and deprivation: inequality and the North. Croom Helm, 1988.

#### BMI status ####
#Underweight/normal weight (<25)
Covariate_data_complete$BMI_status[Covariate_data_complete$BMI<25] <- 0
#Overweight (25-30)
Covariate_data_complete$BMI_status[Covariate_data_complete$BMI<30] <-1
#Obese (≥30)
Covariate_data_complete$BMI_status[Covariate_data_complete$BMI>=30] <- 2
table(Covariate_data_complete$BMI_status,useNA="ifany")
#Covariate 1.pdf

#### Hypertension ####
#table(Data_participant$p6177_i0,useNA="ifany")
Covariate_data_complete$Hypertension[(Covariate_data_complete$Systolic<140&Covariate_data_complete$Diastolic<90)]<-0
Covariate_data_complete$Hypertension[Covariate_data_complete$Systolic>=140|Covariate_data_complete$Diastolic>=90]<-1
table(Covariate_data_complete$Hypertension,useNA="ifany")

#### as.factor ####
Covariate_data_complete$TDI_quantile<-as.factor(Covariate_data_complete$TDI_quantile)
Covariate_data_complete$BMI_status<-as.factor(Covariate_data_complete$BMI_status)
Covariate_data_complete$Hypertension<-as.factor(Covariate_data_complete$Hypertension)

save(Covariate_data_complete,file="original data/Covariate_data_complete.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 4 Interpolation cohort data define ####
load(file="original data/Covariate_data_complete.Rdata")
colnames(Covariate_data)
head(Covariate_data[,2:14])


mice::md.pattern(Covariate_data[,2:14])
sapply(data, function(x) sum(is.na(Covariate_data[,2:14])))
miss <- function(x){sum(is.na(x))/length(x)*100}
TableS1<-as.data.frame(apply(Covariate_data[,2:14],2,miss))
colnames(TableS1)<-"result"
TableS1$result<-round(TableS1$result,3)
rownames(TableS1)
Table_S1<-cbind(c("Age","Sex","Ethnicity","Education levels","TDI","Smoking status","Alcohol consumption",
                  "Physical activity","Body mass index, kg/m2","Systolic",
                  "Diastolic","Diabetes","Medication"),
                c("continuous", 
                  "categorical","categorical","categorical",
                  "continuous","categorical","categorical","categorical",
                  "continuous",
                  "continuous","continuous","categorical","categorical"),
                TableS1[c("Age","Sex","Ethnicity","Education","TDI","Smoke","Alcohol","PA",
                          "BMI","Systolic","Diastolic","Diabetes","Medication"),1])
Table_S1[Table_S1==0]<-"No missing values"
Table_S1
write.table(Table_S1,sep = ",",file ="original data/Supplementary Table 1.csv" ,row.names =F,col.names =F )




Mice_data<-mice(Covariate_data[,2:14],  
           m = 5,  
           maxit = 5,  
           printFlag = TRUE, 
           seed = 1)
save(Mice_data,file="result data/Mice_data.Rdata")
load(file="result data/Mice_data.Rdata")
Interpolation_data<-as.data.frame(Covariate_data$eid)
colnames(Interpolation_data)<-"eid"
Interpolation_data[,2:14]<- complete(Mice_data,5)

#### TDI quantile ####
Quantile<-quantile(Interpolation_data$TDI, probs=c(0.25,0.5,0.75),  na.rm=TRUE)
Interpolation_data$TDI_quantile[Interpolation_data$TDI<=Quantile[1]]<-1
Interpolation_data$TDI_quantile[Interpolation_data$TDI<=Quantile[2]&Interpolation_data$TDI>Quantile[1]]<-2
Interpolation_data$TDI_quantile[Interpolation_data$TDI<=Quantile[3]&Interpolation_data$TDI>Quantile[2]]<-3
Interpolation_data$TDI_quantile[Interpolation_data$TDI>Quantile[3]]<-4
Interpolation_data$TDI_quantile<-as.factor(Interpolation_data$TDI_quantile)
table(Interpolation_data$TDI_quantile,useNA="ifany")
#Lancet Public Health 2023 https://doi.org/10.1016/S2468-2667(23)00048-8
#BMJ 2021 http://dx.doi.org/10.1136/bmj.n604
#Townsend P, Phillimore P, Beattie A. Health and deprivation: inequality and the North. Croom Helm, 1988.

#### BMI status ####
#Underweight/normal weight (<25)
Interpolation_data$BMI_status[Interpolation_data$BMI<25] <- 0
#Overweight (25-30)
Interpolation_data$BMI_status[Interpolation_data$BMI>=25&Interpolation_data$BMI<30] <-1
#Obese (≥30)
Interpolation_data$BMI_status[Interpolation_data$BMI>=30] <- 2
table(Interpolation_data$BMI_status,useNA="ifany")
#Covariate 1.pdf

#### Hypertension ####
table(Data_participant$p6177_i0,useNA="ifany")
Interpolation_data$Hypertension[(Interpolation_data$Systolic<140&Interpolation_data$Diastolic<90)]<-0
Interpolation_data$Hypertension[Interpolation_data$Systolic>=140|Interpolation_data$Diastolic>=90]<-1
table(Interpolation_data$Hypertension,useNA="ifany")

#### as.factor ####
Interpolation_data$TDI_quantile<-as.factor(Interpolation_data$TDI_quantile)
Interpolation_data$BMI_status<-as.factor(Interpolation_data$BMI_status)
Interpolation_data$Hypertension<-as.factor(Interpolation_data$Hypertension)
Covariate_data_interpolation<-Interpolation_data

save(Covariate_data_interpolation,file="original data/Covariate_data_interpolation.Rdata")


