# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
setwd("M:/OP&AZ/Cohort data")
library(data.table)
library(dplyr)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 1 Oral data arrangement ####
Data_participant<-fread("original data/data_participant.csv")
Oral_data<-as.data.frame(Data_participant$eid)
colnames(Oral_data)<-"eid"
Oral_data$oral<-Data_participant$p6149_i0
Oral_data<-subset(Oral_data,oral!=""&oral!=" Prefer not to answer"&oral!="Prefer not to answer")
table(Oral_data$oral,useNA='ifany')
Oral_data$Mouth_ulcers<-grepl("*Mouth ulcers*",Oral_data$oral)
Oral_data$Painful_gums<-grepl("*Painful gums*",Oral_data$oral)
Oral_data$Bleeding_gums<-grepl("*Bleeding gums*",Oral_data$oral)
Oral_data$Loose_teeth<-grepl("*Loose teeth*",Oral_data$oral)
Oral_data$Toothache<-grepl("*Toothache*",Oral_data$oral)
Oral_data$Dentures<-grepl("*Dentures*",Oral_data$oral)
Oral_data[Oral_data==T]<-1
Oral_data[Oral_data==F]<-0
Oral_data$Periodontal_disease<-0
Oral_data$Periodontal_disease[Oral_data$Painful_gums==1|Oral_data$Bleeding_gums==1|Oral_data$Loose_teeth]<-1
Oral_data$Painful_disease<-0
Oral_data$Painful_disease[Oral_data$Painful_gums==1|Oral_data$Toothache==1]<-1
Oral_data$Painful_disease_two<-0
Oral_data$Painful_disease_two[Oral_data$Painful_gums==1&Oral_data$Toothache==1]<-1
Oral_data$Painful_disease_counts<-0
Oral_data$Painful_disease_counts[Oral_data$Painful_gums==1|Oral_data$Toothache==1]<-1
Oral_data$Painful_disease_counts[Oral_data$Painful_gums==1&Oral_data$Toothache==1]<-2
table(Oral_data$Painful_disease_counts)
Oral_data$Painful_disease<-as.factor(Oral_data$Painful_disease)
Oral_data$Periodontal_disease<-as.factor(Oral_data$Periodontal_disease)
Oral_data$Painful_gums<-as.factor(Oral_data$Painful_gums)
Oral_data$Toothache<-as.factor(Oral_data$Toothache)
Oral_data_final<-Oral_data
table(Oral_data$Periodontal_disease)
save(Oral_data_final,file = "original data/Oral_data_final.Rdata")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 2 Merge data  ####
setwd("M:/OP&AZ/Cohort data/original data")
load(file="Covariate_data_interpolation.Rdata")
load(file="Covariate_data_complete.Rdata")
load(file="Covariate_data_orginal.Rdata")
load(file="Date_data_final.Rdata")
load(file="Oral_data_final.Rdata")
table(Oral_data_final$Painful_disease,useNA="ifany")
load(file="Cognitive_data_final.Rdata")
load(file="Image_data_final.Rdata")
Data_final<-merge(Oral_data_final,Date_data_final,by = "eid",all= F)
Data_final<-merge(Data_final,Cognitive_data_final,by = "eid",all.x= T)
Data_final<-merge(Data_final,Image_data_final,by = "eid",all.x= T)
Interpolation_data<-merge(Data_final,Covariate_data_interpolation,by = "eid",all= F)
Complete_data<-merge(Data_final,Covariate_data_complete,by = "eid",all= F)
Orginal_data<-merge(Data_final,Covariate_data_orginal,by = "eid",all= F)

Interpolation_data<-subset(Interpolation_data,Alzheimer_dementia!=2)
Complete_data<-subset(Complete_data,Alzheimer_dementia!=2)
Orginal_data<-subset(Orginal_data,Alzheimer_dementia!=2)
save(Interpolation_data,file="Interpolation_data.Rdata")
save(Complete_data,file="Complete_data.Rdata")
save(Orginal_data,file="Orginal_data.Rdata")
rm(list = ls())


setwd("M:/OP&AZ/Cohort data")
library(data.table)
library(dplyr)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 1 Oral data all arrangement ####
Oral<-fread("original data/Oral data.csv")
Oral_data<-as.data.frame(Oral$eid)
colnames(Oral_data)<-"eid"
Oral_data$oral1<-Oral$p6149_i0
Oral_data$oral2<-Oral$p6149_i1
Oral_data$oral3<-Oral$p6149_i2
Oral_data$oral4<-Oral$p6149_i3
Oral_data<-subset(Oral_data,oral1!=""&oral1!=" Prefer not to answer"&oral1!="Prefer not to answer")
Oral_data[Oral_data==" Prefer not to answer"]<-NA
Oral_data[Oral_data=="Prefer not to answer"]<-NA
Oral_data[Oral_data==""]<-NA

Oral_data$Painful_gums1<-grepl("*Painful gums*",Oral_data$oral1)
Oral_data$Toothache1<-grepl("*Toothache*",Oral_data$oral1)

Oral_data$Painful_gums2<-grepl("*Painful gums*",Oral_data$oral2)
Oral_data$Toothache2<-grepl("*Toothache*",Oral_data$oral2)

Oral_data$Painful_gums3<-grepl("*Painful gums*",Oral_data$oral3)
Oral_data$Toothache3<-grepl("*Toothache*",Oral_data$oral3)

Oral_data$Painful_gums4<-grepl("*Painful gums*",Oral_data$oral4)
Oral_data$Toothache4<-grepl("*Toothache*",Oral_data$oral4)

Oral_data[Oral_data==T]<-1
Oral_data[Oral_data==F]<-0
Oral_data$Painful_disease1<-0
Oral_data$Painful_disease1[Oral_data$Painful_gums1==1|Oral_data$Toothache1==1]<-1

Oral_data$Painful_disease2<-0
Oral_data$Painful_disease2[Oral_data$Painful_gums2==1|Oral_data$Toothache2==1]<-1
Oral_data$Painful_disease3<-0
Oral_data$Painful_disease3[Oral_data$Painful_gums3==1|Oral_data$Toothache3==1]<-1
Oral_data$Painful_disease4<-0
Oral_data$Painful_disease4[Oral_data$Painful_gums4==1|Oral_data$Toothache4==1]<-1

Oral_data1<-as.data.frame(Oral$eid)
colnames(Oral_data1)<-"eid"
Oral_data1$oral22<-Oral$p6149_i1
Oral_data1$oral33<-Oral$p6149_i2
Oral_data1$oral44<-Oral$p6149_i3
Oral_data1[Oral_data1==" Prefer not to answer"]<-NA
Oral_data1[Oral_data1=="Prefer not to answer"]<-NA
Oral_data1[Oral_data1==""]<-NA
Oral_dataall<-merge(Oral_data,Oral_data1,by = "eid",all.x = T)
Oral_dataall$Painful_disease2[is.na(Oral_dataall$oral22)]<-NA
Oral_dataall$Painful_disease3[is.na(Oral_dataall$oral33)]<-NA
Oral_dataall$Painful_disease4[is.na(Oral_dataall$oral44)]<-NA
load(file="original data/Interpolation_data.Rdata")
EID<-Interpolation_data[,c("eid","Alzheimer_dementia")]
Pain_data<-merge(Oral_dataall,EID,by = 'eid',all.y = T)
Pain_data$PD<-NULL
#### Controlled pain ####
#no NA
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==1&Pain_data$Painful_disease3==1&Pain_data$Painful_disease4==0]<-"B"
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==1&Pain_data$Painful_disease3==0&Pain_data$Painful_disease4==0]<-"A"
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==0&Pain_data$Painful_disease3==0&Pain_data$Painful_disease4==0]<-"A"
#1 NA
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==1&Pain_data$Painful_disease3==0&is.na(Pain_data$Painful_disease4)]<-"A"
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==0&Pain_data$Painful_disease3==0&is.na(Pain_data$Painful_disease4)]<-"A"

Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==1&is.na(Pain_data$Painful_disease3)&Pain_data$Painful_disease4==0]<-"B"
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==0&is.na(Pain_data$Painful_disease3)&Pain_data$Painful_disease4==0]<-"B"

Pain_data$PD[Pain_data$Painful_disease1==1&is.na(Pain_data$Painful_disease3)&Pain_data$Painful_disease3==1&Pain_data$Painful_disease4==0]<-"B"
Pain_data$PD[Pain_data$Painful_disease1==1&is.na(Pain_data$Painful_disease3)&Pain_data$Painful_disease3==0&Pain_data$Painful_disease4==0]<-"A"
#2 NA
Pain_data$PD[Pain_data$Painful_disease1==1&is.na(Pain_data$Painful_disease2)&is.na(Pain_data$Painful_disease3)&Pain_data$Painful_disease4==0]<-"B"
Pain_data$PD[Pain_data$Painful_disease1==1&is.na(Pain_data$Painful_disease2)&Pain_data$Painful_disease3==0&is.na(Pain_data$Painful_disease4)]<-"A"
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==0&is.na(Pain_data$Painful_disease3)&is.na(Pain_data$Painful_disease4)]<-"B"


table(Pain_data$PD)
table(Pain_data$PD,Pain_data$Alzheimer_dementia)

#Continuous pain
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==1&Pain_data$Painful_disease3==1&Pain_data$Painful_disease4==1]<-"C"
Pain_data$PD[Pain_data$Painful_disease1==1&Pain_data$Painful_disease2==1&Pain_data$Painful_disease3==1&is.na(Pain_data$Painful_disease4)]<-"C"
Pain_data$PD[Pain_data$Painful_disease1==1&is.na(Pain_data$Painful_disease2)&Pain_data$Painful_disease3==1&Pain_data$Painful_disease4==1]<-"C"
Pain_data$PD[Pain_data$Painful_disease1==1&is.na(Pain_data$Painful_disease2)&Pain_data$Painful_disease3==1&is.na(Pain_data$Painful_disease4)]<-"C"

table(Pain_data$PD)
table(Pain_data$PD,Pain_data$Alzheimer_dementia)



#Recurrent pain



#Never pain
Pain_data$PD[Pain_data$Painful_disease1==0&Pain_data$Painful_disease2==0&Pain_data$Painful_disease3==0&Pain_data$Painful_disease4==0]<-"D"
Pain_data$PD[Pain_data$Painful_disease1==0&Pain_data$Painful_disease2==0&Pain_data$Painful_disease3==0&is.na(Pain_data$Painful_disease4)]<-"D"
Pain_data$PD[Pain_data$Painful_disease1==0&Pain_data$Painful_disease2==0&is.na(Pain_data$Painful_disease3)&Pain_data$Painful_disease4==0]<-"E"
Pain_data$PD[Pain_data$Painful_disease1==0&is.na(Pain_data$Painful_disease2)&Pain_data$Painful_disease3==0&Pain_data$Painful_disease4==0]<-"D"
Pain_data$PD[Pain_data$Painful_disease1==0&Pain_data$Painful_disease2==0&is.na(Pain_data$Painful_disease3)&is.na(Pain_data$Painful_disease4)]<-"E"
Pain_data$PD[Pain_data$Painful_disease1==0&is.na(Pain_data$Painful_disease2)&is.na(Pain_data$Painful_disease3)&Pain_data$Painful_disease4==0]<-"E"
Pain_data$PD[Pain_data$Painful_disease1==0&is.na(Pain_data$Painful_disease2)&Pain_data$Painful_disease3==0&is.na(Pain_data$Painful_disease4)]<-"D"

table(Pain_data$PD)
table(Pain_data$PD,Pain_data$Alzheimer_dementia)
Pain_data_all<-na.omit(Pain_data[,c("eid","PD")])
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$PD<-NULL
Interpolation_data<-merge(Interpolation_data,Pain_data_all,by = "eid",all.x = T)
save(Interpolation_data,file="original data/Interpolation_data.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 1 Oral data Painful gums arrangement ####
Oral<-fread("original data/Oral data.csv")
Oral_data<-as.data.frame(Oral$eid)
colnames(Oral_data)<-"eid"
Oral_data$oral1<-Oral$p6149_i0
Oral_data$oral2<-Oral$p6149_i1
Oral_data$oral3<-Oral$p6149_i2
Oral_data$oral4<-Oral$p6149_i3
Oral_data<-subset(Oral_data,oral1!=""&oral1!=" Prefer not to answer"&oral1!="Prefer not to answer")
Oral_data[Oral_data==" Prefer not to answer"]<-NA
Oral_data[Oral_data=="Prefer not to answer"]<-NA
Oral_data[Oral_data==""]<-NA

Oral_data$Painful_gums1<-grepl("*Painful gums*",Oral_data$oral1)
Oral_data$Toothache1<-grepl("*Toothache*",Oral_data$oral1)

Oral_data$Painful_gums2<-grepl("*Painful gums*",Oral_data$oral2)
Oral_data$Toothache2<-grepl("*Toothache*",Oral_data$oral2)

Oral_data$Painful_gums3<-grepl("*Painful gums*",Oral_data$oral3)
Oral_data$Toothache3<-grepl("*Toothache*",Oral_data$oral3)

Oral_data$Painful_gums4<-grepl("*Painful gums*",Oral_data$oral4)
Oral_data$Toothache4<-grepl("*Toothache*",Oral_data$oral4)

Oral_data[Oral_data==T]<-1
Oral_data[Oral_data==F]<-0

Oral_data1<-as.data.frame(Oral$eid)
colnames(Oral_data1)<-"eid"
Oral_data1$oral22<-Oral$p6149_i1
Oral_data1$oral33<-Oral$p6149_i2
Oral_data1$oral44<-Oral$p6149_i3
Oral_data1[Oral_data1==" Prefer not to answer"]<-NA
Oral_data1[Oral_data1=="Prefer not to answer"]<-NA
Oral_data1[Oral_data1==""]<-NA
Oral_dataall<-merge(Oral_data,Oral_data1,by = "eid",all.x = T)
Oral_dataall$Painful_gums2[is.na(Oral_dataall$oral22)]<-NA
Oral_dataall$Painful_gums3[is.na(Oral_dataall$oral33)]<-NA
Oral_dataall$Painful_gums4[is.na(Oral_dataall$oral44)]<-NA
load(file="original data/Interpolation_data.Rdata")
EID<-Interpolation_data[,c("eid","Alzheimer_dementia")]
Pain_data<-merge(Oral_dataall,EID,by = 'eid',all.y = T)
Pain_data$PG<-NULL
#### Controlled pain ####
#no NA
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==1&Pain_data$Painful_gums3==1&Pain_data$Painful_gums4==0]<-"B"
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==1&Pain_data$Painful_gums3==0&Pain_data$Painful_gums4==0]<-"A"
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==0&Pain_data$Painful_gums3==0&Pain_data$Painful_gums4==0]<-"A"
#1 NA
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==1&Pain_data$Painful_gums3==0&is.na(Pain_data$Painful_gums4)]<-"A"
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==0&Pain_data$Painful_gums3==0&is.na(Pain_data$Painful_gums4)]<-"A"

Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==1&is.na(Pain_data$Painful_gums3)&Pain_data$Painful_gums4==0]<-"B"
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==0&is.na(Pain_data$Painful_gums3)&Pain_data$Painful_gums4==0]<-"B"

Pain_data$PG[Pain_data$Painful_gums1==1&is.na(Pain_data$Painful_gums3)&Pain_data$Painful_gums3==1&Pain_data$Painful_gums4==0]<-"B"
Pain_data$PG[Pain_data$Painful_gums1==1&is.na(Pain_data$Painful_gums3)&Pain_data$Painful_gums3==0&Pain_data$Painful_gums4==0]<-"A"
#2 NA
Pain_data$PG[Pain_data$Painful_gums1==1&is.na(Pain_data$Painful_gums2)&is.na(Pain_data$Painful_gums3)&Pain_data$Painful_gums4==0]<-"B"
Pain_data$PG[Pain_data$Painful_gums1==1&is.na(Pain_data$Painful_gums2)&Pain_data$Painful_gums3==0&is.na(Pain_data$Painful_gums4)]<-"A"
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==0&is.na(Pain_data$Painful_gums3)&is.na(Pain_data$Painful_gums4)]<-"B"


table(Pain_data$PG)
table(Pain_data$PG,Pain_data$Alzheimer_dementia)

#Continuous pain
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==1&Pain_data$Painful_gums3==1&Pain_data$Painful_gums4==1]<-"C"
Pain_data$PG[Pain_data$Painful_gums1==1&Pain_data$Painful_gums2==1&Pain_data$Painful_gums3==1&is.na(Pain_data$Painful_gums4)]<-"C"
Pain_data$PG[Pain_data$Painful_gums1==1&is.na(Pain_data$Painful_gums2)&Pain_data$Painful_gums3==1&Pain_data$Painful_gums4==1]<-"C"
Pain_data$PG[Pain_data$Painful_gums1==1&is.na(Pain_data$Painful_gums2)&Pain_data$Painful_gums3==1&is.na(Pain_data$Painful_gums4)]<-"C"

table(Pain_data$PG)
table(Pain_data$PG,Pain_data$Alzheimer_dementia)



#Recurrent pain



#Never pain
Pain_data$PG[Pain_data$Painful_gums1==0&Pain_data$Painful_gums2==0&Pain_data$Painful_gums3==0&Pain_data$Painful_gums4==0]<-"D"
Pain_data$PG[Pain_data$Painful_gums1==0&Pain_data$Painful_gums2==0&Pain_data$Painful_gums3==0&is.na(Pain_data$Painful_gums4)]<-"D"
Pain_data$PG[Pain_data$Painful_gums1==0&Pain_data$Painful_gums2==0&is.na(Pain_data$Painful_gums3)&Pain_data$Painful_gums4==0]<-"E"
Pain_data$PG[Pain_data$Painful_gums1==0&is.na(Pain_data$Painful_gums2)&Pain_data$Painful_gums3==0&Pain_data$Painful_gums4==0]<-"D"
Pain_data$PG[Pain_data$Painful_gums1==0&Pain_data$Painful_gums2==0&is.na(Pain_data$Painful_gums3)&is.na(Pain_data$Painful_gums4)]<-"E"
Pain_data$PG[Pain_data$Painful_gums1==0&is.na(Pain_data$Painful_gums2)&is.na(Pain_data$Painful_gums3)&Pain_data$Painful_gums4==0]<-"E"
Pain_data$PG[Pain_data$Painful_gums1==0&is.na(Pain_data$Painful_gums2)&Pain_data$Painful_gums3==0&is.na(Pain_data$Painful_gums4)]<-"D"

table(Pain_data$PG)
table(Pain_data$PG,Pain_data$Alzheimer_dementia)
Pain_data_all<-na.omit(Pain_data[,c("eid","PG")])
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$PG<-NULL
Interpolation_data<-merge(Interpolation_data,Pain_data_all,by = "eid",all.x = T)
save(Interpolation_data,file="original data/Interpolation_data.Rdata")



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> section 1 Oral data Toothache arrangement ####
Oral<-fread("original data/Oral data.csv")
Oral_data<-as.data.frame(Oral$eid)
colnames(Oral_data)<-"eid"
Oral_data$oral1<-Oral$p6149_i0
Oral_data$oral2<-Oral$p6149_i1
Oral_data$oral3<-Oral$p6149_i2
Oral_data$oral4<-Oral$p6149_i3
Oral_data<-subset(Oral_data,oral1!=""&oral1!=" Prefer not to answer"&oral1!="Prefer not to answer")
Oral_data[Oral_data==" Prefer not to answer"]<-NA
Oral_data[Oral_data=="Prefer not to answer"]<-NA
Oral_data[Oral_data==""]<-NA

Oral_data$Painful_gums1<-grepl("*Painful gums*",Oral_data$oral1)
Oral_data$Toothache1<-grepl("*Toothache*",Oral_data$oral1)

Oral_data$Painful_gums2<-grepl("*Painful gums*",Oral_data$oral2)
Oral_data$Toothache2<-grepl("*Toothache*",Oral_data$oral2)

Oral_data$Painful_gums3<-grepl("*Painful gums*",Oral_data$oral3)
Oral_data$Toothache3<-grepl("*Toothache*",Oral_data$oral3)

Oral_data$Painful_gums4<-grepl("*Painful gums*",Oral_data$oral4)
Oral_data$Toothache4<-grepl("*Toothache*",Oral_data$oral4)

Oral_data[Oral_data==T]<-1
Oral_data[Oral_data==F]<-0

Oral_data1<-as.data.frame(Oral$eid)
colnames(Oral_data1)<-"eid"
Oral_data1$oral22<-Oral$p6149_i1
Oral_data1$oral33<-Oral$p6149_i2
Oral_data1$oral44<-Oral$p6149_i3
Oral_data1[Oral_data1==" Prefer not to answer"]<-NA
Oral_data1[Oral_data1=="Prefer not to answer"]<-NA
Oral_data1[Oral_data1==""]<-NA
Oral_dataall<-merge(Oral_data,Oral_data1,by = "eid",all.x = T)
Oral_dataall$Toothache2[is.na(Oral_dataall$oral22)]<-NA
Oral_dataall$Toothache3[is.na(Oral_dataall$oral33)]<-NA
Oral_dataall$Toothache4[is.na(Oral_dataall$oral44)]<-NA
load(file="original data/Interpolation_data.Rdata")
EID<-Interpolation_data[,c("eid","Alzheimer_dementia")]
Pain_data<-merge(Oral_dataall,EID,by = 'eid',all.y = T)
Pain_data$TA<-NULL
#### Controlled pain ####
#no NA
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==1&Pain_data$Toothache3==1&Pain_data$Toothache4==0]<-"B"
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==1&Pain_data$Toothache3==0&Pain_data$Toothache4==0]<-"A"
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==0&Pain_data$Toothache3==0&Pain_data$Toothache4==0]<-"A"
#1 NA
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==1&Pain_data$Toothache3==0&is.na(Pain_data$Toothache4)]<-"A"
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==0&Pain_data$Toothache3==0&is.na(Pain_data$Toothache4)]<-"A"

Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==1&is.na(Pain_data$Toothache3)&Pain_data$Toothache4==0]<-"B"
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==0&is.na(Pain_data$Toothache3)&Pain_data$Toothache4==0]<-"B"

Pain_data$TA[Pain_data$Toothache1==1&is.na(Pain_data$Toothache3)&Pain_data$Toothache3==1&Pain_data$Toothache4==0]<-"B"
Pain_data$TA[Pain_data$Toothache1==1&is.na(Pain_data$Toothache3)&Pain_data$Toothache3==0&Pain_data$Toothache4==0]<-"A"
#2 NA
Pain_data$TA[Pain_data$Toothache1==1&is.na(Pain_data$Toothache2)&is.na(Pain_data$Toothache3)&Pain_data$Toothache4==0]<-"B"
Pain_data$TA[Pain_data$Toothache1==1&is.na(Pain_data$Toothache2)&Pain_data$Toothache3==0&is.na(Pain_data$Toothache4)]<-"A"
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==0&is.na(Pain_data$Toothache3)&is.na(Pain_data$Toothache4)]<-"B"

table(Pain_data$TA)
table(Pain_data$TA,Pain_data$Alzheimer_dementia)

#Continuous pain
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==1&Pain_data$Toothache3==1&Pain_data$Toothache4==1]<-"C"
Pain_data$TA[Pain_data$Toothache1==1&Pain_data$Toothache2==1&Pain_data$Toothache3==1&is.na(Pain_data$Toothache4)]<-"C"
Pain_data$TA[Pain_data$Toothache1==1&is.na(Pain_data$Toothache2)&Pain_data$Toothache3==1&Pain_data$Toothache4==1]<-"C"
Pain_data$TA[Pain_data$Toothache1==1&is.na(Pain_data$Toothache2)&Pain_data$Toothache3==1&is.na(Pain_data$Toothache4)]<-"C"

table(Pain_data$TA)
table(Pain_data$TA,Pain_data$Alzheimer_dementia)



#Recurrent pain



#Never pain
Pain_data$TA[Pain_data$Toothache1==0&Pain_data$Toothache2==0&Pain_data$Toothache3==0&Pain_data$Toothache4==0]<-"D"
Pain_data$TA[Pain_data$Toothache1==0&Pain_data$Toothache2==0&Pain_data$Toothache3==0&is.na(Pain_data$Toothache4)]<-"D"
Pain_data$TA[Pain_data$Toothache1==0&Pain_data$Toothache2==0&is.na(Pain_data$Toothache3)&Pain_data$Toothache4==0]<-"E"
Pain_data$TA[Pain_data$Toothache1==0&is.na(Pain_data$Toothache2)&Pain_data$Toothache3==0&Pain_data$Toothache4==0]<-"D"
Pain_data$TA[Pain_data$Toothache1==0&Pain_data$Toothache2==0&is.na(Pain_data$Toothache3)&is.na(Pain_data$Toothache4)]<-"E"
Pain_data$TA[Pain_data$Toothache1==0&is.na(Pain_data$Toothache2)&is.na(Pain_data$Toothache3)&Pain_data$Toothache4==0]<-"E"
Pain_data$TA[Pain_data$Toothache1==0&is.na(Pain_data$Toothache2)&Pain_data$Toothache3==0&is.na(Pain_data$Toothache4)]<-"D"

table(Pain_data$TA)
table(Pain_data$TA,Pain_data$Alzheimer_dementia)
Pain_data_all<-na.omit(Pain_data[,c("eid","TA")])
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$TA<-NULL
Interpolation_data<-merge(Interpolation_data,Pain_data_all,by = "eid",all.x = T)
save(Interpolation_data,file="original data/Interpolation_data.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# >>>>> Cox  ####

load(file="original data/Interpolation_data.Rdata")
table(Interpolation_data$PD,Interpolation_data$Alzheimer_dementia)
Interpolation_data<-Interpolation_data
table(Interpolation_data$PD,Interpolation_data$Painful_disease,useNA = "ifany")
#### Oral pain  ####
Interpolation_data$Oral_pain_all[Interpolation_data$Painful_disease==1]<-1
Interpolation_data$Oral_pain_all[Interpolation_data$PD=="D"|Interpolation_data$PD=="E"]<-0
Interpolation_data$Oral_pain_con[Interpolation_data$Painful_disease==1&(Interpolation_data$PD=="A"|Interpolation_data$PD=="B")]<-1
Interpolation_data$Oral_pain_con[Interpolation_data$PD=="D"|Interpolation_data$PD=="E"]<-0
Interpolation_data$Oral_pain_non[Interpolation_data$Painful_disease==1&(Interpolation_data$Oral_pain_con!=1|is.na(Interpolation_data$Oral_pain_con))]<-1
Interpolation_data$Oral_pain_non[Interpolation_data$PD=="D"|Interpolation_data$PD=="E"]<-0
table(Interpolation_data$Oral_pain_con,Interpolation_data$Oral_pain_non,useNA = "ifany")
Interpolation_data$Oral_pain_vs[Interpolation_data$Oral_pain_con==1]<-1
Interpolation_data$Oral_pain_vs[Interpolation_data$Oral_pain_non==1]<-0
table(Interpolation_data$Oral_pain_vs,Interpolation_data$Oral_pain_all,useNA = "ifany")
#### Painful gums ####

Interpolation_data$Pain_gums_all[Interpolation_data$Painful_gums==1]<-1
Interpolation_data$Pain_gums_all[Interpolation_data$PG=="D"|Interpolation_data$PG=="E"]<-0
Interpolation_data$Pain_gums_con[Interpolation_data$Painful_gums==1&(Interpolation_data$PG=="A"|Interpolation_data$PG=="B")]<-1
Interpolation_data$Pain_gums_con[Interpolation_data$PG=="D"|Interpolation_data$PG=="E"]<-0
Interpolation_data$Pain_gums_non[Interpolation_data$Painful_gums==1&(Interpolation_data$Pain_gums_con!=1|is.na(Interpolation_data$Pain_gums_con))]<-1
Interpolation_data$Pain_gums_non[Interpolation_data$PG=="D"|Interpolation_data$PG=="E"]<-0
table(Interpolation_data$Pain_gums_con,Interpolation_data$Pain_gums_non,useNA = "ifany")
Interpolation_data$Pain_gums_vs[Interpolation_data$Pain_gums_con==1]<-1
Interpolation_data$Pain_gums_vs[Interpolation_data$Pain_gums_non==1]<-0
table(Interpolation_data$Pain_gums_vs,Interpolation_data$Pain_gums_all,useNA = "ifany")
#### Toothache ####
Interpolation_data$Tooth_ache<-NULL
Interpolation_data$Tooth_ache_all[Interpolation_data$Toothache==1]<-1
Interpolation_data$Tooth_ache_all[Interpolation_data$TA=="D"|Interpolation_data$TA=="E"]<-0
Interpolation_data$Tooth_ache_con[Interpolation_data$Toothache==1&(Interpolation_data$TA=="A"|Interpolation_data$TA=="B")]<-1
Interpolation_data$Tooth_ache_con[Interpolation_data$TA=="D"|Interpolation_data$TA=="E"]<-0
Interpolation_data$Tooth_ache_non[Interpolation_data$Toothache==1&(Interpolation_data$Tooth_ache_con!=1|is.na(Interpolation_data$Tooth_ache_con))]<-1
Interpolation_data$Tooth_ache_non[Interpolation_data$TA=="D"|Interpolation_data$TA=="E"]<-0
Interpolation_data$Tooth_ache_vs[Interpolation_data$Tooth_ache_con==1]<-1
Interpolation_data$Tooth_ache_vs[Interpolation_data$Tooth_ache_non==1]<-0
table(Interpolation_data$Tooth_ache_vs,Interpolation_data$Tooth_ache_all,useNA = "ifany")
table(Interpolation_data$Tooth_ache_con,Interpolation_data$Tooth_ache_non,useNA = "ifany")
Matched_Interpolation_data<-Interpolation_data

save(Matched_Interpolation_data,file="original data/Interpolation_data_selected_COX.Rdata")

# >>>>> structure & configtion  ####
# >>>>> section 1 Oral data Toothache arrangement ####

load(file="original data/Interpolation_data.Rdata")
#### Oral pain  ####
Interpolation_data$Oral_pain_all[Interpolation_data$PD=="A"|Interpolation_data$PD=="C"]<-1
Interpolation_data$Oral_pain_all[Interpolation_data$PD=="D"]<-0
Interpolation_data$Oral_pain_con[Interpolation_data$PD=="A"]<-1
Interpolation_data$Oral_pain_con[Interpolation_data$PD=="D"]<-0
Interpolation_data$Oral_pain_non[Interpolation_data$PD=="C"]<-1
Interpolation_data$Oral_pain_non[Interpolation_data$PD=="D"]<-0

table(Interpolation_data$Oral_pain_con,Interpolation_data$Oral_pain_non,useNA = "ifany")
Interpolation_data$Oral_pain_vs[Interpolation_data$Oral_pain_con==1]<-1
Interpolation_data$Oral_pain_vs[Interpolation_data$Oral_pain_non==1]<-0
table(Interpolation_data$Oral_pain_vs,Interpolation_data$Oral_pain_all,useNA = "ifany")
#### Painful gums ####

Interpolation_data$Pain_gums_all[Interpolation_data$PG=="A"|Interpolation_data$PG=="C"]<-1
Interpolation_data$Pain_gums_all[Interpolation_data$PG=="D"]<-0
Interpolation_data$Pain_gums_con[Interpolation_data$PG=="A"]<-1
Interpolation_data$Pain_gums_con[Interpolation_data$PG=="D"]<-0
Interpolation_data$Pain_gums_non[Interpolation_data$PG=="C"]<-1
Interpolation_data$Pain_gums_non[Interpolation_data$PG=="D"]<-0
table(Interpolation_data$Pain_gums_con,Interpolation_data$Pain_gums_non,useNA = "ifany")
Interpolation_data$Pain_gums_vs[Interpolation_data$Pain_gums_con==1]<-1
Interpolation_data$Pain_gums_vs[Interpolation_data$Pain_gums_non==1]<-0
table(Interpolation_data$Pain_gums_vs,Interpolation_data$Pain_gums_all,useNA = "ifany")
#### Toothache ####
Interpolation_data$Tooth_ache_all[Interpolation_data$TA=="A"|Interpolation_data$TA=="C"]<-1
Interpolation_data$Tooth_ache_all[Interpolation_data$TA=="D"]<-0
Interpolation_data$Tooth_ache_con[Interpolation_data$TA=="A"]<-1
Interpolation_data$Tooth_ache_con[Interpolation_data$TA=="D"]<-0
Interpolation_data$Tooth_ache_non[Interpolation_data$TA=="C"]<-1
Interpolation_data$Tooth_ache_non[Interpolation_data$TA=="D"]<-0
Interpolation_data$Tooth_ache_vs[Interpolation_data$Tooth_ache_con==1]<-1
Interpolation_data$Tooth_ache_vs[Interpolation_data$Tooth_ache_non==1]<-0
table(Interpolation_data$Tooth_ache_con,Interpolation_data$Tooth_ache_non,useNA = "ifany")
table(Interpolation_data$Tooth_ache_vs,Interpolation_data$Tooth_ache_all,useNA = "ifany")

Matched_Interpolation_data<-Interpolation_data

save(Matched_Interpolation_data,file="original data/Interpolation_data_selected_Other.Rdata")


# >>>>> structure & configtion  ####
# >>>>> section 1 Oral data Toothache arrangement ####
Data_pain<-fread("original data/total pain.csv")
table(Data_pain$p6159_i0)
Pain_data<-as.data.frame(Data_pain$eid)
colnames(Pain_data)<-"eid"
Pain_data$Pain<-Data_pain$p6159_i0
Pain_data<-subset(Pain_data,Pain!=""&Pain!=" Prefer not to answer"&Pain!="Prefer not to answer")
table(Pain_data$Pain,useNA='ifany')
Pain_data$Headache<-grepl("*Headache*",Pain_data$Pain)
Pain_data$Facial_pain<-grepl("*Facial pain*",Pain_data$Pain)
Pain_data$Neck_shoulder_pain<-grepl("*Neck or shoulder pain*",Pain_data$Pain)
Pain_data$Back_pain<-grepl("*Back pain*",Pain_data$Pain)
Pain_data$Stomach_abdominal_pain<-grepl("*Stomach or abdominal pain*",Pain_data$Pain)
Pain_data$Hip_pain<-grepl("*Hip pain*",Pain_data$Pain)
Pain_data$Knee_pain<-grepl("*Knee pain*",Pain_data$Pain)
Pain_data$All_pain<-grepl("*Pain all over the body*",Pain_data$Pain)

Pain_data[Pain_data==T]<-1
Pain_data[Pain_data==F]<-0
Pain_data$Body_pain[Pain_data$Pain=="None of the above"]<-0
Pain_data$Body_pain[Pain_data$Headache==1|
                      Pain_data$Facial_pain==1|
                      Pain_data$Neck_shoulder_pain==1|
                      Pain_data$Back_pain==1|
                      Pain_data$Stomach_abdominal_pain==1|
                      Pain_data$Hip_pain==1|
                      Pain_data$Knee_pain==1|
                      Pain_data$All_pain==1]<-1
table(Pain_data$Body_pain)

Pain_data$Body_pain<-as.factor(Pain_data$Body_pain)
Pain_data$Headache<-as.factor(Pain_data$Headache)
Pain_data$Facial_pain<-as.factor(Pain_data$Facial_pain)
Pain_data$Neck_shoulder_pain<-as.factor(Pain_data$Neck_shoulder_pain)
Pain_data$Back_pain<-as.factor(Pain_data$Back_pain)
Pain_data$Stomach_abdominal_pain<-as.factor(Pain_data$Stomach_abdominal_pain)
Pain_data$Hip_pain<-as.factor(Pain_data$Hip_pain)
Pain_data$Knee_pain<-as.factor(Pain_data$Knee_pain)
Pain_data$All_pain<-as.factor(Pain_data$All_pain)
Pain_data$Pain<-NULL
Pain_data_final<-Pain_data
table(Pain_data$Knee_pain)
save(Pain_data_final,file = "original data/Pain_data_final.Rdata")

load(file="original data/Interpolation_data.Rdata")

Interpolation_data<-merge(Interpolation_data,Pain_data_final,by = "eid",all.x = T)
save(Interpolation_data,file="original data/Interpolation_data_body_pain.Rdata")
