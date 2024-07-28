setwd("M:/OP&AZ/Cohort data")
library(data.table)
library("survival")
library(tableone)
library(Matching)
library(mediation)
library(survey)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(ggplot2)
library(ggsci)
library(ggrepel)
library(survival)
library(survminer)
library(tidyr)
library(forestplot)
library(mgcv)
library(plyr)
library(pheatmap)
library(Matching)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# +++++++section 1 Interpolation data+++++++++ ####

load(file="original data/Interpolation_data.Rdata")
colnames(Interpolation_data)
pbc<-Interpolation_data[,c(4,7,10,24,254:269)]
tab<-CreateTableOne(data = pbc ,strata = "Painful_disease" )
tableone<-print(tab, formatOptions = list(big.mark = ","))
write.csv(tableone, file = "original data/Supplementary Table 2.csv")
table(Interpolation_data$Alzheimer_dementia,Interpolation_data$Painful_disease)


# +++++++section 2 Interpolation data+++++++++ ####
load(file="original data/Interpolation_data.Rdata")
colnames(Interpolation_data)
pbc<-Interpolation_data[,c(4,7,10,24,254:269)]
tab<-CreateTableOne(data = pbc ,strata = "Painful_disease" )
tableone<-print(tab, formatOptions = list(big.mark = ","))
write.csv(tableone, file = "original data/Supplementary Table 2.csv")
table(Interpolation_data$Alzheimer_dementia,Interpolation_data$Painful_disease)
