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
#* Section 2.2 : Cumulative Incidence of Alzheimer Dementia ####
library(ggthemes)
load(file = "original data/Interpolation_data.Rdata")
Interpolation_data$years<-as.numeric(Interpolation_data$days/365)
FIT_ALL_PD<-survfit(Surv(years, Alzheimer_dementia==1) ~ Painful_disease, Interpolation_data)
ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
           ncensor.plot=F,risk.table =F, 
           break.x.by = 5,font.x = c(20, "bold","black"),font.y = c(20, "bold","black"),font.tickslab = c(12, "plain", "black"),
           xlab ="Time in Year",  pval =T, legend.title = "",ylim = c(0,0.015),ggtheme =  theme_calc())

# pdf 6*8
Figure_1C<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                      ncensor.plot=F,risk.table = T, 
                      break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                      xlab ="Time in Year",  pval =T, legend.title = "",ylim = c(0,0.2),ggtheme =  theme_calc())
print(Figure_1C)



FIT_ALL_PD<-survfit(Surv(years, Alzheimer_dementia==1) ~ Toothache, Interpolation_data)
ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
           ncensor.plot=F,risk.table =F, 
           break.x.by = 5,font.x = c(20, "bold","black"),font.y = c(20, "bold","black"),font.tickslab = c(12, "plain", "black"),
           xlab ="Time in Year",  pval =T, legend.title = "",ylim = c(0,0.015),ggtheme =  theme_calc())

# pdf 6*8
Figure_1C<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                      ncensor.plot=F,risk.table = T, 
                      break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                      xlab ="Time in Year",  pval =T, legend.title = "",ylim = c(0,0.2),ggtheme =  theme_calc())
print(Figure_1C)



FIT_ALL_PD<-survfit(Surv(years, Alzheimer_dementia==1) ~ Painful_gums, Interpolation_data)
ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
           ncensor.plot=F,risk.table =F, 
           break.x.by = 5,font.x = c(20, "bold","black"),font.y = c(20, "bold","black"),font.tickslab = c(12, "plain", "black"),
           xlab ="Time in Year",  pval =T, legend.title = "",ylim = c(0,0.015),ggtheme =  theme_calc())

# pdf 6*8
Figure_1C<-ggsurvplot(FIT_ALL_PD,conf.int =TRUE, fun = "cumhaz",  tables.theme = theme_cleantable(), palette=c("#0073C2E5", "#EFC000E5"), 
                      ncensor.plot=F,risk.table = T, 
                      break.x.by = 5,font.x = c(14, "bold","black"),font.y = c(14, "bold","black"),font.tickslab = c(12, "plain", "black"),
                      xlab ="Time in Year",  pval =T, legend.title = "",ylim = c(0,0.2),ggtheme =  theme_calc())
print(Figure_1C)