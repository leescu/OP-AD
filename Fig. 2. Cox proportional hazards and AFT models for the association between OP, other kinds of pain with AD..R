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
#* Section 1: Alzheimer Dementia (COX) ####
#** Analysis ####
load(file="original data/Interpolation_data.Rdata")

# Define variables
variables <- c("Periodontal_disease", "Painful_disease", "Painful_gums", "Toothache")
models <- list(
  model1 = "Age + Sex + Ethnicity + Education + BMI_status + TDI_quantile",
  model2 = "Age + Sex + Ethnicity + Education + BMI_status + TDI_quantile + Smoke + Alcohol + PA",
  model3 = "Age + Sex + Ethnicity + Education + BMI_status + TDI_quantile + Smoke + Alcohol + PA + Diabetes + Hypertension + Medication"
)

# Initialize results dataframe
result_all <- data.frame()

# Loop through each variable and model
for (variable in variables) {
  for (model_name in names(models)) {
    formula <- as.formula(paste("Surv(days, Alzheimer_dementia) ~", variable, "+", models[[model_name]]))
    model <- coxph(formula, data = Interpolation_data)
    model_result <- summary(model)
    P <- model_result[["coefficients"]][1, "Pr(>|z|)"]
    HR <- as.numeric(model_result[["conf.int"]][1, c("exp(coef)", "lower .95", "upper .95")])
    result <- data.frame('HR' = HR[1], 'lower .95' = HR[2], 'upper .95' = HR[3], 'P value' = P, 'model' = model_name, 'status' = variable)
    result_all <- rbind(result_all, result)
  }
}

# Rename variables for better readability
result_all$status <- factor(result_all$status, levels = c("Periodontal_disease", "Painful_disease", "Painful_gums", "Toothache"),
                            labels = c("Periodontal disease", "Painful disease", "Painful gums", "Toothache"))

# Save results
save(result_all, file="Results final/result_all_Alzheimer_dementia_COX.Rdata")
result_all

#** Plotting ####
load("Results final/result_all_Alzheimer_dementia_COX.Rdata")       
head(result_all)

model_category<-result_all
rs_forest<-as.data.frame(cbind(model_category$model,round(model_category$HR,3),
                               round(model_category$lower..95,3),round(model_category$upper..95,3),round(model_category$P.value,3),
                               model_category$HR,model_category$lower..95,model_category$upper..95),
)
colnames(rs_forest)<-c('V1','V2','V3','V4','V5','V6','V7','V8')
rs_forest$V2<-as.character(rs_forest$V2)
rs_forest$V3<-as.character(rs_forest$V3)
rs_forest$V4<-as.character(rs_forest$V4)
rs_forest$V8<-as.character(rs_forest$V8)

new.function <- function(x){
  while(nchar(x)<5){
    temp <- paste(x,0)
    x <- temp
    x <- gsub(" ","",x)
  }
  return(x)
}
rs_forest$V2<-lapply(rs_forest$V2,new.function)
rs_forest$V3<-lapply(rs_forest$V3,new.function)
rs_forest$V4<-lapply(rs_forest$V4,new.function)
rs_forest$V5<-lapply(rs_forest$V5,new.function)
rs_forest$V6<-as.numeric(rs_forest$V6)
rs_forest$V7<-as.numeric(rs_forest$V7)
rs_forest$V8<-as.numeric(rs_forest$V8)
rs_forest[rs_forest=="10000"]<-"1.000"
rs_forest[rs_forest=="00000"]<-"<0.001"
rs_forest[rs_forest=="model1"]<-"Model 1"
rs_forest[rs_forest=="model2"]<-"Model 2"
rs_forest[rs_forest=="model3"]<-"Model 3"
rs_forest$CI<-paste0("(",rs_forest$V3,"-",rs_forest$V4,")")
rs_forest<-rs_forest[,c('V1','V2','CI','V5','V6','V7','V8')]
colnames(rs_forest)<-c('V1','V2','V3','V4','V5','V6','V7')
Table<-c("Model","HR","95% CI","P value",NA,NA,NA)
Table<-rbind(Table,c("Periodontal disease","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[1:3,])
Table<-rbind(Table,c("Painful gums","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[7:9,])
Table<-rbind(Table,c("Toothache","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[10:12,])
Table<-rbind(Table,c("Oral pain","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[4:6,])
forest<-Table
forest
paste(c(rep("T",2),rep("F",3),"T",rep("F",3),"T",rep("F",3),
        "T",rep("F",3)), sep=",",collapse = ",")
a1<-paste(forest[,1])
a2<-paste(forest[,2])
a3<-paste(forest[,3])
a4<-paste(forest[,4])
labeltext=cbind(a1,a2,a3,a4)

pdf("Results final/Fig. 2 Forestplot for COX.pdf",  height=4,width=11, onefile = FALSE)
forestplot(labeltext=labeltext,
           graphwidth=unit(45,'mm'),
           mean = forest$V5,
           col=fpColors(line = "#CC79A7",
                        box="#D55E00"),
           lower=forest$V6,upper=forest$V7,is.summary=c(T,T,F,F,F,T,F,F,F,T,F,F,F,T,F,F,F),
           zero=1,boxsize=0.25,lineheight=unit(6,'mm'),colgap=unit(6,'mm'),xaxt = "n",  # 阻止绘制 x 轴
           xlab = "",cex.axis = 4,lwd.zero=2,lwd.ci=2,xticks=c(0.9,1.0,1.2,1.4,1.6,1.8),clip = c(.9,1.8),lwd.xaxis=3,lty.ci = "solid",graph.pos = 4)
#PDF:heigt=9,width=11
dev.off()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 2: Alzheimer Dementia (WB) ####
#** Analysis ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_gums<-as.character(Interpolation_data$Painful_gums)
Interpolation_data$Painful_gums<-as.numeric(Interpolation_data$Painful_gums)
Interpolation_data$Toothache<-as.character(Interpolation_data$Toothache)
Interpolation_data$Toothache<-as.numeric(Interpolation_data$Toothache)
Interpolation_data$Painful_disease<-as.character(Interpolation_data$Painful_disease)
Interpolation_data$Painful_disease<-as.numeric(Interpolation_data$Painful_disease)
Interpolation_data$Periodontal_disease<-as.character(Interpolation_data$Periodontal_disease)
Interpolation_data$Periodontal_disease<-as.numeric(Interpolation_data$Periodontal_disease)
# Define variables
variables <- c("Periodontal_disease", "Painful_disease", "Painful_gums", "Toothache")
models <- list(
  model1 = "Age + Sex + Ethnicity + Education + BMI_status + TDI_quantile",
  model2 = "Age + Sex + Ethnicity + Education + BMI_status + TDI_quantile + Smoke + Alcohol + PA",
  model3 = "Age + Sex + Ethnicity + Education + BMI_status + TDI_quantile + Smoke + Alcohol + PA + Diabetes + Hypertension + Medication"
)

# Initialize results dataframe
result_all <- data.frame()

# Loop through each variable and model
for (variable in variables) {
  for (model_name in names(models)) {
    formula <- as.formula(paste("Surv(days, Alzheimer_dementia) ~", variable, "+", models[[model_name]]))
    model <- survreg(formula, data = Interpolation_data, dist = "weibull")
    model_result <- summary(model)
    coef_index <- which(names(coef(model)) == variable)
    if(length(coef_index) == 0) next
    
    coef <- coef(model)[coef_index]
    se <- model_result[["table"]][coef_index, "Std. Error"]
    P <- model_result[["table"]][coef_index, "p"]
    
    HR <- exp(coef)
    lower <- exp(coef - 1.96 * se)
    upper <- exp(coef + 1.96 * se)
    
    result <- data.frame('HR' = HR, 'lower .95' = lower, 'upper .95' = upper, 'P value' = P, 'model' = model_name, 'status' = variable)
    result_all <- rbind(result_all, result)
  }
}

# Rename variables for better readability
result_all$status <- factor(result_all$status, levels = c("Periodontal_disease", "Painful_disease", "Painful_gums", "Toothache"),
                            labels = c("Periodontal disease", "Painful disease", "Painful gums", "Toothache"))

# Save results
save(result_all, file="Results final/result_all_Alzheimer_dementia_WB.Rdata")
result_all

#** Plotting ####
load("Results final/result_all_Alzheimer_dementia_WB.Rdata")       
head(result_all)

model_category<-result_all
rs_forest<-as.data.frame(cbind(model_category$model,round(model_category$HR,3),
                               round(model_category$lower..95,3),round(model_category$upper..95,3),round(model_category$P.value,3),
                               model_category$HR,model_category$lower..95,model_category$upper..95),
)
colnames(rs_forest)<-c('V1','V2','V3','V4','V5','V6','V7','V8')
rs_forest$V2<-as.character(rs_forest$V2)
rs_forest$V3<-as.character(rs_forest$V3)
rs_forest$V4<-as.character(rs_forest$V4)
rs_forest$V8<-as.character(rs_forest$V8)

new.function <- function(x){
  while(nchar(x)<5){
    temp <- paste(x,0)
    x <- temp
    x <- gsub(" ","",x)
  }
  return(x)
}
rs_forest$V2<-lapply(rs_forest$V2,new.function)
rs_forest$V3<-lapply(rs_forest$V3,new.function)
rs_forest$V4<-lapply(rs_forest$V4,new.function)
rs_forest$V5<-lapply(rs_forest$V5,new.function)
rs_forest$V6<-as.numeric(rs_forest$V6)
rs_forest$V7<-as.numeric(rs_forest$V7)
rs_forest$V8<-as.numeric(rs_forest$V8)
rs_forest[rs_forest=="10000"]<-"1.000"
rs_forest[rs_forest=="00000"]<-"<0.001"
rs_forest[rs_forest=="model1"]<-"Model 1"
rs_forest[rs_forest=="model2"]<-"Model 2"
rs_forest[rs_forest=="model3"]<-"Model 3"
rs_forest$CI<-paste0("(",rs_forest$V3,"-",rs_forest$V4,")")
rs_forest<-rs_forest[,c('V1','V2','CI','V5','V6','V7','V8')]
colnames(rs_forest)<-c('V1','V2','V3','V4','V5','V6','V7')
Table<-c("Model","TR","95% CI","P value",NA,NA,NA)
Table<-rbind(Table,c("Periodontal disease","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[1:3,])
Table<-rbind(Table,c("Painful gums","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[7:9,])
Table<-rbind(Table,c("Toothache","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[10:12,])
Table<-rbind(Table,c("Oral pain","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[4:6,])
forest<-Table
forest
paste(c(rep("T",2),rep("F",3),"T",rep("F",3),"T",rep("F",3),
        "T",rep("F",3)), sep=",",collapse = ",")
a1<-paste(forest[,1])
a2<-paste(forest[,2])
a3<-paste(forest[,3])
a4<-paste(forest[,4])
labeltext=cbind(a1,a2,a3,a4)

pdf("Results final/Fig. 2 Forestplot for WB.pdf",  height=4,width=11, onefile = FALSE)
forestplot(labeltext=labeltext,
           graphwidth=unit(45,'mm'),
           mean = forest$V5,
           col=fpColors(line = "#CC79A7",
                        box="#D55E00"),
           lower=forest$V6,upper=forest$V7,is.summary=c(T,T,F,F,F,T,F,F,F,T,F,F,F,T,F,F,F),
           zero=1,boxsize=0.25,lineheight=unit(6,'mm'),colgap=unit(6,'mm'),xaxt = "n",  # 阻止绘制 x 轴
           xlab = "",cex.axis = 4,lwd.zero=2,lwd.ci=2,xticks=c(0.85,0.90,0.95,1.0,1.05),clip = c(.8,1.1),lwd.xaxis=3,lty.ci = "solid",graph.pos = 4)
#PDF:heigt=9,width=11
dev.off()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 3: Other pain (COX) ####
#* Section: Analysis ####


# Load data
load(file="original data/Interpolation_data_body_pain.Rdata")

# Define variables
variables <- c("Body_pain", "Facial_pain", "Neck_shoulder_pain", "Back_pain", "Stomach_abdominal_pain", "Hip_pain", "Knee_pain", "All_pain")

# Define model3 formula
model3 <- "Age + Sex + Ethnicity + Education + BMI_status + TDI_quantile + Smoke + Alcohol + PA + Diabetes + Hypertension + Medication"

# Initialize results dataframe
result_all <- data.frame()

# Loop through each variable
for (variable in variables) {
  formula <- as.formula(paste("Surv(days, Alzheimer_dementia) ~", variable, "+", model3))
  model <- coxph(formula, data = Interpolation_data)
  model_result <- summary(model)
  P <- model_result[["coefficients"]][1, "Pr(>|z|)"]
  HR <- as.numeric(model_result[["conf.int"]][1, c("exp(coef)", "lower .95", "upper .95")])
  result <- data.frame('HR' = HR[1], 'lower .95' = HR[2], 'upper .95' = HR[3], 'P value' = P, 'model' = "model3", 'status' = variable)
  result_all <- rbind(result_all, result)
}

# Rename variables for better readability
result_all$status <- factor(result_all$status, levels = variables,
                            labels = c("Body pain", "Facial pain", "Neck/shoulder pain", "Back pain", "Stomach/abdominal pain", "Hip pain", "Knee pain", "All pain"))
result_all_body_pain<-result_all
# Save results
save(result_all_body_pain, file="Results final/result_all_Alzheimer_dementia_body_pain.Rdata")





#** Plotting ####
load("Results final/result_all_Alzheimer_dementia.Rdata")    
load("Results final/result_all_Alzheimer_dementia_body_pain.Rdata")    
head(result_all)
head(result_all_body_pain)
result_all<-subset(result_all,model=="model3"&status!="Periodontal disease")
model_category<-rbind(result_all,result_all_body_pain)
rs_forest<-as.data.frame(cbind(round(model_category$HR,3),
                               round(model_category$lower..95,3),round(model_category$upper..95,3),round(model_category$P.value,3),
                               model_category$HR,model_category$lower..95,model_category$upper..95),
)
colnames(rs_forest)<-c('V1','V2','V3','V4','V5','V6','V7')
rs_forest$V1<-as.character(rs_forest$V1)
rs_forest$V2<-as.character(rs_forest$V2)
rs_forest$V3<-as.character(rs_forest$V3)
rs_forest$V7<-as.character(rs_forest$V7)

new.function <- function(x){
  while(nchar(x)<5){
    temp <- paste(x,0)
    x <- temp
    x <- gsub(" ","",x)
  }
  return(x)
}
rs_forest$V1<-lapply(rs_forest$V1,new.function)
rs_forest$V2<-lapply(rs_forest$V2,new.function)
rs_forest$V3<-lapply(rs_forest$V3,new.function)
rs_forest$V4<-lapply(rs_forest$V4,new.function)
rs_forest$V5<-as.numeric(rs_forest$V5)
rs_forest$V6<-as.numeric(rs_forest$V6)
rs_forest$V7<-as.numeric(rs_forest$V7)
rs_forest[rs_forest=="10000"]<-"1.000"
rs_forest[rs_forest=="00000"]<-"<0.001"

rs_forest$CI<-paste0("(",rs_forest$V2,"-",rs_forest$V3,")")
rs_forest$type<-as.character(model_category$status)

rs_forest$type[1]<-"Oral pain"
rs_forest$type[4]<-"All kinds of pain"
rs_forest$type[11]<-"Pain all over the body"
rs_forest<-rs_forest[,c('type','V1','CI','V4','V5','V6','V7')]
colnames(rs_forest)<-c('V1','V2','V3','V4','V5','V6','V7')
Table<-c("Pain type","HR","95% CI","P value",NA,NA,NA)
Table<-rbind(Table,c("Oral pain types","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[1:3,])
Table<-rbind(Table,c("Other pain types","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[4:11,])

forest<-Table
forest
paste(c(rep("T",2),rep("F",3),"T",rep("F",8)), sep=",",collapse = ",")
a1<-paste(forest[,1])
a2<-paste(forest[,2])
a3<-paste(forest[,3])
a4<-paste(forest[,4])
labeltext=cbind(a1,a2,a3,a4)

pdf("Results final/Fig. 2 Forestplot for all pain types.pdf",  height=4,width=11, onefile = FALSE)
forestplot(labeltext=labeltext,graphwidth=unit(45,'mm'),
           mean = forest$V5,
           col=fpColors(line = "#CC79A7",
                        box="#D55E00"),
           lower=forest$V6,upper=forest$V7,is.summary=c(T,T,F,F,F,T,F,F,F,F,F,F,F,F),
           zero=1,boxsize=0.25,lineheight=unit(6,'mm'),colgap=unit(6,'mm'),xaxt = "n", 
           xlab = "",cex.axis = 4,lwd.zero=2,lwd.ci=2,xticks=c(0.9,1.0,1.2,1.4,1.6),clip = c(.85,1.7),lwd.xaxis=3,lty.ci = "solid",graph.pos = 4)
#PDF:heigt=9,width=11
dev.off()
