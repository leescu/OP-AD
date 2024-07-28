# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
setwd("M:/OP&AZ/Cohort data")
library(data.table)
library(dplyr)
# >>>>> section 1 Interpolation cohort data define ####
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
