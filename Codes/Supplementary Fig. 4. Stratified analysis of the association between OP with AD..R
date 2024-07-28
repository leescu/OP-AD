# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 4: Subgroup (COX) ####


load(file="original data/Interpolation_data.Rdata")


save_results <- function(result_df, file_name) {
  result_df <- result_df %>% mutate(across(where(is.numeric), round, 4))
  save(result_df, file = file_name)
}


run_coxph <- function(data, formula, model, status) {
  model_fit <- coxph(formula, data = data)
  model_result <- summary(model_fit)
  P <- model_result[["coefficients"]][1, "Pr(>|z|)"]
  HR <- as.numeric(model_result[["conf.int"]][1, c("exp(coef)", "lower .95", "upper .95")])
  data.frame('HR' = HR[1], 'lower..95' = HR[2], 'upper..95' = HR[3], 'P.value' = P, 'model' = model, 'status' = status)
}


run_glm_or_gaussian <- function(data, formula, model, status, family) {
  model_fit <- glm(formula, data = data, family = family)
  model_result <- summary(model_fit)
  BETA <- round(model_result[["coefficients"]][2, 1], 3)
  SE <- round(model_result[["coefficients"]][2, 2], 3)
  P <- model_result$coefficients[2, 4]
  data.frame('BETA' = BETA, 'SE' = SE, 'P.value' = P, 'model' = model, 'status' = status)
}


process_models <- function(data, disease, status, exclude_sex = FALSE) {

  covariates <- c("Age", "Ethnicity", "Education", "BMI_status", "TDI_quantile")
  if (!exclude_sex) {
    covariates <- c(covariates, "Sex")
  }
  

  formula_base <- paste(disease, "+", paste(covariates, collapse = " + "))
  result_cox <- rbind(
    run_coxph(data, as.formula(paste("Surv(days, Alzheimer_dementia) ~", formula_base)), "model1", status),
    run_coxph(data, as.formula(paste("Surv(days, Alzheimer_dementia) ~", formula_base, "+ Smoke + Alcohol + PA")), "model2", status),
    run_coxph(data, as.formula(paste("Surv(days, Alzheimer_dementia) ~", formula_base, "+ Smoke + Alcohol + PA + Diabetes + Hypertension + Medication")), "model3", status)
  )
  

  result_cognitive <- data.frame()
  

  models <- list(
    prospective_memory = binomial(),
    Fluid_intelligence = gaussian(),
    Reaction_time = gaussian(),
    Trail_Making_Test_A = gaussian(),
    Trail_Making_Test_B = gaussian(),
    Broken_letter_recognition = gaussian()
  )
  
  for (outcome in names(models)) {
    family <- models[[outcome]]
    outcome_data <- drop_na(data, !!sym(outcome))
    
    result_cognitive <- rbind(result_cognitive,
                              run_glm_or_gaussian(outcome_data, as.formula(paste(outcome, "~", formula_base)), "model1", status, family),
                              run_glm_or_gaussian(outcome_data, as.formula(paste(outcome, "~", formula_base, "+ Smoke + Alcohol + PA")), "model2", status, family),
                              run_glm_or_gaussian(outcome_data, as.formula(paste(outcome, "~", formula_base, "+ Smoke + Alcohol + PA + Diabetes + Hypertension + Medication")), "model3", status, family)
    )
  }
  
  list(cox = result_cox, cognitive = result_cognitive)
}


main_analysis <- function(data, subgroup_label, exclude_sex = FALSE) {
  diseases <- c("Painful_disease", "Painful_gums", "Toothache")
  status_labels <- c("Painful disease", "Painful gums", "Toothache")
  result_all_cox <- data.frame()
  result_all_cognitive <- data.frame()
  
  for (i in seq_along(diseases)) {
    disease <- diseases[i]
    status <- status_labels[i]
    results <- process_models(data, disease, status, exclude_sex)
    result_all_cox <- rbind(result_all_cox, results$cox)
    result_all_cognitive <- rbind(result_all_cognitive, results$cognitive)
  }
  
  save_results(result_all_cox, paste0("result_data/result_all_Alzheimer_dementia_cox_", subgroup_label, ".Rdata"))
  save_results(result_all_cognitive, paste0("result_data/result_all_Cognitive_function_", subgroup_label, ".Rdata"))
}


main_analysis(subset(Interpolation_data, Age < 65), "less_65")
main_analysis(subset(Interpolation_data, Age >= 65), "greater_equal_65")
main_analysis(subset(Interpolation_data, Sex == 0), "female", exclude_sex = TRUE)
main_analysis(subset(Interpolation_data, Sex == 1), "male", exclude_sex = TRUE)




load("result_data/result_all_Alzheimer_dementia_cox_less_65.Rdata")
result_less_65 <- result_df
result_less_65 $Subgroup="Age<65"
load("result_data/result_all_Alzheimer_dementia_cox_greater_equal_65.Rdata")
result_greater_equal_65 <- result_df
result_greater_equal_65 $Subgroup="Age>=65"
result_age_groups <- rbind(result_less_65, result_greater_equal_65)

load("result_data/result_all_Alzheimer_dementia_cox_female.Rdata")
result_female <- result_df
result_female$Subgroup <- "Female"

load("result_data/result_all_Alzheimer_dementia_cox_male.Rdata")
result_male <- result_df
result_male$Subgroup <- "Male"

result_sex_groups <- rbind(result_female, result_male)

result_model3 <- subset(result_age_groups, model == "model3")

model_category<-result_model3
rs_forest<-as.data.frame(cbind(model_category$status,round(model_category$HR,3),
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

rs_forest$CI<-paste0("(",rs_forest$V3,"-",rs_forest$V4,")")
rs_forest<-rs_forest[,c('V1','V2','CI','V5','V6','V7','V8')]
colnames(rs_forest)<-c('V1','V2','V3','V4','V5','V6','V7')
Table<-c("Model","HR","95% CI","P value",NA,NA,NA)
Table<-rbind(Table,c("Age<65","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[1:3,])
Table<-rbind(Table,c("Age>=65","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[4:6,])
forest<-Table
forest
paste(c(rep("T",2),rep("F",3),"T",rep("F",3),"T",rep("F",3),
        "T",rep("F",3)), sep=",",collapse = ",")
a1<-paste(forest[,1])
a2<-paste(forest[,2])
a3<-paste(forest[,3])
a4<-paste(forest[,4])
labeltext1=cbind(a1,a2,a3,a4)

pdf("Results/Fig. S3 Subgroup age.pdf",  height=3,width=11, onefile = FALSE)
forestplot(labeltext=labeltext1,
           graphwidth=unit(45,'mm'),
           mean = forest$V5,
           col=fpColors(line = "#CC79A7",
                        box="#D55E00"),
           lower=forest$V6,upper=forest$V7,is.summary=c(T,T,F,F,F,T,F,F,F,T,F,F,F,T,F,F,F),
           zero=1,boxsize=0.25,lineheight=unit(6,'mm'),colgap=unit(6,'mm'),xaxt = "n",  # 阻止绘制 x 轴
           xlab = "",cex.axis = 4,lwd.zero=2,lwd.ci=2,xticks=c(0.9,1.0,1.2,1.4,1.6,1.8),clip = c(.9,1.8),lwd.xaxis=3,lty.ci = "solid",graph.pos = 4)
#PDF:heigt=9,width=11
dev.off()


result_model3 <- subset(result_sex_groups, model == "model3")

model_category<-result_model3
rs_forest<-as.data.frame(cbind(model_category$status,round(model_category$HR,3),
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

rs_forest$CI<-paste0("(",rs_forest$V3,"-",rs_forest$V4,")")
rs_forest<-rs_forest[,c('V1','V2','CI','V5','V6','V7','V8')]
colnames(rs_forest)<-c('V1','V2','V3','V4','V5','V6','V7')
Table<-c("Model","HR","95% CI","P value",NA,NA,NA)
Table<-rbind(Table,c("Female","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[1:3,])
Table<-rbind(Table,c("Male","","","",NA,NA,NA))
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

pdf("Results/Fig. 4 Subgroup sex.pdf",  height=3,width=11, onefile = FALSE)
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

