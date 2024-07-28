# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
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
# >>>>> section 2.4 Pheatmap for Image ####
load(file="original data/Interpolation_data.Rdata")

Interpolation_data <- Interpolation_data %>% drop_na(IX_Cerebellum_right)
# Load necessary library
library(stats)
colnames(Interpolation_data)
# Regional grey matter volumes (FAST) 
colnames(Interpolation_data)[37:175]
Colnames <- as.data.frame(colnames(Interpolation_data)[37:175])
colnames(Colnames)<-"Description"
#Colnames$Brain_Region<-NULL


classify_brain_structure <- function(structure) {
  frontal_keywords <- c("Subcallosal_Cortex","Paracingulate_Gyrus","Juxtapositional_Lobule_Cortex","Frontal", "Precentral", "Orbital")
  parietal_keywords <- c("Angular_Gyrus","Postcentral", "Parietal", "Supramarginal", "Precuneous", "Intracalcarine")
  temporal_keywords <- c("Parahippocampal_Gyrus","Temporal", "Fusiform", "Heschls", "Planum")
  occipital_keywords <- c("Supracalcarine_Cortex","Lingual_Gyrus","Occipital", "Cuneal", "Calcarine")
  cerebellum_keywords <- c("Cerebellum", "Vermis")
  subcortical_keywords <- c("Ventral_Striatum","Thalamus", "Caudate", "Putamen", "Pallidum", "Amygdala", "Hippocampus")
  
  # 检查属于哪个区域
  if (any(sapply(frontal_keywords, grepl, structure))) {
    return("Frontal Lobe")
  } else if (any(sapply(parietal_keywords, grepl, structure))) {
    return("Parietal Lobe")
  } else if (any(sapply(temporal_keywords, grepl, structure))) {
    return("Temporal Lobe")
  } else if (any(sapply(occipital_keywords, grepl, structure))) {
    return("Occipital Lobe")
  } else if (any(sapply(cerebellum_keywords, grepl, structure))) {
    return("Cerebellum")
  } else if (any(sapply(subcortical_keywords, grepl, structure))) {
    return("Subcortical Structures")
  } else if (grepl("Cingulate_Gyrus", structure)) {
    return(ifelse(grepl("anterior", structure), "Frontal Lobe", "Parietal Lobe"))
  } else if (grepl("Brain_Stem", structure)) {
    return("Brain Stem")
  } else {
    return("Other")
  }
}


Colnames$Brain_Region <- sapply(Colnames$Description , classify_brain_structure)

head(Colnames)
Colnames$Description[Colnames$Brain_Region=="Subcortical Structures"]
table(Colnames$Brain_Region)
save(Colnames,file="result data/Colnames.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.1 Frontal Lobe ####

brain_structures <- Colnames$Description[Colnames$Brain_Region=="Frontal Lobe"] # add more names as per your data
#** section 2.4.1.1 Painful disease ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_disease<-as.numeric(Interpolation_data$Painful_disease)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_disease+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_disease", "Estimate"],
    SE = coef_summary["Painful_disease", "Std. Error"],
    P_value = coef_summary["Painful_disease", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PD<-results
results_PD$Group<-"Painful disease"
#** section 2.4.1.2 Painful gums ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_gums<-as.numeric(Interpolation_data$Painful_gums)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_gums+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_gums", "Estimate"],
    SE = coef_summary["Painful_gums", "Std. Error"],
    P_value = coef_summary["Painful_gums", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PG<-results
results_PG$Group<-"Painful gums"
#** section 2.4.1.3 Toothache ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Toothache<-as.numeric(Interpolation_data$Toothache)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Toothache+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Toothache", "Estimate"],
    SE = coef_summary["Toothache", "Std. Error"],
    P_value = coef_summary["Toothache", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_TA<-results
results_TA$Group<-"Toothache"
glm_Frontal_results<-rbind(results_PD,results_PG,results_TA)

save(glm_Frontal_results,file="result data/glm_Frontal_results.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.2 Parietal Lobe ####
load(file="result data/Colnames.Rdata")
brain_structures <- Colnames$Description[Colnames$Brain_Region=="Parietal Lobe"] # add more names as per your data
#** section 2.4.2.1 Painful disease ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_disease<-as.numeric(Interpolation_data$Painful_disease)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_disease+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_disease", "Estimate"],
    SE = coef_summary["Painful_disease", "Std. Error"],
    P_value = coef_summary["Painful_disease", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PD<-results
results_PD$Group<-"Painful disease"
#** section 2.4.1.2 Painful gums ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_gums<-as.numeric(Interpolation_data$Painful_gums)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_gums+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_gums", "Estimate"],
    SE = coef_summary["Painful_gums", "Std. Error"],
    P_value = coef_summary["Painful_gums", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PG<-results
results_PG$Group<-"Painful gums"
#** section 2.4.1.3 Toothache ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Toothache<-as.numeric(Interpolation_data$Toothache)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Toothache+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Toothache", "Estimate"],
    SE = coef_summary["Toothache", "Std. Error"],
    P_value = coef_summary["Toothache", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_TA<-results
results_TA$Group<-"Toothache"
glm_Parietal_results<-rbind(results_PD,results_PG,results_TA)

save(glm_Parietal_results,file="result data/glm_Parietal_results.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.3 Temporal Lobe ####
load(file="result data/Colnames.Rdata")
table(Colnames$Brain_Region)
brain_structures <- Colnames$Description[Colnames$Brain_Region=="Temporal Lobe"] # add more names as per your data
#** section 2.4.2.1 Painful disease ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_disease<-as.numeric(Interpolation_data$Painful_disease)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_disease+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_disease", "Estimate"],
    SE = coef_summary["Painful_disease", "Std. Error"],
    P_value = coef_summary["Painful_disease", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PD<-results
results_PD$Group<-"Painful disease"
#** section 2.4.1.2 Painful gums ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_gums<-as.numeric(Interpolation_data$Painful_gums)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_gums+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_gums", "Estimate"],
    SE = coef_summary["Painful_gums", "Std. Error"],
    P_value = coef_summary["Painful_gums", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PG<-results
results_PG$Group<-"Painful gums"
#** section 2.4.1.3 Toothache ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Toothache<-as.numeric(Interpolation_data$Toothache)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Toothache+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Toothache", "Estimate"],
    SE = coef_summary["Toothache", "Std. Error"],
    P_value = coef_summary["Toothache", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_TA<-results
results_TA$Group<-"Toothache"
glm_Temporal_results<-rbind(results_PD,results_PG,results_TA)

save(glm_Temporal_results,file="result data/glm_Temporal_results.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.4 Cerebellum ####
load(file="result data/Colnames.Rdata")
table(Colnames$Brain_Region)
brain_structures <- Colnames$Description[Colnames$Brain_Region=="Cerebellum"] # add more names as per your data
#** section 2.4.2.1 Painful disease ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_disease<-as.numeric(Interpolation_data$Painful_disease)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_disease+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_disease", "Estimate"],
    SE = coef_summary["Painful_disease", "Std. Error"],
    P_value = coef_summary["Painful_disease", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PD<-results
results_PD$Group<-"Painful disease"
#** section 2.4.1.2 Painful gums ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_gums<-as.numeric(Interpolation_data$Painful_gums)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_gums+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_gums", "Estimate"],
    SE = coef_summary["Painful_gums", "Std. Error"],
    P_value = coef_summary["Painful_gums", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PG<-results
results_PG$Group<-"Painful gums"
#** section 2.4.1.3 Toothache ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Toothache<-as.numeric(Interpolation_data$Toothache)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Toothache+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Toothache", "Estimate"],
    SE = coef_summary["Toothache", "Std. Error"],
    P_value = coef_summary["Toothache", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_TA<-results
results_TA$Group<-"Toothache"
glm_Cerebellum_results<-rbind(results_PD,results_PG,results_TA)

save(glm_Cerebellum_results,file="result data/glm_Cerebellum_results.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.5 Occipital Lobe ####
load(file="result data/Colnames.Rdata")
table(Colnames$Brain_Region)
brain_structures <- Colnames$Description[Colnames$Brain_Region=="Occipital Lobe"] # add more names as per your data
#** section 2.4.2.1 Painful disease ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_disease<-as.numeric(Interpolation_data$Painful_disease)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_disease+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_disease", "Estimate"],
    SE = coef_summary["Painful_disease", "Std. Error"],
    P_value = coef_summary["Painful_disease", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PD<-results
results_PD$Group<-"Painful disease"
#** section 2.4.1.2 Painful gums ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_gums<-as.numeric(Interpolation_data$Painful_gums)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_gums+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_gums", "Estimate"],
    SE = coef_summary["Painful_gums", "Std. Error"],
    P_value = coef_summary["Painful_gums", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PG<-results
results_PG$Group<-"Painful gums"
#** section 2.4.1.3 Toothache ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Toothache<-as.numeric(Interpolation_data$Toothache)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Toothache+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Toothache", "Estimate"],
    SE = coef_summary["Toothache", "Std. Error"],
    P_value = coef_summary["Toothache", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_TA<-results
results_TA$Group<-"Toothache"
glm_Occipital_results<-rbind(results_PD,results_PG,results_TA)

save(glm_Occipital_results,file="result data/glm_Occipital_results.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.6 Subcortical Structures ####
load(file="result data/Colnames.Rdata")
table(Colnames$Brain_Region)
brain_structures <- Colnames$Description[Colnames$Brain_Region=="Subcortical Structures"] # add more names as per your data
#** section 2.4.2.1 Painful disease ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_disease<-as.numeric(Interpolation_data$Painful_disease)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_disease+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_disease", "Estimate"],
    SE = coef_summary["Painful_disease", "Std. Error"],
    P_value = coef_summary["Painful_disease", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PD<-results
results_PD$Group<-"Painful disease"
#** section 2.4.1.2 Painful gums ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_gums<-as.numeric(Interpolation_data$Painful_gums)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_gums+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_gums", "Estimate"],
    SE = coef_summary["Painful_gums", "Std. Error"],
    P_value = coef_summary["Painful_gums", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PG<-results
results_PG$Group<-"Painful gums"
#** section 2.4.1.3 Toothache ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Toothache<-as.numeric(Interpolation_data$Toothache)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Toothache+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Toothache", "Estimate"],
    SE = coef_summary["Toothache", "Std. Error"],
    P_value = coef_summary["Toothache", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_TA<-results
results_TA$Group<-"Toothache"
glm_Subcortical_results<-rbind(results_PD,results_PG,results_TA)

save(glm_Subcortical_results,file="result data/glm_Subcortical_results.Rdata")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.7 Other Structures ####
load(file="result data/Colnames.Rdata")
table(Colnames$Brain_Region)
brain_structures <- Colnames$Description[Colnames$Brain_Region=="Brain Stem"|
                                           Colnames$Brain_Region=="Other"] # add more names as per your data
#** section 2.4.2.1 Painful disease ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_disease<-as.numeric(Interpolation_data$Painful_disease)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_disease+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_disease", "Estimate"],
    SE = coef_summary["Painful_disease", "Std. Error"],
    P_value = coef_summary["Painful_disease", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PD<-results
results_PD$Group<-"Painful disease"
#** section 2.4.1.2 Painful gums ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Painful_gums<-as.numeric(Interpolation_data$Painful_gums)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Painful_gums+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Painful_gums", "Estimate"],
    SE = coef_summary["Painful_gums", "Std. Error"],
    P_value = coef_summary["Painful_gums", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_PG<-results
results_PG$Group<-"Painful gums"
#** section 2.4.1.3 Toothache ####
load(file="original data/Interpolation_data.Rdata")
Interpolation_data$Toothache<-as.numeric(Interpolation_data$Toothache)
# Create an empty dataframe to store the results
results <- data.frame(
  Structure = character(),
  Beta = numeric(),
  SE = numeric(),
  P_value = numeric(),
  AIC = numeric(),
  BIC = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each brain structure
for (structure in brain_structures) {
  # Build the model
  formula <- as.formula(paste(structure, "~Toothache+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
  model <- glm(formula, family = gaussian(), data = Interpolation_data)
  
  # Extract coefficients
  coef_summary <- summary(model)$coefficients
  
  # Store the results
  temp_results <- data.frame(
    Structure = structure,
    Beta = coef_summary["Toothache", "Estimate"],
    SE = coef_summary["Toothache", "Std. Error"],
    P_value = coef_summary["Toothache", "Pr(>|t|)"],
    
    AIC = AIC(model),
    BIC = BIC(model)
  )
  
  # Append to the results dataframe
  results <- rbind(results, temp_results)
}

# Display the results

results$P_BF<- p.adjust(results$P_value, method = "bonferroni")
results$P_FDR<- p.adjust(results$P_value, method = "fdr")
results_TA<-results
results_TA$Group<-"Toothache"
glm_Other_results<-rbind(results_PD,results_PG,results_TA)

save(glm_Other_results,file="result data/glm_Other_results.Rdata")


load("result data/colnames.Rdata")
table(Colnames$Brain_Region)
load("result data/glm_Frontal_results.Rdata")
glm_Frontal_results$Structure_group<-"Frontal Lobe"
load("result data/glm_Temporal_results.Rdata")
glm_Temporal_results$Structure_group<-"Temporal Lobe"
load("result data/glm_Occipital_results.Rdata")
glm_Occipital_results$Structure_group<-"Occipital Lobe"
load("result data/glm_Parietal_results.Rdata")
glm_Parietal_results$Structure_group<-"Parietal Lobe"
load("result data/glm_Cerebellum_results.Rdata")
glm_Cerebellum_results$Structure_group<-"Cerebellum"
load("result data/glm_Subcortical_results.Rdata")
glm_Subcortical_results$Structure_group<-"Subcortical Structures"
load("result data/glm_Other_results.Rdata")

glm_Other_results$Structure_group<-"Other"

glm_all_results<-rbind(glm_Frontal_results,glm_Temporal_results,glm_Occipital_results,glm_Parietal_results,
                       glm_Cerebellum_results,glm_Subcortical_results,glm_Other_results)
glm_all_results$logP<--log10(glm_all_results$P_value)
glm_all_results$Structure<- gsub("_", " ",glm_all_results$Structure)
glm_all_results$Structure<- gsub("left", "(left)",glm_all_results$Structure)
glm_all_results$Structure<- gsub("right", "(right)",glm_all_results$Structure)
glm_all_results$Structure_group <- factor(glm_all_results$Structure_group, levels = c("Frontal Lobe","Parietal Lobe",
                                                                                      "Temporal Lobe","Occipital Lobe",
                                                                                      "Cerebellum","Subcortical Structures",
                                                                                      "Other"))
save(glm_all_results,file = "result data/glm_all_results.Rdata")