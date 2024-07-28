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
#* section 1 A ####
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
load(file = "result data/glm_all_results.Rdata")

glm_PD_results<-subset(glm_all_results,Group=="Painful disease")
glm_PD_results$significant <- glm_PD_results$P_value < 0.05
threshold1 <- -log10(0.05)
threshold2 <- -log10(0.05/139)
my_colors <- c("#b4d9ec", "#fdfb97", "#b7cf9d", "#f7a6b5", "#ECC86C", "#BD4146", "#551F33")
glm_PD_results <- glm_PD_results[order(glm_PD_results$Structure_group,-glm_PD_results$logP), ]
glm_PD_results$ID<-1:length(glm_PD_results$Structure)
pdf("result data/Fig. 4a.pdf",  height=7,width=20, onefile = FALSE)

ggplot(glm_PD_results, aes(x =ID, y = logP, color = Structure_group)) +
  geom_hline(yintercept = threshold1, color = "red", linetype = "dashed", size = 2) +
  #geom_hline(yintercept = threshold1, color = "yellow", linetype = "dashed", size = 2) +
  geom_point(alpha = 1, size = 5) +  # 绘制点
  scale_color_manual(values = my_colors) +  #
  
  scale_x_discrete(labels = glm_all_results$Structure)+# 设置颜色方案
  labs(title = " ",
       x = "Structures",
       y = "-log10(p-value)",
       color = "Structure location") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # 取消主要网格线
    panel.grid.minor = element_blank(),  # 取消次要网格线
    axis.text.x = element_blank(),       # 取消X轴标签
    axis.ticks.x = element_blank() , 
    axis.text.y = element_text(size = 20),
    axis.line = element_line(size = 1.2, color = "black"),  # 加粗轴线
    axis.title.x = element_text(size = 20, face = "bold"),  
    axis.title.y = element_text(size = 20, face = "bold") , 
    legend.title = element_text(size = 20),  # 调整图例标题的大小
    legend.text = element_text(size = 20)    # 调整图例文本的大小
  )
dev.off()
graphics.off()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2 B ####
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

#*** section 2.4.1.4.3 Pheatmap ####
load("result data/glm_Temporal_results.Rdata")

# Subset data
result_PD <- subset(glm_Temporal_results, Group == "Painful disease")
result_PG <- subset(glm_Temporal_results, Group == "Painful gums")
result_TA <- subset(glm_Temporal_results, Group == "Toothache")

# Combine and transpose data
test <- as.data.frame(t(cbind(result_PD$Beta, result_PG$Beta, result_TA$Beta)))
test.p <- as.data.frame(t(cbind(result_PD$P_value, result_PG$P_value, result_TA$P_value)))

# Set row and column names
rownames(test) <- c("Oral pain", "Painful gums", "Toothache")
colnames(test) <- result_PD$Structure
rownames(test) <- gsub("_", " ", rownames(test))
colnames(test) <- gsub("_", " ", colnames(test))
colnames(test) <- gsub("left", "(left)", colnames(test))
colnames(test) <- gsub("right", "(right)", colnames(test))


# Calculate color breaks and palette
min_value <- min(test, na.rm = TRUE)
max_value <- max(test, na.rm = TRUE)
mid_value <- 0
color_breaks <- c(seq(min_value, mid_value, length.out = 50), 
                  seq(mid_value + 1e-5, max_value, length.out = 50))
color_palette <- colorRampPalette(c("blue", "white", "red"))(99)

# Create matrix for significance
matrix <- ifelse(test.p < 0.05, 
                 ifelse(test.p < 0.01, 
                        ifelse(test.p < 0.001, "***", "**"), "*"), "")

legend_labels <- list(title = "Beta value")
legend_labels 
# Create heatmap
pdf("Results final/Fig. 4b.pdf", height =  32, width =12, onefile = FALSE)
pheatmap(t(test), scale = "none", display_numbers =t(matrix),cluster_rows = F, cluster_cols = F, border = F, fontsize_col = 25, 
         fontsize_row = 22, cellwidth = 50, cellheight = 50, fontsize_number = 30, 
         angle_col = 45, legend_position = "bottom", color = color_palette, 
         breaks = color_breaks, annotation_legend = TRUE, annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)
dev.off()
graphics.off()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 3 C  ####
#*** section 2.4.1.4.3 Pheatmap ####
load("result data/glm_Cerebellum_results.Rdata")

# Subset data
result_PD <- subset(glm_Cerebellum_results, Group == "Painful disease")
result_PG <- subset(glm_Cerebellum_results, Group == "Painful gums")
result_TA <- subset(glm_Cerebellum_results, Group == "Toothache")

# Combine and transpose data
test <- as.data.frame(t(cbind(result_PD$Beta, result_PG$Beta, result_TA$Beta)))
test.p <- as.data.frame(t(cbind(result_PD$P_value, result_PG$P_value, result_TA$P_value)))

# Set row and column names
rownames(test) <- c("Oral pain", "Painful gums", "Toothache")
colnames(test) <- result_PD$Structure
rownames(test) <- gsub("_", " ", rownames(test))
colnames(test) <- gsub("_", " ", colnames(test))
colnames(test) <- gsub("left", "(left)", colnames(test))
colnames(test) <- gsub("right", "(right)", colnames(test))
colnames(test) <- gsub("vermis", "(vermis)", colnames(test))
# Calculate color breaks and palette
min_value <- min(test, na.rm = TRUE)
max_value <- max(test, na.rm = TRUE)
mid_value <- 0
color_breaks <- c(seq(min_value, mid_value, length.out = 50), 
                  seq(mid_value + 1e-5, max_value, length.out = 50))
color_palette <- colorRampPalette(c("blue", "white", "red"))(99)

# Create matrix for significance
matrix <- ifelse(test.p < 0.05, 
                 ifelse(test.p < 0.01, 
                        ifelse(test.p < 0.001, "***", "**"), "*"), "")

legend_labels <- list(title = "Beta value")
legend_labels 
# Create heatmap
pdf("Results final/Fig. 4c.pdf", height =  32, width =12, onefile = FALSE)
pheatmap(t(test), scale = "none", display_numbers =t(matrix),cluster_rows = F, cluster_cols = F, border = F, fontsize_col = 25, 
         fontsize_row = 22, cellwidth = 50, cellheight = 50, fontsize_number = 30, 
         angle_col = 45, legend_position = "bottom", color = color_palette, 
         breaks = color_breaks, annotation_legend = TRUE, annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)
dev.off()
graphics.off()


