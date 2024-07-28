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
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.3 Frontal Lobe ####
#*** section 2.4.1.4.3 Pheatmap ####
load("result data/glm_Frontal_results.Rdata")

# Subset data
result_PD <- subset(glm_Frontal_results, Group == "Painful disease")
result_PG <- subset(glm_Frontal_results, Group == "Painful gums")
result_TA <- subset(glm_Frontal_results, Group == "Toothache")

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
pdf("Results final/Fig. S5.pdf", height =  32, width =12, onefile = FALSE)
pheatmap(t(test), scale = "none", display_numbers =t(matrix),cluster_rows = F, cluster_cols = F, border = F, fontsize_col = 25, 
         fontsize_row = 22, cellwidth = 50, cellheight = 50, fontsize_number = 30, 
         angle_col = 45, legend_position = "bottom", color = color_palette, 
         breaks = color_breaks, annotation_legend = TRUE, annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)
dev.off()
graphics.off()



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.3 Parietal Lobe ####
#*** section 2.4.1.4.3 Pheatmap ####
load("result data/glm_Parietal_results.Rdata")

# Subset data
result_PD <- subset(glm_Parietal_results, Group == "Painful disease")
result_PG <- subset(glm_Parietal_results, Group == "Painful gums")
result_TA <- subset(glm_Parietal_results, Group == "Toothache")

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
pdf("Results final/Fig. S6.pdf", height =  32, width =12, onefile = FALSE)
pheatmap(t(test), scale = "none", display_numbers =t(matrix),cluster_rows = F, cluster_cols = F, border = F, fontsize_col = 25, 
         fontsize_row = 22, cellwidth = 50, cellheight = 50, fontsize_number = 30, 
         angle_col = 45, legend_position = "bottom", color = color_palette, 
         breaks = color_breaks, annotation_legend = TRUE, annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)
dev.off()
graphics.off()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.3 Occipital Lobe ####
#*** section 2.4.1.4.3 Pheatmap ####
load("result data/glm_Occipital_results.Rdata")

# Subset data
result_PD <- subset(glm_Occipital_results, Group == "Painful disease")
result_PG <- subset(glm_Occipital_results, Group == "Painful gums")
result_TA <- subset(glm_Occipital_results, Group == "Toothache")

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
pdf("Results final/Fig. S7.pdf", height =  32, width =12, onefile = FALSE)
pheatmap(t(test), scale = "none", display_numbers =t(matrix),cluster_rows = F, cluster_cols = F, border = F, fontsize_col = 25, 
         fontsize_row = 22, cellwidth = 50, cellheight = 50, fontsize_number = 30, 
         angle_col = 45, legend_position = "bottom", color = color_palette, 
         breaks = color_breaks, annotation_legend = TRUE, annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)
dev.off()
graphics.off()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.3 Subcortical Structures ####
#*** section 2.4.1.4.3 Pheatmap ####
load("result data/glm_Subcortical_results.Rdata")

# Subset data
result_PD <- subset(glm_Subcortical_results, Group == "Painful disease")
result_PG <- subset(glm_Subcortical_results, Group == "Painful gums")
result_TA <- subset(glm_Subcortical_results, Group == "Toothache")

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
pdf("Results final/Fig. S8.pdf", height =  32, width =12, onefile = FALSE)
pheatmap(t(test), scale = "none", display_numbers =t(matrix),cluster_rows = F, cluster_cols = F, border = F, fontsize_col = 25, 
         fontsize_row = 22, cellwidth = 50, cellheight = 50, fontsize_number = 30, 
         angle_col = 45, legend_position = "bottom", color = color_palette, 
         breaks = color_breaks, annotation_legend = TRUE, annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)
dev.off()
graphics.off()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2.4.3 Other Structures ####
#*** section 2.4.1.4.3 Pheatmap ####
load("result data/glm_Other_results.Rdata")

# Subset data
result_PD <- subset(glm_Other_results, Group == "Painful disease")
result_PG <- subset(glm_Other_results, Group == "Painful gums")
result_TA <- subset(glm_Other_results, Group == "Toothache")

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
pdf("Results final/Fig. S9.pdf", height =  32, width =12, onefile = FALSE)
pheatmap(t(test), scale = "none", display_numbers =t(matrix),cluster_rows = F, cluster_cols = F, border = F, fontsize_col = 25, 
         fontsize_row = 22, cellwidth = 50, cellheight = 50, fontsize_number = 30, 
         angle_col = 45, legend_position = "bottom", color = color_palette, 
         breaks = color_breaks, annotation_legend = TRUE, annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)
dev.off()
graphics.off()
