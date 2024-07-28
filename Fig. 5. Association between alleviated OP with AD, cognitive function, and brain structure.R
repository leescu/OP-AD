# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
# Section 0: Load Packages
library(survival)
library(stats)
library(reshape2)
library(pheatmap)
library(gridExtra)
library(ggplot2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1: a ####
#** Analysis ####
load(file="original data/Interpolation_data_selected_COX.Rdata")

# Define independent variables
independent_vars <- c("Oral_pain_all", "Oral_pain_con", "Oral_pain_non", "Oral_pain_vs",
                      "Pain_gums_all", "Pain_gums_con", "Pain_gums_non", "Pain_gums_vs",
                      "Tooth_ache_all", "Tooth_ache_con", "Tooth_ache_non", "Tooth_ache_vs")

# Initialize an empty dataframe to store results
results <- data.frame(Variable=character(), OR=numeric(), CI_lower=numeric(), CI_upper=numeric(), p_value=numeric(), stringsAsFactors=FALSE)

# Loop through each independent variable
for (var in independent_vars) {
  # Build formula
  formula <- as.formula(paste("Surv(days, Alzheimer_dementia) ~", var, "+ Age + Ethnicity + Education + BMI_status + TDI_quantile + Smoke + Alcohol + PA + Diabetes + Hypertension + Medication"))
  
  # Run Cox regression model
  cox_model <- coxph(formula, data=Matched_Interpolation_data)
  
  # Extract results
  summary_cox <- summary(cox_model)
  or_value <- exp(coef(cox_model)[var])
  ci <- exp(confint(cox_model)[var, ])
  p_value <- summary_cox$coefficients[var, "Pr(>|z|)"]
  
  # Add results to dataframe
  results <- rbind(results, data.frame(Variable=var, OR=or_value, CI_lower=ci[1], CI_upper=ci[2], p_value=p_value))
}

# Round p-values
results$p<-round(results$p_value,5)
results

save(results,file="Results/result_COX.Rdata")

#**  Plotting ####
load(file="Results/result_COX.Rdata")
head(results)

model_category<-results
rs_forest<-as.data.frame(cbind(model_category$Variable,round(model_category$OR,3),
                               round(model_category$CI_lower,3),round(model_category$CI_upper,3),round(model_category$p_value,3),
                               model_category$OR,model_category$CI_lower,model_category$CI_upper),
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
rs_forest$V1<-c("Oral pain vs Oral pain-free","Alleviated oral pain vs Oral pain-free","Persistent oral pain vs Oral pain-free","Alleviated oral pain vs Persistent oral pain",
                "Oral pain vs Oral pain-free","Alleviated oral pain vs Oral pain-freen","Persistent oral pain vs Oral pain-free","Alleviated oral pain vs Persistent oral pain",
                "Oral pain vs Oral pain-free","Alleviated oral pain vs Oral pain-free","Persistent oral pain vs Oral pain-free","Alleviated oral pain vs Persistent oral pain")
Table<-c("Model","HR","95% CI","P value",NA,NA,NA)
Table<-rbind(Table,c("Oral pain","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[1:4,])
Table<-rbind(Table,c("Painful gums","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[5:8,])
Table<-rbind(Table,c("Toothache","","","",NA,NA,NA))
Table<-rbind(Table,rs_forest[9:12,])
forest<-Table
forest
paste(c(rep("T",2),rep("F",4),"T",rep("F",4),"T",rep("F",4),
        "T",rep("F",4)), sep=",",collapse = ",")
a1<-paste(forest[,1])
a2<-paste(forest[,2])
a3<-paste(forest[,3])
a4<-paste(forest[,4])
labeltext=cbind(a1,a2,a3,a4)

pdf("Results final/Fig. 5a.pdf",  height=4,width=11, onefile = FALSE)
forestplot(labeltext=labeltext,
           graphwidth=unit(45,'mm'),
           mean = forest$V5,
           col=fpColors(line = "#CC79A7",
                        box="#D55E00"),
           lower=forest$V6,upper=forest$V7,is.summary=c(T,T,F,F,F,F,T,F,F,F,F,T,F,F,F,F,T,F,F,F,F),
           zero=1,boxsize=0.25,lineheight=unit(6,'mm'),colgap=unit(6,'mm'),xaxt = "n",  # Suppress x-axis
           xlab = "",cex.axis = 4,lwd.zero=2,lwd.ci=2,xticks=c(0,2,4,6),clip = c(-1,7),lwd.xaxis=3,lty.ci = "solid",graph.pos = 4)
# PDF: height=9, width=11
dev.off()




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1: b ####
#** Analysis ####
load(file="original data/Interpolation_data_selected_Other.Rdata")
load(file = "original data/Cognitive_data_new.Rdata")

Matched_Interpolation_data[,c(28:39)]<-NULL
Matched_Interpolation_data<-merge(Matched_Interpolation_data,Cognitive_data_final,by = "eid",all.x = T)

Matched_Interpolation_data$Reaction_time_1<-Matched_Interpolation_data$Reaction_time_1/1000
Matched_Interpolation_data$Reaction_time_2<-Matched_Interpolation_data$Reaction_time_2/1000
Matched_Interpolation_data$Trail_Making_Test_A<-Matched_Interpolation_data$Trail_Making_Test_A/100
Matched_Interpolation_data$Trail_Making_Test_B<-Matched_Interpolation_data$Trail_Making_Test_B/100


normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

Matched_Interpolation_data[, 273:284] <- lapply(Matched_Interpolation_data[, 273:284], normalize)

# Verify the result
summary(Matched_Interpolation_data[273:284])

colnames(Matched_Interpolation_data[273:284])


independent_vars <- c("Oral_pain_all", "Oral_pain_con", "Oral_pain_non", "Oral_pain_vs",
                      "Pain_gums_all", "Pain_gums_con", "Pain_gums_non", "Pain_gums_vs",
                      "Tooth_ache_all", "Tooth_ache_con", "Tooth_ache_non", "Tooth_ache_vs")
colnames(Matched_Interpolation_data)[269:284]
col<-colnames(Matched_Interpolation_data)[273:284]
# Define Y variables
y_vars <-col[c(2,4,9:12,6:8)]

y_vars 
# Initialize an empty dataframe to store results
results <- data.frame(Disease=character(), Exposure=character(), Y=character(), BETA=numeric(), SE=numeric(), p_value=numeric(), stringsAsFactors=FALSE)

# Function to split X variable
split_x_var <- function(x) {
  parts <- strsplit(x, "_")[[1]]
  disease <- paste(parts[1], parts[2], sep="_")
  exposure <- parts[3]
  
  # Describe disease variable
  if (disease == "Oral_pain") {
    disease_desc <- "Oral pain"
  } else if (disease == "Pain_gums") {
    disease_desc <- "Painful gums"
  } else if (disease == "Tooth_ache") {
    disease_desc <- "Toothache"
  } else {
    disease_desc <- "Unknown"
  }
  
  # Describe exposure variable
  if (exposure == "all") {
    exposure_desc <- "All pain vs No pain"
  } else if (exposure == "con") {
    exposure_desc <- "Controlled pain vs No pain"
  } else if (exposure == "non") {
    exposure_desc <- "No controlled pain vs No pain"
  } else if (exposure == "vs") {
    exposure_desc <- "Controlled pain vs No controlled pain"
  } else {
    exposure_desc <- "Unknown"
  }
  
  return(c(disease_desc, exposure_desc))
}

# Loop through each Y variable
for (y in y_vars) {
  # Select model type based on Y variable
  if (y == "Prospective_memory") {
    family <- binomial()
    p_col <- "Pr(>|z|)"
  } else {
    family <- gaussian()
    p_col <- "Pr(>|t|)"
  }
  
  # Loop through each independent variable
  for (x in independent_vars) {
    # Build formula
    formula <- as.formula(paste(y, "~", x, "+ Age + Ethnicity + Education + BMI_status + TDI_quantile + Smoke + Alcohol + PA + Diabetes + Hypertension + Medication"))
    
    # Run GLM model
    glm_model <- glm(formula, data=Matched_Interpolation_data, family=family)
    
    # Extract results
    summary_glm <- summary(glm_model)
    
    if (x %in% rownames(summary_glm$coefficients)) {
      beta_value <- coef(glm_model)[x]
      se_value <- summary_glm$coefficients[x, "Std. Error"]
      p_value <- summary_glm$coefficients[x, p_col]
      
      # Split X variable
      x_split <- split_x_var(x)
      disease <- x_split[1]
      exposure <- x_split[2]
      
      # Add results to dataframe and remove underscores from Y variable names
      y_result <- gsub("_", " ", y)
      results <- rbind(results, data.frame(Disease=disease, Exposure=exposure, Y=y_result, BETA=beta_value, SE=se_value, p_value=p_value))
    }
  }
}

# Show results
print(results)
write.csv(results,file = "Results final/aa.csv")
#** Plotting ####
# Assume results data is stored in results dataframe
# Create matrix of Beta values
heatmap_data <- dcast(results, Disease + Exposure ~ Y, value.var = "BETA")
colnames(heatmap_data)
heatmap_data<-heatmap_data[,c(1,2,7,4,5,3,6,9,8,10,11)]
colnames(heatmap_data)
colnames(heatmap_data)<-c("Disease","Exposure","Prospective memory (Follow−up)","Fluid intelligence (Follow−up)",
                          "Matrix pattern completion","Broken letter recognition",
                          "Picture vocabulary","Tower rearranging",        
                          "Reaction time (Follow−up)","Trail Making Test A","Trail Making Test B")
colnames(heatmap_data)
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")

# Prepare P-value matrix
pvalue_data <- dcast(results, Disease + Exposure ~ Y, value.var = "p_value")
pvalue_data<-pvalue_data[,c(1,2,7,4,5,3,6,9,8,10,11)]
colnames(pvalue_data)<-c("Disease","Exposure","Prospective memory (Follow−up)","Fluid intelligence (Follow−up)",
                          "Matrix pattern completion","Broken letter recognition",
                          "Picture vocabulary","Tower rearranging",        
                          "Reaction time (Follow−up)","Trail Making Test A","Trail Making Test B")
pvalue_matrix <- as.matrix(pvalue_data[, -c(1, 2)])
rownames(pvalue_matrix) <- paste(pvalue_data$Disease, pvalue_data$Exposure, sep=" - ")

# Annotate significance
significance_matrix <- ifelse(pvalue_matrix < 0.05, ifelse(pvalue_matrix < 0.01, ifelse(pvalue_matrix < 0.001, "***", "**"), "*"), "")

# Create color list
group_colors <- c(
  "All pain vs No pain" = "#BD4146",
  "Controlled pain vs No pain" = "#ECC68C",
  "No controlled pain vs No pain" = "#86D3DE",
  "Controlled pain vs No controlled pain" = "#4CAF50"
)

# Create row color annotation
annotation_row <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
annotation_colors <- list(Group = group_colors,
                          Type= c("High performance" = "#F0A19A", "Efficient completion" = "#5086c4"))

# Adjust color scheme
min_value <- min(heatmap_matrix, na.rm = TRUE)
max_value <- max(heatmap_matrix, na.rm = TRUE)
mid_value <- 0
color_breaks <- c(seq(min_value, mid_value, length.out = 50), 
                  seq(mid_value + 1e-5, max_value, length.out = 50))
color_palette <- colorRampPalette(c("blue", "white", "red"))(100)

# Sort by Exposure type
order <- order(factor(heatmap_data$Exposure, levels = c("All pain vs No pain", "Controlled pain vs No pain", "No controlled pain vs No pain", "Controlled pain vs No controlled pain")))
heatmap_matrix <- heatmap_matrix[order, ]
significance_matrix <- significance_matrix[order, ]
annotation_row <- annotation_row[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_row) <- rownames(heatmap_matrix)
annotation_col <- data.frame(Type = c(rep("Efficient completion", 6), rep("High performance", 3)))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. 5B.pdf", height = 16, width = 25)
pheatmap(heatmap_matrix, 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = significance_matrix, 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_row = c(3, 6, 9), 
         annotation_row = annotation_row, 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_row = gsub(" - .*", "", rownames(annotation_row)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         gaps_col = 6, 
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 3: c ####
#** Analysis ####
# Load necessary packages
load(file="result data/Colnames.Rdata")
load(file="original data/Interpolation_data_selected_Other.Rdata")
# Define independent variables
independent_vars <- c("Oral_pain_all", "Oral_pain_con", "Oral_pain_non", "Oral_pain_vs",
                      "Pain_gums_all", "Pain_gums_con", "Pain_gums_non", "Pain_gums_vs",
                      "Tooth_ache_all", "Tooth_ache_con", "Tooth_ache_non", "Tooth_ache_vs")

# Define brain region groups
brain_regions <- list(
  "Frontal Lobe" = Colnames$Description[Colnames$Brain_Region == "Frontal Lobe"],
  "Other" = Colnames$Description[Colnames$Brain_Region %in% c("Brain Stem", "Other")],
  "Cerebellum" = Colnames$Description[Colnames$Brain_Region == "Cerebellum"],
  "Occipital Lobe" = Colnames$Description[Colnames$Brain_Region == "Occipital Lobe"],
  "Parietal Lobe" = Colnames$Description[Colnames$Brain_Region == "Parietal Lobe"],
  "Subcortical Structures" = Colnames$Description[Colnames$Brain_Region == "Subcortical Structures"],
  "Temporal Lobe" = Colnames$Description[Colnames$Brain_Region == "Temporal Lobe"]
)

# Initialize an empty dataframe to store results
results <- data.frame(Disease=character(), Exposure=character(), Y=character(), BETA=numeric(), SE=numeric(), p_value=numeric(), Group=character(), Subgroup=character(), stringsAsFactors=FALSE)

# Function to split X variable
split_x_var <- function(x) {
  parts <- strsplit(x, "_")[[1]]
  disease <- paste(parts[1], parts[2], sep="_")
  exposure <- parts[3]
  
  if (disease == "Oral_pain") {
    disease_desc <- "Oral pain"
  } else if (disease == "Pain_gums") {
    disease_desc <- "Painful gums"
  } else if (disease == "Tooth_ache") {
    disease_desc <- "Toothache"
  } else {
    disease_desc <- "Unknown"
  }
  
  if (exposure == "all") {
    exposure_desc <- "All pain vs No pain"
  } else if (exposure == "con") {
    exposure_desc <- "Controlled pain vs No pain"
  } else if (exposure == "non") {
    exposure_desc <- "No controlled pain vs No pain"
  } else if (exposure == "vs") {
    exposure_desc <- "Controlled pain vs No controlled pain"
  } else {
    exposure_desc <- "Unknown"
  }
  
  return(c(disease_desc, exposure_desc))
}

# Loop through each Y variable (brain structures)
for (group in names(brain_regions)) {
  for (y in brain_regions[[group]]) {
    # Choose model type based on Y variable
    family <- gaussian()
    p_col <- "Pr(>|t|)"
    
    # Loop through each independent variable
    for (x in independent_vars) {
      # Build formula
      formula <- as.formula(paste(y, "~", x, "+ Age + Ethnicity + Education + BMI_status + TDI_quantile + Smoke + Alcohol + PA + Diabetes + Hypertension + Medication"))
      
      # Run GLM model
      glm_model <- glm(formula, data=Matched_Interpolation_data, family=family)
      
      # Extract results
      summary_glm <- summary(glm_model)
      
      if (x %in% rownames(summary_glm$coefficients)) {
        beta_value <- coef(glm_model)[x]
        se_value <- summary_glm$coefficients[x, "Std. Error"]
        p_value <- summary_glm$coefficients[x, p_col]
        
        # Split X variable
        x_split <- split_x_var(x)
        disease <- x_split[1]
        exposure <- x_split[2]
        
        # Add results to dataframe and add grouping information
        results <- rbind(results, data.frame(Disease=disease, Exposure=exposure, Y=y, BETA=beta_value, SE=se_value, p_value=p_value, Group="Brain Structure", Subgroup=group))
      }
    }
  }
}

# Show results
print(results)
#** Plotting ####

#*** Plotting - Heatmap ####
# Assume results data is stored in results dataframe

# Prepare data for heatmap
for (group in names(brain_regions)) {
  subset_results <- subset(results, Subgroup == group)
  heatmap_data <- dcast(subset_results, Disease + Exposure ~ Y, value.var = "BETA")
  heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
  rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
  
  # Prepare P-value matrix
  pvalue_data <- dcast(subset_results, Disease + Exposure ~ Y, value.var = "p_value")
  pvalue_matrix <- as.matrix(pvalue_data[, -c(1, 2)])
  rownames(pvalue_matrix) <- paste(pvalue_data$Disease, pvalue_data$Exposure, sep=" - ")
  
  # Annotate significance
  significance_matrix <- ifelse(pvalue_matrix < 0.05, ifelse(pvalue_matrix < 0.01, ifelse(pvalue_matrix < 0.001, "***", "**"), "*"), "")
  
  # Create color list
  group_colors <- c(
    "All pain vs No pain" = "#BD4146",
    "Controlled pain vs No pain" = "#ECC68C",
    "No controlled pain vs No pain" = "#86D3DE",
    "Controlled pain vs No controlled pain" = "#4CAF50"
  )
  
  # Create row color annotation
  annotation_row <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
  annotation_colors <- list(Group = group_colors)
  
  # Adjust color scheme
  min_value <- min(heatmap_matrix, na.rm = TRUE)
  max_value <- max(heatmap_matrix, na.rm = TRUE)
  mid_value <- 0
  color_breaks <- c(seq(min_value, mid_value, length.out = 50), 
                    seq(mid_value + 1e-5, max_value, length.out = 50))
  color_palette <- colorRampPalette(c("blue", "white", "red"))(99)
  
  # Sort by Exposure type
  order <- order(factor(heatmap_data$Exposure, levels = c("All pain vs No pain", "Controlled pain vs No pain", "No controlled pain vs No pain", "Controlled pain vs No controlled pain")))
  heatmap_matrix <- heatmap_matrix[order, ]
  significance_matrix <- significance_matrix[order, ]
  annotation_row <- annotation_row[order, , drop = FALSE]
  
  # Ensure row names and annotation dataframe match
  rownames(annotation_row) <- rownames(heatmap_matrix)
  
  # Calculate heatmap width
  num_brain_structures <- ncol(heatmap_matrix)
  heatmap_width <- max(25, num_brain_structures * 1.5)  # Ensure minimum width is 25, adjust based on number of brain structures
  
  # Ensure all plotting devices are closed
  graphics.off()
  
  # Plot heatmap
  pdf(paste0("Results/Fig. 5b ", group, ".pdf"), height = 16, width = heatmap_width)
  pheatmap(heatmap_matrix, 
           scale = "none", 
           cluster_rows = F, 
           border = F,
           cluster_cols = F, 
           display_numbers = significance_matrix, 
           fontsize_col = 18, 
           fontsize_row = 18, 
           cellwidth = 50, 
           cellheight = 50, 
           fontsize_number = 30, 
           angle_col = 45, 
           gaps_row = c(3, 6, 9),  # Divide into 4 blocks
           annotation_row = annotation_row, 
           annotation_colors = annotation_colors,
           labels_row = gsub(" - .*", "", rownames(annotation_row)), # Show only disease names
           legend = TRUE, 
           legend_labels = list(title = "Beta value"), 
           legend_position = "bottom",
           color = color_palette, 
           breaks = color_breaks)
  
  dev.off()
}

#*** Plotting - LogP Values ####

# Assume results data is loaded and contains all results
glm_all_results <- subset(results, Disease == "Oral pain")

# Calculate -log10(p-value)
glm_all_results$logP <- -log10(glm_all_results$p_value)
glm_all_results$Structure <- gsub("_", " ", glm_all_results$Y)
glm_all_results$Structure <- gsub("left", "(left)", glm_all_results$Structure)
glm_all_results$Structure <- gsub("right", "(right)", glm_all_results$Structure)
glm_all_results$Structure_group <- factor(glm_all_results$Subgroup, levels = c("Frontal Lobe","Parietal Lobe",
                                                                               "Temporal Lobe","Occipital Lobe",
                                                                               "Cerebellum","Subcortical Structures",
                                                                               "Other"))

# Save results
if (!dir.exists("Results")) {
  dir.create("Results")
}
save(glm_all_results, file = "Results/glm_all_results.Rdata")

load(file = "Results/glm_all_results.Rdata")
write.csv(glm_all_results,'Results final/Table S6.csv')
# Process four groups separately
groups <- c("All pain vs No pain", "Controlled pain vs No pain", "No controlled pain vs No pain", "Controlled pain vs No controlled pain")
titles <- c("Oral pain vs Oral pain-free","Alleviated oral pain vs Oral pain-free","Persistent oral pain vs Oral pain-free","Alleviated oral pain vs Persistent oral pain")
colors <- c("#b4d9ec", "#fdfb97", "#b7cf9d", "#f7a6b5", "#ECC86C", "#BD4146", "#551F33")
threshold1 <- -log10(0.05)

plots <- list()

for (i in 1:4) {
  group_name <- groups[i]
  plot_title <- titles[i]
  
  glm_group_results <- subset(glm_all_results, Exposure == group_name)
  glm_group_results$significant <- glm_group_results$p_value < 0.05
  glm_group_results <- glm_group_results[order(glm_group_results$Structure_group, -glm_group_results$logP), ]
  glm_group_results$ID <- 1:length(glm_group_results$Structure)
  
  p <- ggplot(glm_group_results, aes(x = ID, y = logP, color = Structure_group)) +
    geom_hline(yintercept = threshold1, color = "red", linetype = "dashed", size = 2) +
    geom_point(alpha = 1, size = 5) +
    scale_color_manual(values = colors) +
    labs(title = plot_title,
         x = "Structures",
         y = "-log10(p-value)",
         color = "Structure location") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = 20),
      axis.line = element_line(size = 1.2, color = "black"),
      axis.title.x = element_text(size = 20, face = "bold"),
      axis.title.y = element_text(size = 20, face = "bold"),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20),
      plot.title = element_text(size = 22, face = "bold")
    )
  
  plots[[i]] <- p
}

# Combine four subplots into one canvas, layout 2x2
pdf("Results final/Fig. 5c.pdf", height = 20, width = 10)
grid.arrange(grobs = plots, ncol = 1)
dev.off()
graphics.off()

