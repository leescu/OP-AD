library(ggplot2)

#* Section 3: Brain Structure Data ####
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
save(results,file = "Results final/results.Rdata")
load(file = "Results final/results.Rdata")
glm_all_results<-results

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1:Frontal ####
# 假设 results_FL 已经加载并包含所有结果
glm_all_results <- subset(results, Subgroup == "Frontal Lobe")



heatmap_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "BETA")
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
rownames(heatmap_matrix)

# Prepare P-value matrix
pvalue_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "p_value")
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

# Create column color annotation
annotation_col <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
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
annotation_col <- annotation_col[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_col) <- rownames(heatmap_matrix)
colnames(heatmap_matrix) <- gsub("_", " ", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("left", "(left)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("right", "(right)", colnames(heatmap_matrix))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. XX.pdf", height = 25, width =20)
pheatmap(t(heatmap_matrix), 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = t(significance_matrix), 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_col = c(3, 6, 9), 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_col = gsub(" - .*", "", rownames(annotation_col)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1:Parietal Lobe ####
# 假设 results_FL 已经加载并包含所有结果
glm_all_results <- subset(results, Subgroup == "Parietal Lobe")



heatmap_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "BETA")
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
rownames(heatmap_matrix)

# Prepare P-value matrix
pvalue_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "p_value")
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

# Create column color annotation
annotation_col <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
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
annotation_col <- annotation_col[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_col) <- rownames(heatmap_matrix)
colnames(heatmap_matrix) <- gsub("_", " ", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("left", "(left)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("right", "(right)", colnames(heatmap_matrix))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. S8.pdf", height = 25, width =20)
pheatmap(t(heatmap_matrix), 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = t(significance_matrix), 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_col = c(3, 6, 9), 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_col = gsub(" - .*", "", rownames(annotation_col)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1:Temporal Lobe ####
# 假设 results_FL 已经加载并包含所有结果
glm_all_results <- subset(results, Subgroup == "Temporal Lobe")



heatmap_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "BETA")
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
rownames(heatmap_matrix)

# Prepare P-value matrix
pvalue_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "p_value")
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

# Create column color annotation
annotation_col <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
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
annotation_col <- annotation_col[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_col) <- rownames(heatmap_matrix)
colnames(heatmap_matrix) <- gsub("_", " ", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("left", "(left)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("right", "(right)", colnames(heatmap_matrix))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. S9.pdf", height = 30, width =20)
pheatmap(t(heatmap_matrix), 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = t(significance_matrix), 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_col = c(3, 6, 9), 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_col = gsub(" - .*", "", rownames(annotation_col)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1:Occipital Lobe ####
# 假设 results_FL 已经加载并包含所有结果
glm_all_results <- subset(results, Subgroup == "Occipital Lobe")



heatmap_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "BETA")
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
rownames(heatmap_matrix)

# Prepare P-value matrix
pvalue_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "p_value")
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

# Create column color annotation
annotation_col <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
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
annotation_col <- annotation_col[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_col) <- rownames(heatmap_matrix)
colnames(heatmap_matrix) <- gsub("_", " ", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("left", "(left)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("right", "(right)", colnames(heatmap_matrix))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. S10A.pdf", height = 30, width =20)
pheatmap(t(heatmap_matrix), 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = t(significance_matrix), 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_col = c(3, 6, 9), 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_col = gsub(" - .*", "", rownames(annotation_col)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1:Cerebellum ####
# 假设 results_FL 已经加载并包含所有结果
glm_all_results <- subset(results, Subgroup == "Cerebellum")



heatmap_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "BETA")
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
rownames(heatmap_matrix)

# Prepare P-value matrix
pvalue_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "p_value")
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

# Create column color annotation
annotation_col <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
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
annotation_col <- annotation_col[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_col) <- rownames(heatmap_matrix)
colnames(heatmap_matrix) <- gsub("_", " ", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("left", "(left)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("right", "(right)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("vermis", "(vermis)", colnames(heatmap_matrix))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. S11.pdf", height = 30, width =20)
pheatmap(t(heatmap_matrix), 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = t(significance_matrix), 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_col = c(3, 6, 9), 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_col = gsub(" - .*", "", rownames(annotation_col)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1:Occipital Lobe ####
# 假设 results_FL 已经加载并包含所有结果
glm_all_results <- subset(results, Subgroup == "Occipital Lobe")



heatmap_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "BETA")
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
rownames(heatmap_matrix)

# Prepare P-value matrix
pvalue_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "p_value")
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

# Create column color annotation
annotation_col <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
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
annotation_col <- annotation_col[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_col) <- rownames(heatmap_matrix)
colnames(heatmap_matrix) <- gsub("_", " ", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("left", "(left)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("right", "(right)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("vermis", "(vermis)", colnames(heatmap_matrix))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. S12.pdf", height = 30, width =20)
pheatmap(t(heatmap_matrix), 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = t(significance_matrix), 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_col = c(3, 6, 9), 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_col = gsub(" - .*", "", rownames(annotation_col)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1:Subcortical Structures ####
# 假设 results_FL 已经加载并包含所有结果
glm_all_results <- subset(results, Subgroup == "Subcortical Structures")
heatmap_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "BETA")
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
rownames(heatmap_matrix)

# Prepare P-value matrix
pvalue_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "p_value")
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

# Create column color annotation
annotation_col <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
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
annotation_col <- annotation_col[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_col) <- rownames(heatmap_matrix)
colnames(heatmap_matrix) <- gsub("_", " ", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("left", "(left)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("right", "(right)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("vermis", "(vermis)", colnames(heatmap_matrix))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. S13a.pdf", height = 30, width =20)
pheatmap(t(heatmap_matrix), 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = t(significance_matrix), 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_col = c(3, 6, 9), 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_col = gsub(" - .*", "", rownames(annotation_col)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section 1:Other ####
# 假设 results_FL 已经加载并包含所有结果
glm_all_results <- subset(results, Subgroup == "Other")
heatmap_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "BETA")
heatmap_matrix <- as.matrix(heatmap_data[, -c(1, 2)])
rownames(heatmap_matrix) <- paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
paste(heatmap_data$Disease, heatmap_data$Exposure, sep=" - ")
rownames(heatmap_matrix)

# Prepare P-value matrix
pvalue_data <- dcast(glm_all_results, Disease + Exposure ~ Y, value.var = "p_value")
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

# Create column color annotation
annotation_col <- data.frame(Group = heatmap_data$Exposure, row.names = rownames(heatmap_matrix))
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
annotation_col <- annotation_col[order, , drop = FALSE]

# Ensure row names and annotation dataframe match
rownames(annotation_col) <- rownames(heatmap_matrix)
colnames(heatmap_matrix) <- gsub("_", " ", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("left", "(left)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("right", "(right)", colnames(heatmap_matrix))
colnames(heatmap_matrix) <- gsub("vermis", "(vermis)", colnames(heatmap_matrix))
# Ensure all plotting devices are closed
graphics.off()

# Plot heatmap
pdf("Results final/Fig. S13b.pdf", height = 30, width =20)
pheatmap(t(heatmap_matrix), 
         scale = "none", 
         cluster_rows = F, 
         border = F,
         cluster_cols = F, 
         display_numbers = t(significance_matrix), 
         fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         gaps_col = c(3, 6, 9), 
         annotation_col = annotation_col, 
         annotation_colors = annotation_colors,
         labels_col = gsub(" - .*", "", rownames(annotation_col)), # Show only disease names
         legend = TRUE, 
         legend_labels = list(title = "Beta value"), 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18)
dev.off()
