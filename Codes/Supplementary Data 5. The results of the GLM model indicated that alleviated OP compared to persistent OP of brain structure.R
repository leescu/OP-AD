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
write.csv(results,file = "Results final/Supplementary Data 5. The results of the GLM model indicated that alleviated OP compared to persistent OP of brain structure.csv")
