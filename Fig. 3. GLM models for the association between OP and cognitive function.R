setwd("M:/OP&AZ/Cohort data")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 1 ####
load(file="original data/Interpolation_data.Rdata")
colnames(Interpolation_data)
Interpolation_data$Reaction_time_1<-Interpolation_data$Reaction_time_1/1000
Interpolation_data$Reaction_time_2<-Interpolation_data$Reaction_time_2/1000
Interpolation_data$Trail_Making_Test_A<-Interpolation_data$Trail_Making_Test_A/100
Interpolation_data$Trail_Making_Test_B<-Interpolation_data$Trail_Making_Test_B/100

data<-Interpolation_data[,30:39]
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

Interpolation_data[, 30:39] <- lapply(Interpolation_data[, 30:39], normalize)

# Verify the result
summary(Interpolation_data[, 30:39])

colnames(Interpolation_data[, 30:39])







# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####

#### Painful disease ####
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0(x,"~Painful_disease+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+
             Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
    glm_poisson<-glm(FML,data=Interpolation_data, family=gaussian())
    summary<-summary(glm_poisson)$coefficients
    Beta = summary["Painful_disease1", "Estimate"]
    SE =summary["Painful_disease1", "Std. Error"]
    P_value = summary["Painful_disease1", "Pr(>|t|)"]
    Uni_glm_model <- data.frame('Characteristics'=x,
                                'Beta' = Beta,
                                'SE'=SE,
                                'P' = P_value)
    return(Uni_glm_model)
  }  


variable.names<- colnames(Interpolation_data)[28:39];variable.names
Uni_glm<- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
rownames(Uni_glm)<-Uni_glm$Characteristics
Uni_glm <- Uni_glm[,-1]
Uni_glm
Painful_disease_test<-Uni_glm
save(Painful_disease_test,file="Results final/Painful_disease_test_glm.Rdata")
#Painful_disease_test$fdr<-p.adjust(Painful_disease_test$P,method ="BH")

#### Painful_gums ####
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0(x,"~Painful_gums+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
    glm_poisson<-glm(FML,data=Interpolation_data,family=gaussian())
    summary<-summary(glm_poisson)$coefficients
    Beta = summary["Painful_gums1", "Estimate"]
    SE =summary["Painful_gums1", "Std. Error"]
    P_value = summary["Painful_gums1", "Pr(>|t|)"]
    Uni_glm_model <- data.frame('Characteristics'=x,
                                'Beta' = Beta,
                                'SE'=SE,
                                'P' = P_value
                                 )
    return(Uni_glm_model)
  }  
variable.names<- colnames(Interpolation_data)[28:39];variable.names
Uni_glm<- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
rownames(Uni_glm)<-Uni_glm$Characteristics
Uni_glm <- Uni_glm[,-1]
Uni_glm
Painful_gums_test<-Uni_glm
save(Painful_gums_test,file="Results final/Painful_gums_test_glm.Rdata")


#### Toothache ####
Uni_glm_model<- 
  function(x){
    FML<-as.formula(paste0(x,"~Toothache+Age+Sex+Ethnicity+Education+BMI_status+TDI_quantile+Smoke+Alcohol+PA+Diabetes+Hypertension+Medication"))
    glm_poisson<-glm(FML,data=Interpolation_data,family=gaussian())
    summary<-summary(glm_poisson)$coefficients
    Beta = summary["Toothache1", "Estimate"]
    SE =summary["Toothache1", "Std. Error"]
    P_value = summary["Toothache1", "Pr(>|t|)"]
    Uni_glm_model <- data.frame('Characteristics'=x,
                                'Beta' = Beta,
                                'SE'=SE,
                                'P' = P_value )
    
    return(Uni_glm_model)
  }  
variable.names<- colnames(Interpolation_data)[28:39];variable.names
Uni_glm<- lapply(variable.names, Uni_glm_model)
Uni_glm<- ldply(Uni_glm,data.frame);Uni_glm
rownames(Uni_glm)<-Uni_glm$Characteristics
Uni_glm <- Uni_glm[,-1]
Uni_glm
Toothache_test<-Uni_glm
save(Toothache_test,file="Results final/Toothache_test_glm.Rdata")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 2   ####
load(file="Results final/Painful_disease_test_glm.Rdata")
load(file="Results final/Painful_gums_test_glm.Rdata")
load(file="Results final/Toothache_test_glm.Rdata")
Painful_disease_test[1:6,]

Painful_disease_test$Beta<-round(Painful_disease_test$Beta,3)
Painful_disease_test$SE<-round(Painful_disease_test$SE,3)
Painful_disease_test$P<-round(Painful_disease_test$P,3)
Painful_disease_test[1:6,]
Painful_disease_test

Table <- c("Cognitive function", "Beta", "Se", "P value", NA, NA, NA)
Table <- rbind(Table, c("Prospective memory", "", "", "", NA, NA, NA))
Table
Table <- rbind(Table,c("Baseline",Painful_disease_test[1,],Painful_disease_test[1,]))
Table <- rbind(Table,c("Follow-up",Painful_disease_test[2,],Painful_disease_test[2,]))
Table <- rbind(Table, c("Fluid intelligence", "", "", "", NA, NA, NA))
Table <- rbind(Table,c("Baseline",Painful_disease_test[3,],Painful_disease_test[3,]))
Table <- rbind(Table,c("Follow-up",Painful_disease_test[4,],Painful_disease_test[4,]))
Table <- rbind(Table, c("Reaction time", "", "", "", NA, NA, NA))
Table <- rbind(Table,c("Baseline",Painful_disease_test[5,],Painful_disease_test[5,]))
Table <- rbind(Table,c("Follow-up",Painful_disease_test[6,],Painful_disease_test[6,]))
colnames(Table)<-c('V1','V2','V3','V4','V5','V6','V7')
Table<-as.data.frame(Table)
Table
Table$V5 <- as.numeric(Table$V5)
Table$V6 <- as.numeric(Table$V6)
Table$V7 <- as.numeric(Table$V7)
Table$V2<-as.character(Table$V2)
Table$V3<-as.character(Table$V3)
Table$V4<-as.character(Table$V4)

Table$V4[Table$V4=="0"]<-"<0.001"
Table$V4[Table$V4=="4e-04"]<-"<0.001"


forest <- Table
forest 
paste(c(rep("T",2),rep("F",3),"T",rep("F",3),"T",rep("F",3),
        "T",rep("F",3)), sep=",",collapse = ",")
a1<-paste(forest[,1])
a2<-paste(forest[,2])
a3<-paste(forest[,3])
a4<-paste(forest[,4])
labeltext=cbind(a1,a2,a3,a4)
labeltext
forest$V8<-forest$V5-forest$V6
forest$V9<-forest$V5+forest$V6
pdf("Results final/Fig. 3a .pdf",  height=3,width=11, onefile = FALSE)
forestplot(labeltext=labeltext,
           graphwidth=unit(45,'mm'),
           mean = forest$V5,
           col=fpColors(line = "#CC79A7",
                        box="#D55E00"),
           lower=forest$V8,upper=forest$V9,is.summary=c(T,T,F,F,T,F,F,T,F,F),
           zero=0,boxsize=0.25,lineheight=unit(6,'mm'),colgap=unit(8,'mm'),xaxt = "n",  # 阻止绘制 x 轴
           xlab = "",cex.axis = 4,lwd.zero=2,lwd.ci=2,xticks=c(-1,0,1,2,3,4,5,6,7),clip = c(-1,7),lwd.xaxis=3,lty.ci = "solid",graph.pos = 4)
#PDF:heigt=9,width=11
dev.off()


test<-as.data.frame(t(cbind(Painful_disease_test$Beta,Painful_gums_test$Beta,Toothache_test$Beta)))
test.p<-as.data.frame(t(cbind(Painful_disease_test$P,Painful_gums_test$P,Toothache_test$P)))
rownames(test)<-c("Painful_disease","Painful_gums","Toothache")
colnames(test)<-rownames(Painful_disease_test)
data1 <- melt(test)
library(pheatmap)
rownames(test) <- gsub("_", " ", rownames(test))
colnames(test) <- gsub("_", " ", colnames(test))
colnames(test)<- gsub("1", "(Baseline)", colnames(test))
colnames(test) <- gsub("2", "(Follow-up)", colnames(test))
test

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 3 Pheatmap ####

load(file="Results final/Painful_disease_test_glm.Rdata")
load(file="Results final/Painful_gums_test_glm.Rdata")
load(file="Results final/Toothache_test_glm.Rdata")
C<-cbind(Painful_disease_test,Painful_gums_test,Toothache_test)
write.csv(C,file = "Results final/C_GLM.csv")
# Prepare data and transpose
test <- as.data.frame(cbind(Painful_disease_test$Beta, Painful_gums_test$Beta, Toothache_test$Beta))
test.p <- as.data.frame(cbind(Painful_disease_test$P, Painful_gums_test$P, Toothache_test$P))
rownames(test) <- rownames(Painful_disease_test)
colnames(test) <- c("Oral pain", "Painful gums", "Toothache")

# Modify row and column names
rownames(test) <- gsub("_", " ", rownames(test))
rownames(test) <- gsub("1", "(Baseline)", rownames(test))
rownames(test) <- gsub("2", "(Follow-up)", rownames(test))
colnames(test) <- gsub("_", " ", colnames(test))


test1<-test[1:6,]

# Matrix for significance stars
matrix <- ifelse(test.p < 0.05, 
                 ifelse(test.p < 0.01, 
                        ifelse(test.p < 0.001, "***", "**"), "*"), "")
matrix1<-matrix[1:6,]
legend_labels <- list(title = "Beta value")

# Create heatmap with annotation
min_value <- min(test1, na.rm = TRUE)
max_value <- max(test1, na.rm = TRUE)
mid_value <- 0
pdf("Results final/Fig. 3a.pdf", height = 6, width = 20, onefile = FALSE)
color_breaks <- c(seq(min_value, mid_value, length.out = 50), 
                  seq(mid_value + 1e-5, max_value, length.out = 50))
color_palette <- colorRampPalette(c("blue", "white", "red"))(99)
# Create annotation for rows
annotation_row <- data.frame(Group = c(rep("High performance", 4), rep("Efficient completion", 2)))
rownames(annotation_row) <- rownames(test1)
# Create heatmap with gaps between groups
pheatmap(test1, scale = "none", cluster_rows = F, cluster_cols = F, border = F,
         display_numbers = matrix1, fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_row = annotation_row, 
         annotation_colors = list(Group = c("High performance" = "#F0A19A", "Efficient completion" = "#5086c4")),
         gaps_row = 4,  # Add gap after the 4th row
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)

dev.off()
graphics.off()

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 3 Pheatmap ####
test1<-test[7:12,]

# Matrix for significance stars
matrix <- ifelse(test.p < 0.05, 
                 ifelse(test.p < 0.01, 
                        ifelse(test.p < 0.001, "***", "**"), "*"), "")
matrix1<-matrix[7:12,]
legend_labels <- list(title = "Beta value")

# Create heatmap with annotation
min_value <- min(test1, na.rm = TRUE)
max_value <- max(test1, na.rm = TRUE)
mid_value <- 0
pdf("Results final/Fig. 3b.pdf", height = 6, width = 20, onefile = FALSE)
color_breaks <- c(seq(min_value, mid_value, length.out = 50), 
                  seq(mid_value + 1e-5, max_value, length.out = 50))
color_palette <- colorRampPalette(c("blue", "white", "red"))(99)
# Create annotation for rows
annotation_row <- data.frame(Group = c(rep("Efficient completion", 2), rep("High performance", 4)))
rownames(annotation_row) <- rownames(test1)
# Create heatmap with gaps between groups
pheatmap(test1, scale = "none", cluster_rows = F, cluster_cols = F, border = F,
         display_numbers = matrix1, fontsize_col = 18, 
         fontsize_row = 18, 
         cellwidth = 50, 
         cellheight = 50, 
         fontsize_number = 30, 
         angle_col = 45, 
         legend_position = "bottom",
         color = color_palette, 
         breaks = color_breaks,
         annotation_row = annotation_row, 
         annotation_colors = list(Group = c("High performance" = "#F0A19A", "Efficient completion" = "#5086c4")),
         gaps_row = 2,  # Add gap after the 4th row
         annotation_legend = TRUE, 
         annotation_legend_side = "right", 
         annotation_legend_fontsize = 18, legend_labels = legend_labels)

dev.off()
graphics.off()






