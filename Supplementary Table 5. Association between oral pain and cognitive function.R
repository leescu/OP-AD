setwd("M:/OP&AZ/Cohort data")

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* section 1 ####
load(file="original data/Interpolation_data.Rdata")
load(file="Results final/Painful_disease_test_glm.Rdata")
load(file="Results final/Painful_gums_test_glm.Rdata")
load(file="Results final/Toothache_test_glm.Rdata")
C<-cbind(Painful_disease_test,Painful_gums_test,Toothache_test)
write.csv(C,file = "Results final/C_GLM.csv")