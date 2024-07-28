# ++++++++++++++++++++++++++++++++++++++++++++++++++++++ ####
#* Section: Cumulative Incidence and Absolute Risk Difference ####
setwd("M:/OP&AZ/Cohort data")
# Load necessary packages
library(survival)
library(boot)

# Load data
load(file="original data/Interpolation_data.Rdata")


data<-Interpolation_data



data$years <- data$days / 365


time_points <- c(5, 10) 


calculate_cumulative_incidence <- function(data, time_point) {
  survival_fit <- survfit(Surv(years, Alzheimer_dementia) ~ Painful_disease, data = data)
  summary_survival <- summary(survival_fit, times = time_point)
  cumulative_incidence <- 1 - summary_survival$surv
  return(cumulative_incidence)
}


calculate_absolute_risk_difference <- function(data, time_point) {
  cumulative_incidence <- calculate_cumulative_incidence(data, time_point)
  risk_difference <- cumulative_incidence[2] - cumulative_incidence[1]
  return(risk_difference)
}


bootstrap_cumulative_incidence <- function(data, indices, time_point) {
  sample_data <- data[indices, ]
  return(calculate_cumulative_incidence(sample_data, time_point))
}

bootstrap_absolute_risk_difference <- function(data, indices, time_point) {
  sample_data <- data[indices, ]
  return(calculate_absolute_risk_difference(sample_data, time_point))
}


results <- list()


for (time_point in time_points) {

  cumulative_incidence <- calculate_cumulative_incidence(data, time_point)
  

  bootstrap_result_ci_0 <- boot(data = data, statistic = function(data, indices) {
    bootstrap_cumulative_incidence(data, indices, time_point)[1]
  }, R = 1000)  # 
  
  bootstrap_result_ci_1 <- boot(data = data, statistic = function(data, indices) {
    bootstrap_cumulative_incidence(data, indices, time_point)[2]
  }, R = 1000) 
  
  ci_lower_0 <- boot.ci(bootstrap_result_ci_0, type = "perc")$percent[4]
  ci_upper_0 <- boot.ci(bootstrap_result_ci_0, type = "perc")$percent[5]
  ci_lower_1 <- boot.ci(bootstrap_result_ci_1, type = "perc")$percent[4]
  ci_upper_1 <- boot.ci(bootstrap_result_ci_1, type = "perc")$percent[5]
  

  absolute_risk_difference <- calculate_absolute_risk_difference(data, time_point)
  

  bootstrap_result_ard <- boot(data = data, statistic = function(data, indices) {
    bootstrap_absolute_risk_difference(data, indices, time_point)
  }, R = 1000)  
  
  ard_ci <- boot.ci(bootstrap_result_ard, type = "perc")
  ard_ci_lower <- ard_ci$percent[4]
  ard_ci_upper <- ard_ci$percent[5]
  

  results[[as.character(time_point)]] <- list(
    cumulative_incidence_0 = cumulative_incidence[1],
    ci_lower_0 = ci_lower_0,
    ci_upper_0 = ci_upper_0,
    cumulative_incidence_1 = cumulative_incidence[2],
    ci_lower_1 = ci_lower_1,
    ci_upper_1 = ci_upper_1,
    absolute_risk_difference = absolute_risk_difference,
    ard_ci_lower = ard_ci_lower,
    ard_ci_upper = ard_ci_upper
  )
}


for (time_point in time_points) {
  cat("Time：", time_point, "year\n")
  cat("Painful_disease = 0, Cumulative incidence：", results[[as.character(time_point)]]$cumulative_incidence_0 * 1000, 
      " (", results[[as.character(time_point)]]$ci_lower_0 * 1000, "-", results[[as.character(time_point)]]$ci_upper_0 * 1000, ")\n")
  cat("Painful_disease = 1, Cumulative incidence：", results[[as.character(time_point)]]$cumulative_incidence_1 * 1000, 
      " (", results[[as.character(time_point)]]$ci_lower_1 * 1000, "-", results[[as.character(time_point)]]$ci_upper_1 * 1000, ")\n")
  cat("Absolute risk difference：", results[[as.character(time_point)]]$absolute_risk_difference * 1000, 
      " (", results[[as.character(time_point)]]$ard_ci_lower * 1000, "-", results[[as.character(time_point)]]$ard_ci_upper * 1000, ")\n\n")
}