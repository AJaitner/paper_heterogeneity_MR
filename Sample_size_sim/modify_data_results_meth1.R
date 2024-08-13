library(tidyverse)
library(ggplot2)
library(reshape2)
library(xtable)
library(here)

rm(list = ls())

# Load Functions ---------------------------------------------------------------
# set path for saving
PATH = here("Sample_size_sim/")
PATH_RESULTS = paste(PATH, "Results_method_1/", sep = "")

setwd(PATH)


beta_0_matrix_scen1 = as.data.frame(read.table(paste(PATH_RESULTS,"lm_Y_sen1_sum_beta_0.csv", sep = "")))
beta_1_matrix_scen1 = as.data.frame(read.table(paste(PATH_RESULTS,"lm_Y_sen1_sum_beta_1.csv", sep = "")))

sd_beta0_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_lm_beta_0.csv", sep = "")))
sd_beta1_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_lm_beta_1.csv", sep = "")))

beta1_beta0_matrix_scen1 = beta_1_matrix_scen1 - beta_0_matrix_scen1
sd_beta1_beta0_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_beta1_beta0_per_n.csv", sep = "")))

fstat_stage_1_MR_G2 = as.data.frame(read.table(paste(PATH_RESULTS, "fstat_stage_1_MR_G2.csv", sep = "")))
fstat_stage_1_MR_G = as.data.frame(read.table(paste(PATH_RESULTS, "fstat_stage_1_MR_G.csv", sep = "")))

pval_beta_0 = as.data.frame(read.table(paste(PATH_RESULTS, "pval_beta_0.csv", sep = "")))
pval_beta_1 = as.data.frame(read.table(paste(PATH_RESULTS, "pval_beta_1.csv", sep = "")))

# Data properties: 
mean_S_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"mean_S_per_n.csv", sep = "")))
mean_G_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"mean_G_per_n.csv", sep = "")))
mean_G2_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"mean_G2_per_n.csv", sep = "")))
mean_Y_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"mean_Y_per_n.csv", sep = "")))

sd_S_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_S_per_n.csv", sep = "")))
sd_G_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_G_per_n.csv", sep = "")))
sd_G2_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_G2_per_n.csv", sep = "")))
sd_Y_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_Y_per_n.csv", sep = "")))


# ----------------------------------------------------------------------------------------------------
parameter_matrix_scenario_1 = read.table(paste(PATH_RESULTS,"parameter_matrix_scenario_1.csv", sep = ""))
true_values_beta0 = parameter_matrix_scenario_1$beta.0 
true_values_beta1 = parameter_matrix_scenario_1$beta.1
true_value_beta_diff = true_values_beta1 - true_values_beta0

N = dim(beta_0_matrix_scen1)[1]
# Mean across N simulations
beta_0_est_mean = colMeans(beta_0_matrix_scen1, na.rm = TRUE)
# Standard Derivation from the beta values across N simulations
beta_0_est_sd = apply(beta_0_matrix_scen1, 2, sd, na.rm = TRUE)

# Mean across N simulations
beta_1_est_mean = colMeans(beta_1_matrix_scen1, na.rm = TRUE)
# Standard Derivation from the beta values across N simulations
beta_1_est_sd = apply(beta_1_matrix_scen1, 2, sd, na.rm = TRUE)

# Mean across N simulations
beta1_beta0_est_mean = colMeans(beta1_beta0_matrix_scen1, na.rm = TRUE)
# Standard Derivation from the beta values across N simulations
beta1_beta0_est_sd = apply(beta1_beta0_matrix_scen1, 2, sd, na.rm = TRUE)

n = c(seq(100,600, 100), 800, seq(1000,10000, 1000), 20000, 50000, 80000)
#n = seq(100,600, 50)

res_table = cbind(n, colMeans(fstat_stage_1_MR_G2), colMeans(fstat_stage_1_MR_G), beta_0_est_mean, beta_0_est_sd, beta_1_est_mean, beta_1_est_sd, beta1_beta0_est_mean, beta1_beta0_est_sd)
colnames(res_table)= c("n","FSTAT_G2", "FSTAT_G", "BETA_0_EST", "SD_BETA_0_EST",  "BETA_1_EST", "SD_BETA_1_EST", "BETA1_BETA0_EST", "SD_BETA1_BETA0_EST")
res_table = as.data.frame(res_table)

res_table$LCI_BETA_1 = res_table$BETA_1_EST - qnorm(0.975)* res_table$SD_BETA_1_EST
res_table$HCI_BETA_1 = res_table$BETA_1_EST + qnorm(0.975)* res_table$SD_BETA_1_EST
res_table$LCI_BETA_0 = res_table$BETA_0_EST - qnorm(0.975)* res_table$SD_BETA_0_EST
res_table$HCI_BETA_0 = res_table$BETA_0_EST + qnorm(0.975)* res_table$SD_BETA_0_EST

res_table$LCI_BETA1_BETA0 = res_table$BETA1_BETA0_EST - qnorm(0.975)* res_table$SD_BETA1_BETA0_EST
res_table$HCI_BETA1_BETA0 = res_table$BETA1_BETA0_EST + qnorm(0.975)* res_table$SD_BETA1_BETA0_EST

# Data properties mean and standard derivation
res_table$mean_S = colMeans(mean_S_per_n)
res_table$sd_S = colMeans(sd_S_per_n)

res_table$mean_G = colMeans(mean_G_per_n)
res_table$sd_G = colMeans(sd_G_per_n)

res_table$mean_G2 = colMeans(mean_G2_per_n)
res_table$sd_G2 = colMeans(sd_G2_per_n)

res_table$mean_Y = colMeans(mean_Y_per_n)
res_table$sd_Y = colMeans(sd_Y_per_n)

# Power calculation 
power_cal = function(x){
    x_bin = ifelse(x < 0.05, 1, 0)
    power = mean(x_bin, na.rm = TRUE)*100
    return(power)
}

res_table$power_beta_0 = apply(pval_beta_0, 2, power_cal)
power_beta_0_se = sqrt((res_table$power_beta_0*(100-res_table$power_beta_0))/N)
res_table$power_beta_0_LCI = res_table$power_beta_0 -qnorm(0.975)*power_beta_0_se
res_table$power_beta_0_HCI = res_table$power_beta_0 +qnorm(0.975)*power_beta_0_se

res_table$power_beta_1 = apply(pval_beta_1, 2, power_cal)
power_beta_1_se = sqrt(res_table$power_beta_1*(100-res_table$power_beta_1)/N)
res_table$power_beta_1_LCI = res_table$power_beta_1 -qnorm(0.975)*power_beta_1_se
res_table$power_beta_1_HCI = res_table$power_beta_1 +qnorm(0.975)*power_beta_1_se

# calculate p-vale for beta_1-beta_0
test_stat = abs(beta1_beta0_matrix_scen1/sd_beta1_beta0_per_n)
pval_diff = map_df(test_stat, function(x) pmin(1, 2*pmin(pnorm(x), 1-pnorm(x)))) 
res_table$power_rgmte = apply(pval_diff, 2, power_cal)
power_rgmte_se = sqrt(res_table$power_rgmte*(100-res_table$power_rgmte)/N)
res_table$power_rgmte_LCI = res_table$power_rgmte -qnorm(0.975)*power_rgmte_se
res_table$power_rgmte_HCI = res_table$power_rgmte +qnorm(0.975)*power_rgmte_se

# Calculate the coverage probablity 
# beta 0 ---------------------------------------------------------------------------------------------------------
beta_0_lower_CI = beta_0_matrix_scen1 - qnorm(0.975)*sd_beta0_per_n
beta_0_higher_CI = beta_0_matrix_scen1 + qnorm(0.975)*sd_beta0_per_n

true_beta_0_within_CI = (true_values_beta0 > beta_0_lower_CI & true_values_beta0 < beta_0_higher_CI) *1
res_table$coverage_beta0 = colMeans(true_beta_0_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta0 = sqrt((res_table$coverage_beta0 * (1-res_table$coverage_beta0))/N)

res_table$coverage_beta0_lower = res_table$coverage_beta0 - qnorm(0.975)*se_mc_coverage_beta0
res_table$coverage_beta0_higher = res_table$coverage_beta0 + qnorm(0.975)*se_mc_coverage_beta0

# beta 1 ---------------------------------------------------------------------------------------------------------
beta_1_lower_CI = beta_1_matrix_scen1 - qnorm(0.975)*sd_beta1_per_n
beta_1_higher_CI = beta_1_matrix_scen1 + qnorm(0.975)*sd_beta1_per_n

true_beta_1_within_CI = (true_values_beta1 > beta_1_lower_CI & true_values_beta1 < beta_1_higher_CI) *1
res_table$coverage_beta1 = colMeans(true_beta_1_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta1 = sqrt((res_table$coverage_beta1 * (1-res_table$coverage_beta1))/N)

res_table$coverage_beta1_lower = res_table$coverage_beta1 - qnorm(0.975)*se_mc_coverage_beta1
res_table$coverage_beta1_higher = res_table$coverage_beta1 + qnorm(0.975)*se_mc_coverage_beta1

# beta 1 - beta 0 ---------------------------------------------------------------------------------------------------------
beta_diff_lower_CI = beta1_beta0_matrix_scen1 - qnorm(0.975)*sd_beta1_beta0_per_n
beta_diff_higher_CI = beta1_beta0_matrix_scen1 + qnorm(0.975)*sd_beta1_beta0_per_n

true_beta_diff_within_CI = (true_value_beta_diff > beta_diff_lower_CI & true_value_beta_diff < beta_diff_higher_CI) *1
res_table$coverage_beta_diff = colMeans(true_beta_diff_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta_diff = sqrt((res_table$coverage_beta_diff * (1-res_table$coverage_beta_diff))/N)

res_table$coverage_beta_diff_lower = res_table$coverage_beta_diff - qnorm(0.975)*se_mc_coverage_beta_diff
res_table$coverage_beta_diff_higher = res_table$coverage_beta_diff + qnorm(0.975)*se_mc_coverage_beta_diff

#################################################################################################################
res_table
write.table(res_table, file = paste(PATH_RESULTS, "res_table_method_1.csv", sep = ""))
