library(tidyverse)
library(ggplot2)
library(reshape2)
library(xtable)
library(here)

rm(list = ls())

# Load Functions ---------------------------------------------------------------
# set path for saving
PATH = here("Sample_size_sim/")
PATH_RESULTS = paste(PATH, "Results_method_2/", sep = "")

setwd(PATH)

rgmte_est = as.data.frame(read.table(paste(PATH_RESULTS, "rgmte_est.csv", sep = "")))
beta_0_est = as.data.frame(read.table(paste(PATH_RESULTS, "beta_0_est.csv", sep = "")))

sd_rgmte = as.data.frame(read.table(paste(PATH_RESULTS, "sd_rgmte.csv", sep = "")))
sd_beta_0 = as.data.frame(read.table(paste(PATH_RESULTS, "sd_beta_0.csv", sep = "")))

fstat_stage_1_MR = as.data.frame(read.table(paste(PATH_RESULTS, "fstat_stage_1_MR.csv", sep = "")))

beta_1_est = rgmte_est + beta_0_est
sd_beta_1 = sqrt(sd_rgmte**2 + sd_beta_0**2) # beta_1 sd if we assume that beta_1-beta_0 is independent of beta_0

pval_rgmte = as.data.frame(read.table(paste(PATH_RESULTS, "pval_rgmte.csv", sep = "")))
pval_beta_0 = as.data.frame(read.table(paste(PATH_RESULTS, "pval_beta_0.csv", sep = "")))


# Data properties: 
mean_S_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"mean_S_per_n.csv", sep = "")))
mean_G_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"mean_G_per_n.csv", sep = "")))
mean_G2_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"mean_G2_per_n.csv", sep = "")))
mean_Y_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"mean_Y_per_n.csv", sep = "")))

sd_S_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_S_per_n.csv", sep = "")))
sd_G_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_G_per_n.csv", sep = "")))
sd_G2_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_G2_per_n.csv", sep = "")))
sd_Y_per_n = as.data.frame(read.table(paste(PATH_RESULTS,"sd_Y_per_n.csv", sep = "")))

parameter_matrix_scenario_1 = read.table(paste(PATH_RESULTS,"parameter_matrix_scenario_2.csv", sep = ""))
true_values_beta0 = parameter_matrix_scenario_1$beta.0 
true_values_beta1 = parameter_matrix_scenario_1$beta.1
true_values_beta1_beta0 = true_values_beta1 - true_values_beta0

N = dim(beta_0_est)[1]

# Mean across N simulations
rgmte_est_mean = colMeans(rgmte_est, na.rm = TRUE)
# Standard Derivation from the beta values across N simulations
rgmte_est_sd = apply(rgmte_est, 2, sd,  na.rm = TRUE)

# Mean across N simulations
beta_0_est_mean = colMeans(beta_0_est, na.rm = TRUE)
# Standard Derivation from the beta values across N simulations
beta_0_est_sd = apply(beta_0_est, 2, sd,  na.rm = TRUE)

# Mean across N simulations
beta_1_est_mean = colMeans(beta_1_est, na.rm = TRUE)
# Standard Derivation from the beta values across N simulations
beta_1_est_sd = apply(beta_1_est, 2, sd,  na.rm = TRUE)


n = c(seq(100,600, 100), 800, seq(1000,10000, 1000), 20000, 50000, 80000)

res_table = cbind(n, colMeans(fstat_stage_1_MR), rgmte_est_mean, rgmte_est_sd, beta_0_est_mean, beta_0_est_sd, beta_1_est_mean, beta_1_est_sd)
colnames(res_table)= c("n","FSTAT_G2", "BETA1_BETA0_EST", "SD_BETA1_BETA0_EST",  "BETA_0_EST", "SD_BETA_0_EST", "BETA_1_EST", "SD_BETA_1_EST")
res_table = as.data.frame(res_table)

res_table$LCI_BETA1_BETA0 = res_table$BETA1_BETA0_EST - qnorm(0.975)* res_table$SD_BETA1_BETA0_EST
res_table$HCI_BETA1_BETA0 = res_table$BETA1_BETA0_EST + qnorm(0.975)* res_table$SD_BETA1_BETA0_EST
res_table$LCI_BETA_0 = res_table$BETA_0_EST - qnorm(0.975)* res_table$SD_BETA_0_EST
res_table$HCI_BETA_0 = res_table$BETA_0_EST + qnorm(0.975)* res_table$SD_BETA_0_EST
res_table$LCI_BETA_1 = res_table$BETA_1_EST - qnorm(0.975)* res_table$SD_BETA_1_EST
res_table$HCI_BETA_1 = res_table$BETA_1_EST + qnorm(0.975)* res_table$SD_BETA_1_EST

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

res_table$power_rgmte = apply(pval_rgmte, 2, power_cal)
power_rgmte_se = sqrt(res_table$power_rgmte*(100-res_table$power_rgmte)/N)
res_table$power_rgmte_LCI = res_table$power_rgmte -qnorm(0.975)*power_rgmte_se
res_table$power_rgmte_HCI = res_table$power_rgmte +qnorm(0.975)*power_rgmte_se

res_table$power_beta_0 = apply(pval_beta_0, 2, power_cal)
power_beta_0_se = sqrt((res_table$power_beta_0*(100-res_table$power_beta_0))/N)
res_table$power_beta_0_LCI = res_table$power_beta_0 -qnorm(0.975)*power_beta_0_se
res_table$power_beta_0_HCI = res_table$power_beta_0 +qnorm(0.975)*power_beta_0_se

# calculate the p-value for beta_1
test_stat = abs(beta_1_est/sd_beta_1)
pval_beta_1 = map_df(test_stat, function(x) pmin(1, 2*pmin(pnorm(x), 1-pnorm(x)))) 

res_table$power_beta_1 = apply(pval_beta_1, 2, power_cal)
power_beta_1_se = sqrt(res_table$power_beta_1*(100-res_table$power_beta_1)/N)
res_table$power_beta_1_LCI = res_table$power_beta_1 -qnorm(0.975)*power_beta_1_se
res_table$power_beta_1_HCI = res_table$power_beta_1 +qnorm(0.975)*power_beta_1_se
# Calcualte the coverage probablity 
# beta 0 ---------------------------------------------------------------------------------------------------------
beta_0_lower_CI = beta_0_est - qnorm(0.975)*sd_beta_0
beta_0_higher_CI = beta_0_est + qnorm(0.975)*sd_beta_0

true_beta_0_within_CI = (true_values_beta0 > beta_0_lower_CI & true_values_beta0 < beta_0_higher_CI) *1
res_table$coverage_beta0 = colMeans(true_beta_0_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta0 = sqrt((res_table$coverage_beta0 * (1-res_table$coverage_beta0))/N)

res_table$coverage_beta0_lower = res_table$coverage_beta0 - qnorm(0.975)*se_mc_coverage_beta0
res_table$coverage_beta0_higher = res_table$coverage_beta0 + qnorm(0.975)*se_mc_coverage_beta0


# beta 1 - beta 0 ---------------------------------------------------------------------------------------------------------
beta_diff_lower_CI = rgmte_est - qnorm(0.975)*sd_rgmte
beta_diff_higher_CI = rgmte_est + qnorm(0.975)*sd_rgmte

true_beta_diff_within_CI = (true_values_beta1_beta0 > beta_diff_lower_CI & true_values_beta1_beta0 < beta_diff_higher_CI) *1
res_table$coverage_beta_diff = colMeans(true_beta_diff_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta_diff = sqrt((res_table$coverage_beta_diff * (1-res_table$coverage_beta_diff))/N)

res_table$coverage_beta_diff_lower = res_table$coverage_beta_diff - qnorm(0.975)*se_mc_coverage_beta_diff
res_table$coverage_beta_diff_higher = res_table$coverage_beta_diff + qnorm(0.975)*se_mc_coverage_beta_diff


# beta_1 if we assume that beta_1-beta_0 is independent of beta_0 ------------------------------------------------------------
#sd_beta_1 = sqrt(sd_rgmte**2 + sd_beta_0**2)
beta_1_lower_CI = beta_1_est - qnorm(0.975)*sd_beta_1
beta_1_higher_CI = beta_1_est + qnorm(0.975)*sd_beta_1

true_beta_1_within_CI = (true_values_beta1 > beta_1_lower_CI & true_values_beta1 < beta_1_higher_CI) *1
res_table$coverage_beta1 = colMeans(true_beta_1_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta1 = sqrt((res_table$coverage_beta1 * (1-res_table$coverage_beta1))/N)

res_table$coverage_beta1_lower = res_table$coverage_beta1 - qnorm(0.975)*se_mc_coverage_beta1
res_table$coverage_beta1_higher = res_table$coverage_beta1 + qnorm(0.975)*se_mc_coverage_beta1

#################################################################################################################
res_table
write.table(res_table, file = paste(PATH_RESULTS, "res_table_method_2.csv", sep = ""))
