# save mean results for N simulations -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------
beta_1_est = rgmte_est + beta_0_est
sd_beta_1 = sqrt(sd_rgmte**2 + sd_beta_0**2)

res_table = cbind(n, scen$beta_1[s], scen$beta_0[s],scen$beta_1[s]-scen$beta_0[s],
mean(fstat_stage_1_MR),
mean(beta_0_est, na.rm = TRUE),  sd(beta_0_est, na.rm = TRUE), 
mean(beta_1_est, na.rm = TRUE), sd(beta_1_est, na.rm = TRUE), 
mean(rgmte_est, na.rm = TRUE), sd(rgmte_est,na.rm = TRUE))

colnames(res_table)= c("n","True_beta_1", "True_beta_0", "True_beta_diff",
                      "FSTAT_G2", 
                      "BETA_0_EST", "SD_BETA_0_EST",  
                      "BETA_1_EST", "SD_BETA_1_EST",
                      "BETA1_BETA0_EST", "SD_BETA1_BETA0_EST")
res_table = as.data.frame(res_table)

# CI interval
res_table$LCI_BETA_1 = res_table$BETA_1_EST - qnorm(0.975)* res_table$SD_BETA_1_EST
res_table$HCI_BETA_1 = res_table$BETA_1_EST + qnorm(0.975)* res_table$SD_BETA_1_EST
res_table$LCI_BETA_0 = res_table$BETA_0_EST - qnorm(0.975)* res_table$SD_BETA_0_EST
res_table$HCI_BETA_0 = res_table$BETA_0_EST + qnorm(0.975)* res_table$SD_BETA_0_EST
res_table$LCI_BETA1_BETA0 = res_table$BETA1_BETA0_EST - qnorm(0.975)* res_table$SD_BETA1_BETA0_EST
res_table$HCI_BETA1_BETA0 = res_table$BETA1_BETA0_EST + qnorm(0.975)* res_table$SD_BETA1_BETA0_EST

# Power calculation 
power_cal = function(x){
    x_bin = ifelse(x < 0.05, 1, 0)
    power = mean(x_bin, na.rm = TRUE)*100
    return(power)
}

res_table$power_beta_0 = power_cal(pval_beta_0)
power_beta_0_se = sqrt((res_table$power_beta_0*(100-res_table$power_beta_0))/N)
res_table$power_beta_0_LCI = res_table$power_beta_0 -qnorm(0.975)*power_beta_0_se
res_table$power_beta_0_HCI = res_table$power_beta_0 +qnorm(0.975)*power_beta_0_se

# calculate the p-value for beta_1
test_stat = abs(beta_1_est/sd_beta_1) %>% as_tibble()
pval_beta_1 = map_df(test_stat, function(x) pmin(1, 2*pmin(pnorm(x), 1-pnorm(x)))) 

res_table$power_beta_1 = power_cal(pval_beta_1)
power_beta_1_se = sqrt(res_table$power_beta_1*(100-res_table$power_beta_1)/N)
res_table$power_beta_1_LCI = res_table$power_beta_1 -qnorm(0.975)*power_beta_1_se
res_table$power_beta_1_HCI = res_table$power_beta_1 +qnorm(0.975)*power_beta_1_se

# calculate p-vale for beta_1-beta_0
res_table$power_rgmte = power_cal(pval_rgmte)
power_rgmte_se = sqrt(res_table$power_rgmte*(100-res_table$power_rgmte)/N)
res_table$power_rgmte_LCI = res_table$power_rgmte -qnorm(0.975)*power_rgmte_se
res_table$power_rgmte_HCI = res_table$power_rgmte +qnorm(0.975)*power_rgmte_se

# Calculate the coverage probablity 
# beta 0 ---------------------------------------------------------------------------------------------------------
beta_0_lower_CI = beta_0_est - qnorm(0.975)*sd_beta_0
beta_0_higher_CI = beta_0_est + qnorm(0.975)*sd_beta_0

true_beta_0_within_CI = (res_table$True_beta_0 > beta_0_lower_CI & res_table$True_beta_0 < beta_0_higher_CI) *1
res_table$coverage_beta0 = mean(true_beta_0_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta0 = sqrt((res_table$coverage_beta0 * (1-res_table$coverage_beta0))/N)

res_table$coverage_beta0_lower = res_table$coverage_beta0 - qnorm(0.975)*se_mc_coverage_beta0
res_table$coverage_beta0_higher = res_table$coverage_beta0 + qnorm(0.975)*se_mc_coverage_beta0

# beta_1 if we assume that beta_1-beta_0 is independent of beta_0 ------------------------------------------------------------
#sd_beta_1 = sqrt(sd_rgmte**2 + sd_beta_0**2)
beta_1_lower_CI = beta_1_est - qnorm(0.975)*sd_beta_1
beta_1_higher_CI = beta_1_est + qnorm(0.975)*sd_beta_1

true_beta_1_within_CI = (res_table$True_beta_1 > beta_1_lower_CI & res_table$True_beta_1 < beta_1_higher_CI) *1
res_table$coverage_beta1 = mean(true_beta_1_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta1 = sqrt((res_table$coverage_beta1 * (1-res_table$coverage_beta1))/N)

res_table$coverage_beta1_lower = res_table$coverage_beta1 - qnorm(0.975)*se_mc_coverage_beta1
res_table$coverage_beta1_higher = res_table$coverage_beta1 + qnorm(0.975)*se_mc_coverage_beta1

# beta 1 - beta 0 ---------------------------------------------------------------------------------------------------------
beta_diff_lower_CI = rgmte_est - qnorm(0.975)*sd_rgmte
beta_diff_higher_CI = rgmte_est + qnorm(0.975)*sd_rgmte

true_beta_diff_within_CI = (res_table$True_beta_diff > beta_diff_lower_CI & res_table$True_beta_diff < beta_diff_higher_CI) *1
res_table$coverage_beta_diff = mean(true_beta_diff_within_CI, na.rm = TRUE)

# MC SE of coverage estimate
se_mc_coverage_beta_diff = sqrt((res_table$coverage_beta_diff * (1-res_table$coverage_beta_diff))/N)

res_table$coverage_beta_diff_lower = res_table$coverage_beta_diff - qnorm(0.975)*se_mc_coverage_beta_diff
res_table$coverage_beta_diff_higher = res_table$coverage_beta_diff + qnorm(0.975)*se_mc_coverage_beta_diff
