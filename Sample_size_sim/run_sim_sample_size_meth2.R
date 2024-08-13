library(tidyverse)
library(here)
rm(list = ls())

# Load Functions ---------------------------------------------------------------
# set path for saving
source(here("sample_size_sim", "data_gen_alspac_fct.R"))
PATH = here("sample_size_sim")
PATH_RESULTS = paste(PATH, "Results_method_2/", sep = "")
setwd(PATH)


# Simulation parameters --------------------------------------------------------
set.seed(3456)
n = c(seq(100,600, 100), 800, seq(1000,10000, 1000), 20000, 50000, 80000)
N = 20000 # number of simulation runs

#No pleitropy
number_setting = 1
parameter_matrix = matrix(NA, nrow = number_setting, ncol = 13)

parameter_matrix[,1] = 0 #par.U0
par.UG = parameter_matrix[,2] = 0  # par.UG
parameter_matrix[,3] = -2 # par.S0
par.SDG = parameter_matrix[,4] = 0 # par.SG
parameter_matrix[,5] = 0.8  # par.SG2 
parameter_matrix[,6] = 1.6 # par.SU
parameter_matrix[,7] = 3500 # par.Y0
parameter_matrix[,8] = 80 #c(0,0.8,0.8) # par.YG
parameter_matrix[,9] = 80 # par.YU
parameter_matrix[,10] = 0 # par.YG2
parameter_matrix[,11] = 0 # par.UG2
parameter_matrix[,12] = -168 # beta.1
parameter_matrix[,13] = -159 #beta.0

# parameter matrix as dataframe 
parameter_matrix_dtf = as.data.frame(parameter_matrix)
colnames(parameter_matrix_dtf) = c("gamma.U0", "gamma.UG", "gamma.S0", "gamma.SG",
                                    "gamma.SG2", "gamma.SU", "gamma.Y0", "gamma.YG", "gamma.YU", "gamma.YG2", "gamma.UG2",
                                    "beta.1", "beta.0")

# Lists to save results
rgmte_est_lm_sum = vector("list", length = N)
rgmte_est_lm_coef = vector("list", length = N)
glm_sstar_g_sum = vector("list", length = N)
glm_sstar_g_coef = vector("list", length = N)

glm_s_g2_sum = vector("list", length = N)
glm_s_g2_coef = vector("list", length = N)

lm_ynew_shat_sum =  vector("list", length = N)
lm_ynew_shat_coef =  vector("list", length = N)

rgmte_est = matrix(NA, nrow =N, ncol = length(n))
beta_0_est = matrix(NA, nrow =N, ncol = length(n))
sd_rgmte = matrix(NA, nrow =N, ncol = length(n))
sd_beta_0 = matrix(NA, nrow =N, ncol = length(n))

fstat_stage_1_MR = matrix(NA, nrow =N, ncol = length(n))

pval_rgmte = matrix(NA, nrow =N, ncol = length(n))
pval_beta_0 = matrix(NA, nrow =N, ncol = length(n))

mean_S = rep(NA, times=N)
sd_S = rep(NA, times=N)
mean_G = rep(NA, times=N)
sd_G = rep(NA, times=N)
mean_G2 = rep(NA, times=N)
sd_G2 = rep(NA, times=N)
mean_Y = rep(NA, times=N)
sd_Y = rep(NA, times=N)

mean_G_S1 = rep(NA, times=N)

mean_S_per_n = matrix(NA, nrow =N, ncol = length(n))
sd_S_per_n = matrix(NA, nrow =N, ncol = length(n))
mean_G_per_n = matrix(NA, nrow =N, ncol = length(n))
sd_G_per_n = matrix(NA, nrow =N, ncol = length(n))
mean_G2_per_n = matrix(NA, nrow =N, ncol = length(n))
sd_G2_per_n = matrix(NA, nrow =N, ncol = length(n))
mean_Y_per_n = matrix(NA, nrow =N, ncol = length(n))
sd_Y_per_n = matrix(NA, nrow =N, ncol = length(n))

mean_G_S1_per_n = matrix(NA, nrow =N, ncol = length(n))


# Simulation -------------------------------------------------------------------
for(k in 1:length(n)){
  for(i in 1:N){

    data = data_gen_alspac_twoG(n[k], parameter_matrix_dtf, 1)[[1]]                                    
    parameters = data_gen_alspac_twoG(n[k], parameter_matrix_dtf, 1)[[2]]    

    # Data properties
    mean_S[i] = mean(data$S)
    sd_S[i] = sd(data$S)
    mean_G[i] = mean(data$G)
    sd_G[i] = sd(data$G)
    mean_G2[i] = mean(data$G2)
    sd_G2[i] = sd(data$G2)
    mean_Y[i] = mean(data$Y)
    sd_Y[i] = sd(data$Y)

    mean_G_S1[i] = mean(data$G[data$S ==1])

# Estimation of GMTE using RGMTE method as in TWIST paper
    data$Sstar = data$G * data$S
    glm_sstar_g = glm(Sstar ~ G,  family ="binomial", data = data)
    data$Sstar_hat = glm_sstar_g$fitted.values
    glm_sstar_g_sum[[i]] = summary(glm_sstar_g)
    glm_sstar_g_coef[[i]] = summary(glm_sstar_g)$coefficients
 

    rgmte_est_lm = lm(Y~ S + Sstar + Sstar_hat, data = data)
    rgmte_est_lm_sum[[i]] = summary(rgmte_est_lm)
    rgmte_est_lm_coef[[i]] = summary(rgmte_est_lm)$coefficients
    GMTE = rgmte_est_lm$coef[3]

    data$Y_new = data$Y-GMTE*(data$S*data$G)
    
    # Estimation of S_hat
    glm_s_g2 = glm(S~G2, family ="binomial", data = data)
    glm_s_g2_sum[[i]] = summary(glm_s_g2)
    glm_s_g2_coef[[i]] = summary(glm_s_g2)$coefficients
    data$S_hat = glm_s_g2$fitted.values

    # Estimation of beta_0
    lm_ynew_shat = lm(Y_new ~ S_hat, data = data)
    lm_ynew_shat_sum[[i]] = summary(lm_ynew_shat)
    lm_ynew_shat_coef[[i]] = summary(lm_ynew_shat)$coefficients

  }

    mean_S_per_n[,k] = mean_S
    sd_S_per_n[,k] = sd_S

    mean_G_per_n[,k] = mean_G
    sd_G_per_n[,k] = sd_G

    mean_G2_per_n[,k] = mean_G2
    sd_G2_per_n[,k] = sd_G2

    mean_Y_per_n[,k] = mean_Y
    sd_Y_per_n[,k] = sd_Y
 
    mean_G_S1_per_n[,k] = mean_G_S1

    rgmte_est[,k] = sapply(rgmte_est_lm_sum, function(x) x$coefficients[3,1]) # [4,1] for jacks calculation of RGMTE
    beta_0_est[,k] = sapply(lm_ynew_shat_sum, function(x) tryCatch({help_coef =x$coefficients[2,1]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))

    sd_rgmte[,k] = sapply(rgmte_est_lm_sum, function(x) x$coefficients[3,2]) # [4,1] for jacks calculation of RGMTE
    sd_beta_0[,k] = sapply(lm_ynew_shat_sum, function(x) tryCatch({help_coef =x$coefficients[2,2]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))                                  
    fstat_stage_1_MR[,k] = sapply(glm_s_g2_sum, function(x) x$coefficients[2,1]**2/x$coefficients[2,2]**2)
    
    # Store p-value for power calculation
    pval_rgmte[,k] = sapply(rgmte_est_lm_sum, function(x) x$coefficients[3,4])
    pval_beta_0[,k] = sapply(lm_ynew_shat_sum, function(x) tryCatch({help_coef =x$coefficients[2,4]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))
   
   print(k)

}

write.table(mean_S_per_n, file = paste(PATH_RESULTS, "mean_S_per_n.csv", sep = ""))
write.table(mean_G_per_n, file = paste(PATH_RESULTS, "mean_G_per_n.csv", sep = ""))
write.table(mean_G2_per_n, file = paste(PATH_RESULTS, "mean_G2_per_n.csv", sep = ""))
write.table(mean_Y_per_n, file = paste(PATH_RESULTS, "mean_Y_per_n.csv", sep = ""))
write.table(mean_G_S1_per_n, file = paste(PATH_RESULTS, "mean_G_S1_per_n.csv", sep = ""))

write.table(sd_S_per_n, file = paste(PATH_RESULTS, "sd_S_per_n.csv", sep = ""))
write.table(sd_G_per_n, file = paste(PATH_RESULTS, "sd_G_per_n.csv", sep = ""))
write.table(sd_G2_per_n, file = paste(PATH_RESULTS, "sd_G2_per_n.csv", sep = ""))
write.table(sd_Y_per_n, file = paste(PATH_RESULTS, "sd_Y_per_n.csv", sep = ""))

write.table(parameter_matrix_dtf, file = paste(PATH_RESULTS, "parameter_matrix_scenario_2.csv", sep = ""))

write.table(rgmte_est, file = paste(PATH_RESULTS, "rgmte_est.csv", sep = ""))
write.table(beta_0_est, file = paste(PATH_RESULTS, "beta_0_est.csv", sep = ""))
write.table(sd_rgmte, file = paste(PATH_RESULTS, "sd_rgmte.csv", sep = ""))
write.table(sd_beta_0, file = paste(PATH_RESULTS, "sd_beta_0.csv", sep = ""))

write.table(fstat_stage_1_MR, file = paste(PATH_RESULTS, "fstat_stage_1_MR.csv", sep = ""))

write.table(pval_rgmte, file = paste(PATH_RESULTS, "pval_rgmte.csv", sep = ""))
write.table(pval_beta_0, file = paste(PATH_RESULTS, "pval_beta_0.csv", sep = ""))