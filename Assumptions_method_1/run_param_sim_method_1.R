library(tidyverse)
library(here)
rm(list = ls())

# Load Functions ---------------------------------------------------------------
# set path for saving
source(here("sample_size_sim", "data_gen_alspac_fct.R"))
PATH = here("Assumptions_method_1")
PATH_RESULTS = paste(PATH, "Results/", sep = "")
setwd(PATH)

# Simulation parameters --------------------------------------------------------
set.seed(3456)
n = 20000 # number of patients
N = 20000 # number of simulation runs

#No pleitropy
number_setting = 9
parameter_matrix = matrix(NA, nrow = number_setting, ncol = 13)

parameter_matrix[,1] = rep(0,number_setting) #par.U0
par.UG = parameter_matrix[,2] = c(rep(0,6), 0.8, 0, 0.8) # par.UG
parameter_matrix[,3] = rep(-2,number_setting) # par.S0
par.SDG = parameter_matrix[,4] = c(0.2,0.8,2,0,0,0,0.8,0.8,0.8) # par.SG
parameter_matrix[,5] = c(0.4,0.8,2, 0.4,0.8,2,0.8,0.8,0.8)  # par.SG2 
parameter_matrix[,6] = rep(1.6,number_setting) # par.SU
parameter_matrix[,7] = rep(3500,number_setting) # par.Y0
parameter_matrix[,8] = c(rep(0,6),0,80,80) # par.YG
parameter_matrix[,9] = rep(80,number_setting) # par.YU
parameter_matrix[,10] = rep(0,number_setting) # par.YG2
parameter_matrix[,11] = rep(0,number_setting) # par.UG2
parameter_matrix[,12] = rep(-200,number_setting) # beta.1
parameter_matrix[,13] = rep(-100,number_setting)#beta.0


# parameter matrix as dataframe 
parameter_matrix_dtf = as.data.frame(parameter_matrix)
colnames(parameter_matrix_dtf) = c("gamma.U0", "gamma.UG", "gamma.S0", "gamma.SG",
                                    "gamma.SG2", "gamma.SU", "gamma.Y0", "gamma.YG", "gamma.YU", "gamma.YG2", "gamma.UG2",
                                    "beta.1", "beta.0")

# Lists to save results
glm_s_g2_g_sum = vector("list", length = N)
glm_s_g2_g_coef = vector("list", length = N)

lm_Y_sen1_sum = vector("list", length = N)
lm_Y_sen1_coef = vector("list", length = N)

lm_Y_sen1_sum_beta_1 = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
lm_Y_sen1_sum_beta_0 = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
sd_lm_beta_1 = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
sd_lm_beta_0 = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])

pval_beta_1 = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
pval_beta_0 = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])

fstat_stage_1_MR_G = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
fstat_stage_1_MR_G2 = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])

mean_S = rep(NA, times=N)
sd_S = rep(NA, times=N)
mean_G = rep(NA, times=N)
sd_G = rep(NA, times=N)
mean_G2 = rep(NA, times=N)
sd_G2 = rep(NA, times=N)
mean_Y = rep(NA, times=N)
sd_Y = rep(NA, times=N)
var_beta1_beta0 =  rep(NA, times=N)
sd_beta1_beta0 =  rep(NA, times=N)

mean_S_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
sd_S_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
mean_G_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
sd_G_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
mean_G2_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
sd_G2_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
mean_Y_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
sd_Y_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])

var_beta1_beta0_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])
sd_beta1_beta0_per_n = matrix(NA, nrow =N, ncol = dim(parameter_matrix_dtf)[1])

# Simulation -------------------------------------------------------------------
for(k in 1:dim(parameter_matrix_dtf)[1]){
#for(k in 7:9){
  for(i in 1:N){

    data = data_gen_alspac_twoG(n, parameter_matrix_dtf, k)[[1]]                                    
    parameters = data_gen_alspac_twoG(n, parameter_matrix_dtf, k)[[2]]   

     # Data properties
    mean_S[i] = mean(data$S)
    sd_S[i] = sd(data$S)
    mean_G[i] = mean(data$G)
    sd_G[i] = sd(data$G)
    mean_G2[i] = mean(data$G2)
    sd_G2[i] = sd(data$G2)
    mean_Y[i] = mean(data$Y)
    sd_Y[i] = sd(data$Y) 

    # Stage 1: predict S with G and G2
    # Use G and G2 for estimation of SD
    glm_s_g2_g= glm(S ~ G2 + G, family="binomial", data = data)
    data$S_hat = glm_s_g2_g$fitted.values 
    glm_s_g2_g_sum[[i]] = summary(glm_s_g2_g)
    glm_s_g2_g_coef[[i]] = summary(glm_s_g2_g)$coefficients

    data$S_hat_G = data$S_hat * data$G
    data$S_hat_1_G = data$S_hat*(1-data$G)
    lm_Y_sen1 = lm(Y~S_hat_G + S_hat_1_G, data = data)
    lm_Y_sen1_sum[[i]] = summary(lm_Y_sen1)
    lm_Y_sen1_coef[[i]] = summary(lm_Y_sen1)$coefficients

    cov_mat = vcov(lm_Y_sen1)
    var_beta1_beta0[i] = cov_mat[2,2] + cov_mat[3,3] -2*cov_mat[2,3]
    sd_beta1_beta0[i] = sqrt(var_beta1_beta0[i])

  }

    mean_S_per_n[,k] = mean_S
    sd_S_per_n[,k] = sd_S

    mean_G_per_n[,k] = mean_G
    sd_G_per_n[,k] = sd_G

    mean_G2_per_n[,k] = mean_G2
    sd_G2_per_n[,k] = sd_G2

    mean_Y_per_n[,k] = mean_Y
    sd_Y_per_n[,k] = sd_Y
 
    var_beta1_beta0_per_n[,k] = var_beta1_beta0
    sd_beta1_beta0_per_n[,k] = sd_beta1_beta0

    lm_Y_sen1_sum_beta_1[,k] = sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[2,1]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))

    lm_Y_sen1_sum_beta_0[,k] = sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[3,1]},
                                   error = function(e){
                                     help_coef = NA
                                     return(help_coef)
                                   }))

    sd_lm_beta_1[,k] = sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[2,2]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))
    sd_lm_beta_0[,k] = sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[3,2]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))
    
    
    pval_beta_1[,k] = sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[2,4]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))
    pval_beta_0[,k] =sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[3,4]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))
    fstat_stage_1_MR_G2[,k] = sapply(glm_s_g2_g_sum, function(x) x$coefficients[2,1]**2/x$coefficients[2,2]**2) 
    fstat_stage_1_MR_G[,k] = sapply(glm_s_g2_g_sum, function(x) x$coefficients[3,1]**2/x$coefficients[3,2]**2) 

    print(k)
}

write.table(mean_S_per_n, file = paste(PATH_RESULTS, "mean_S_per_n.csv", sep = ""))
write.table(mean_G_per_n, file = paste(PATH_RESULTS, "mean_G_per_n.csv", sep = ""))
write.table(mean_G2_per_n, file = paste(PATH_RESULTS, "mean_G2_per_n.csv", sep = ""))
write.table(mean_Y_per_n, file = paste(PATH_RESULTS, "mean_Y_per_n.csv", sep = ""))

write.table(sd_S_per_n, file = paste(PATH_RESULTS, "sd_S_per_n.csv", sep = ""))
write.table(sd_G_per_n, file = paste(PATH_RESULTS, "sd_G_per_n.csv", sep = ""))
write.table(sd_G2_per_n, file = paste(PATH_RESULTS, "sd_G2_per_n.csv", sep = ""))
write.table(sd_Y_per_n, file = paste(PATH_RESULTS, "sd_Y_per_n.csv", sep = ""))

write.table(parameter_matrix_dtf, file = paste(PATH_RESULTS, "parameter_matrix_scenario_1.csv", sep = ""))

write.table(sd_lm_beta_1, file = paste(PATH_RESULTS, "sd_lm_beta_1.csv", sep = ""))
write.table(sd_lm_beta_0, file = paste(PATH_RESULTS, "sd_lm_beta_0.csv", sep = ""))

write.table(lm_Y_sen1_sum_beta_1, file = paste(PATH_RESULTS, "lm_Y_sen1_sum_beta_1.csv", sep = ""))
write.table(lm_Y_sen1_sum_beta_0, file = paste(PATH_RESULTS, "lm_Y_sen1_sum_beta_0.csv", sep = ""))

write.table(pval_beta_1, file = paste(PATH_RESULTS, "pval_beta_1.csv", sep = ""))
write.table(pval_beta_0, file = paste(PATH_RESULTS, "pval_beta_0.csv", sep = ""))

write.table(fstat_stage_1_MR_G2, file = paste(PATH_RESULTS, "fstat_stage_1_MR_G2.csv", sep = ""))
write.table(fstat_stage_1_MR_G, file = paste(PATH_RESULTS, "fstat_stage_1_MR_G.csv", sep = ""))

write.table(var_beta1_beta0_per_n, file = paste(PATH_RESULTS, "var_beta1_beta0_per_n.csv", sep = ""))
write.table(sd_beta1_beta0_per_n, file = paste(PATH_RESULTS, "sd_beta1_beta0_per_n.csv", sep = ""))
