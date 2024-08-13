library(tidyverse)
library(here)
rm(list = ls())

args = commandArgs(TRUE)
scen_number = as.numeric(args[1])
N = as.numeric(args[2]) # number of simulation runs

print(scen_number)
print(N)

# Load Functions ---------------------------------------------------------------
# set path for saving
source(here("sample_size_sim", "data_gen_alspac_fct.R")
PATH = here("sample_size_sim", "Low_effect_diff"/)
PATH_RESULTS = paste0(PATH, "Results/")
setwd(PATH)

# Simulation parameters --------------------------------------------------------
set.seed(3456)

n = c(7000, 10000, 50000,100000, 200000, 300000, 500000)
beta_1 = c(-168, -180, -200, -220)
scen = expand.grid(n, beta_1)
scen$beta_0 = c(rep(-163,7), rep(-170,7), rep(-185,7), rep(-200,7))
colnames(scen) = c("n", "beta_1", "beta_0")

s = scen_number 
n = scen$n[s]

# Parameter Matrix --------------------------------------------------------------
parameter_matrix = matrix(NA, nrow = 1, ncol = 13)

parameter_matrix[,1] = 0 #par.U0
par.UG = parameter_matrix[,2] = 0  # par.UG
parameter_matrix[,3] = -2 # par.S0
par.SDG = parameter_matrix[,4] = 0.2 # par.SG
parameter_matrix[,5] = 0.8  # par.SG2 
parameter_matrix[,6] = 1.6 # par.SU
parameter_matrix[,7] = 3500 # par.Y0
parameter_matrix[,8] = 0 #c(0,0.8,0.8) # par.YG
parameter_matrix[,9] = 80 # par.YU
parameter_matrix[,10] = 0 # par.YG2
parameter_matrix[,11] = 0 # par.UG2
parameter_matrix[,12] = scen$beta_1[s] # beta.1
parameter_matrix[,13] = scen$beta_0[s] #beta.0

parameter_matrix_dtf = as.data.frame(parameter_matrix)
colnames(parameter_matrix_dtf) = c("gamma.U0", "gamma.UG", "gamma.S0", "gamma.SG",
                                    "gamma.SG2", "gamma.SU", "gamma.Y0", "gamma.YG", "gamma.YU", "gamma.YG2", "gamma.UG2",
                                    "beta.1", "beta.0")

# Lists to save results --------------------------------------------------------------
glm_s_g2_g_sum = vector("list", length = N)
glm_s_g2_g_coef = vector("list", length = N)

lm_Y_sen1_sum = vector("list", length = N)
lm_Y_sen1_coef = vector("list", length = N)

var_beta1_beta0 =  rep(NA, times=N)
sd_beta1_beta0 =  rep(NA, times=N)

# Repeat calculatioin for N simualtions --------------------------------------------------------------
for(i in 1:N){
    data = data_gen_alspac_twoG(n, parameter_matrix_dtf, 1)[[1]]                                    
    parameters = data_gen_alspac_twoG(n, parameter_matrix_dtf, 1)[[2]]    

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

 
var_beta1_beta0_per_n = var_beta1_beta0
sd_beta1_beta0_per_n = sd_beta1_beta0

lm_Y_sen1_sum_beta_1 = sapply(lm_Y_sen1_sum, function(x) x$coefficients[2,1])
lm_Y_sen1_sum_beta_0 = sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[3,1]},
                                  error = function(e){
                                  help_coef = NA
                                  return(help_coef)
                                  }))

sd_lm_beta_1 = sapply(lm_Y_sen1_sum, function(x) x$coefficients[2,2])
sd_lm_beta_0 = sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[3,2]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))
    
    
pval_beta_1 = sapply(lm_Y_sen1_sum, function(x) x$coefficients[2,4])
pval_beta_0 =sapply(lm_Y_sen1_sum, function(x) tryCatch({help_coef = x$coefficients[3,4]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))
fstat_stage_1_MR_G2 = sapply(glm_s_g2_g_sum, function(x) x$coefficients[2,1]**2/x$coefficients[2,2]**2) 
fstat_stage_1_MR_G = sapply(glm_s_g2_g_sum, function(x) x$coefficients[3,1]**2/x$coefficients[3,2]**2) 

# save mean results for N simulations -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------
source('./Sample_size_sim/Low_effect_diff/save_results_N_sim_meth1.R')


#################################################################################################################
write.table(res_table, file = paste(PATH_RESULTS,s, "_res_table_method_1.csv", sep = ""), row.names = FALSE)
write.table(parameter_matrix_dtf, file = paste(PATH_RESULTS,s, "_parameter_matrix_method_1.csv", sep = ""), row.names = FALSE)

print(s)


