library(tidyverse)
# library(remotes)
# #remotes::install_github("lukepilling/twistR")
#library(twistR)
#library(AER)
#library(ggplot2)
#library(reshape)

rm(list = ls())
start.time <- Sys.time()

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
parameter_matrix[,8] = 80 #c(0,0.8,0.8) # par.YG
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
rgmte_est_lm_sum = vector("list", length = N)
rgmte_est_lm_coef = vector("list", length = N)
glm_sstar_g_sum = vector("list", length = N)
glm_sstar_g_coef = vector("list", length = N)

glm_s_g2_sum = vector("list", length = N)
glm_s_g2_coef = vector("list", length = N)

lm_ynew_shat_sum =  vector("list", length = N)
lm_ynew_shat_coef =  vector("list", length = N)


# Repeat calculatioin for N simualtions --------------------------------------------------------------
for(i in 1:N){
    data = data_gen_alspac_twoG(n, parameter_matrix_dtf, 1)[[1]]                                    
    parameters = data_gen_alspac_twoG(n, parameter_matrix_dtf, 1)[[2]]    

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

 
    rgmte_est = sapply(rgmte_est_lm_sum, function(x) x$coefficients[3,1]) # [4,1] for jacks calculation of RGMTE
    beta_0_est = sapply(lm_ynew_shat_sum, function(x) tryCatch({help_coef =x$coefficients[2,1]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))

    sd_rgmte = sapply(rgmte_est_lm_sum, function(x) x$coefficients[3,2]) # [4,1] for jacks calculation of RGMTE
    sd_beta_0 = sapply(lm_ynew_shat_sum, function(x) tryCatch({help_coef =x$coefficients[2,2]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))                                  
    fstat_stage_1_MR = sapply(glm_s_g2_sum, function(x) x$coefficients[2,1]**2/x$coefficients[2,2]**2)

     # Store p-value for power calculation
    pval_rgmte = sapply(rgmte_est_lm_sum, function(x) x$coefficients[3,4])
    pval_beta_0 = sapply(lm_ynew_shat_sum, function(x) tryCatch({help_coef =x$coefficients[2,4]},
                                  error = function(e){
                                    help_coef = NA
                                    return(help_coef)
                                  }))

# save mean results for N simulations -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------
source("./Sample_size_sim/Low_effect_diff/save_results_N_sim_meth2.R")


#################################################################################################################
write.table(res_table, file = paste(PATH_RESULTS,s, "_res_table_method_2.csv", sep = ""), row.names = FALSE)
write.table(parameter_matrix_dtf, file = paste(PATH_RESULTS,"parameter_matrix/", s, "_parameter_matrix_method_2.csv", sep = ""), row.names = FALSE)

#res_table
print(s)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

