rm(list = ls())

library(tidyverse)
library(here)

PROJECT_DIR = here("ALSPAC_applied_work/")
# load the data -------------------------------------------------------------
setwd(PROJECT_DIR)
source("load_dataset.R")

dim(data_with_gnetic_score)
########################################################################################################################################
source('different_method_fct.R')

PCs = "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10"
Z_exp = PCs
Z_out = PCs 
b663_g2_pc = standard_MR(Y = "kz030", S = "b663_yn", I = "weighted_allele_score_sminit", Z_exp, Z_out, data = data_with_gnetic_score)
b663_g_pc =standard_MR(Y = "kz030", S = "b663_yn", I= "chr15.78894339_G_A_01", Z_exp, Z_out, data = data_with_gnetic_score)

b665_g2_pc = standard_MR(Y = "kz030", S = "b665_yn", I = "weighted_allele_score_sminit", Z_exp, Z_out, data = data_with_gnetic_score)
b665_g_pc =standard_MR(Y = "kz030", S = "b665_yn", I= "chr15.78894339_G_A_01", Z_exp, Z_out, data = data_with_gnetic_score)

# adjust for mothers age and partner smoking in the exposure model
# adjust for offpsring sex, mothers age, prepreg weight, parity and mothers height in the outcome model
Conf_exp ="ptnr_sm+mother_age"
Conf_out = "kz021+mother_age+mothers_height+parity+mothers_prepreg_weight"
Z_exp = paste0(PCs, "+", Conf_exp)
Z_out = paste0(PCs, "+", Conf_out) 

b663_g2_pc_conf = standard_MR(Y = "kz030", S = "b663_yn", I = "weighted_allele_score_sminit", Z_exp, Z_out, data = data_with_gnetic_score)
b663_g_pc_conf =standard_MR(Y = "kz030", S = "b663_yn", I= "chr15.78894339_G_A_01", Z_exp, Z_out, data = data_with_gnetic_score)

# adjust for mothers age, partner smoking, parity and mothers height in the exposure model
# adjust for offpsring sex, mothers age, prepreg weight, parity and mothers height in the outcome model

Conf_exp ="ptnr_sm+mother_age+parity+mothers_height"
Conf_out = "kz021+mother_age+mothers_height+parity+mothers_prepreg_weight"
Z_exp = paste0(PCs, "+", Conf_exp)
Z_out = paste0(PCs, "+", Conf_out) 

b665_g2_pc_conf = standard_MR(Y = "kz030", S = "b665_yn", I = "weighted_allele_score_sminit", Z_exp, Z_out, data = data_with_gnetic_score)
b665_g_pc_conf =standard_MR(Y = "kz030", S = "b665_yn", I= "chr15.78894339_G_A_01", Z_exp, Z_out, data = data_with_gnetic_score)

source('read_res_MR_fct.R')
# Inputs: model_1, model_2, exposure, outcome, genetic_instrument, confounder_char
results = matrix(NA, nrow = 8, ncol = 15)
results[1,] = read_res_MR(b663_g2_pc[[2]], b663_g2_pc[[3]], "b663_yn", "kz030", "weighted_allele_score_sminit", "PC1-10")
results[2,] = read_res_MR(b663_g_pc[[2]], b663_g_pc[[3]], "b663_yn", "kz030", "chr15.78894339_G_A_01", "PC1-10")
results[3,] = read_res_MR(b663_g2_pc_conf[[2]], b663_g2_pc_conf[[3]], "b663_yn", "kz030", "weighted_allele_score_sminit", "PC1-10, conf")
results[4,] = read_res_MR(b663_g_pc_conf[[2]], b663_g_pc_conf[[3]], "b663_yn", "kz030", "chr15.78894339_G_A_01", "PC1-10, conf")

results[5,] = read_res_MR(b665_g2_pc[[2]], b665_g2_pc[[3]], "b665_yn", "kz030", "weighted_allele_score_sminit", "PC1-10")
results[6,] = read_res_MR(b665_g_pc[[2]], b665_g_pc[[3]], "b665_yn", "kz030", "chr15.78894339_G_A_01", "PC1-10")
results[7,] = read_res_MR(b665_g2_pc_conf[[2]], b665_g2_pc_conf[[3]], "b665_yn", "kz030", "weighted_allele_score_sminit", "PC1-10, conf")
results[8,] = read_res_MR(b665_g_pc_conf[[2]], b665_g_pc_conf[[3]], "b665_yn", "kz030", "chr15.78894339_G_A_01", "PC1-10, conf")

colnames(results) = c("Instrument", "Exposure", "Outcome", 
                      "Fstat", "Est_1", "Std_E_1", "T_val_1", "P_val_1", "n_stage1", 
                      "Est_2", "Std_E_2", "T_val_2", "P_val_2","n_stage1", "Confounders")

results
write.table(results, file = paste(PROJECT_DIR, "Results/standard_MR_results.csv", sep = ""),  row.names = FALSE)
