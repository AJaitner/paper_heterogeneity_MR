rm(list = ls())

library(tidyverse)
liibrary(here)

PROJECT_DIR = here("ALSPAC_applied_work/")
# load the data -------------------------------------------------------------
setwd(PROJECT_DIR)
source("load_dataset.R")

dim(data_with_gnetic_score)

# Check Pr(S=1,G=1) and Pr(S=1,G=0)
mean(data_with_gnetic_score$b663_yn[data_with_gnetic_score$chr15.78894339_G_A_01 ==1], na.rm = TRUE)
mean(data_with_gnetic_score$b663_yn[data_with_gnetic_score$chr15.78894339_G_A_01 ==0], na.rm = TRUE)

length(data_with_gnetic_score$b663_yn[data_with_gnetic_score$chr15.78894339_G_A_01 ==1])
length(which(is.na(data_with_gnetic_score$b663_yn[data_with_gnetic_score$chr15.78894339_G_A_01 ==1])))

length(data_with_gnetic_score$b663_yn[data_with_gnetic_score$chr15.78894339_G_A_01 ==0])
length(which(is.na(data_with_gnetic_score$b663_yn[data_with_gnetic_score$chr15.78894339_G_A_01 ==0])))

############################################################################################################################################################
source("run_analysis_with_different_conf_fct.R")

Conf_exp ="ptnr_sm+mother_age"
Conf_out = "kz021+mother_age+mothers_height+parity+mothers_prepreg_weight"
run_analysis_with_different_conf(exp ="sm_pre_preg_yn", Conf_exp, Conf_out, res_name = "sm_pre_preg_yn", PROJECT_DIR, dataset =data_with_gnetic_score)
