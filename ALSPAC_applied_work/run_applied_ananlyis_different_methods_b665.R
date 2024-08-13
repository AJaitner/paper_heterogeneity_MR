library('tidyverse')
library('here')

PROJECT_DIR = here("ALSPAC_applied_work/")

# load the data -------------------------------------------------------------
setwd(PROJECT_DIR)
source("load_dataset.R")

dim(data_with_genetic_score) [1]
# [1] 7752

############################################################################################################################################################
source("run_analysis_with_different_conf_fct.R")

Conf_exp ="ptnr_sm+mother_age+parity"
Conf_out = "offspring_sex+mothers_prepreg_weight+mother_age+mothers_height+parity"
run_analysis_with_different_conf(exp ="sm_1st3month_yn", Conf_exp, Conf_out, res_name = "res_sm_1st3month_yn", PROJECT_DIR, dataset =data_with_genetic_score)
