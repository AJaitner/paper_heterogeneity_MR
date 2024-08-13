library('tidyverse')
library('here')

PROJECT_DIR = here("ALSPAC_applied_work/")
DATA_DIR = here("ALSPAC_applied_work", "data/")

setwd(PROJECT_DIR)

# Load ALSPAC data 
# including SNP information for rs1051730, 
# and mothers PCs 

dim(full_data_combine)[1]
# [1] 8744

source("data_filter_fct.R")
full_data_filter = data_filter_fct(full_data_combine)
dim(full_data_filter)[1]
# [1] 7752

# Recode variables
full_data_smoked_preg = full_data_filter %>%
        # Pre-pregnancy smoking coded as 0:non-smoker and 1:smoker
        mutate(b663_yn = ifelse(b663 ==1, 0, 1)) %>% 
        # Smoking in the first three month of pregnancy coded as 0:non-smoker and 1:smoker
        mutate(b665_yn = ifelse(b665 == 1, 0, 1 )) %>%
        # partner smoking is stored in different variables, combine and code with 0:non-smoker and 1:smoker
        mutate(ptnr_sm = case_when(b683 == 1 ~ 0, b683 == 2 ~ 1, b683 == 3 ~ 1, b683 == 4 ~ 1, b683 == 5 ~ 1, TRUE ~ NA_real_)) %>%
        mutate(mother_bw = ifelse(dw032 <0, NA, dw032)) %>%
        mutate(mother_age = ifelse(mz028b <0, NA, mz028b)) %>% # mothers ages at delivery(<16 is set to 15 and >43 is set to 44)
        mutate(mothers_prepreg_weight = ifelse(dw002 <0, NA, dw002)) %>% # in kg
        mutate(parity = ifelse(b032 <0, NA, b032)) %>% # number of previous children. If 0, then this is the first child
        mutate(mothers_height = ifelse(dw021 <0, NA, dw021)) # in cm
dim(full_data_smoked_preg)

# Load weighted allele score for smoking initiaiton
# Information for smoking initiaiton SNPs identified in Lui et at (2019)
# grs_weighted_allele_score_sminit file: 
        # first column: individual IDs, 
        # second column: the weighted allele scores for smoking initiation
weighted_allele = read.table(paste0(DATA_DIR, "grs_weighted_allele_score_sminit"), header = TRUE)
data_with_genetic_score = left_join(full_data_smoked_preg, weighted_allele, by = "ID")
                     
# Dicotomise G
data_with_genetic_score = data_with_genetic_score %>%
                                mutate(rs1051730= ifelse(rs1051730 <0.5, 0, 1))
