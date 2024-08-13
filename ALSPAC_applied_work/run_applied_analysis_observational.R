rm(list = ls())

library(tidyverse)
library(here)

PROJECT_DIR = here("ALSPAC_applied_work/")
# load the data -------------------------------------------------------------
setwd(PROJECT_DIR)
source("load_data_and_modify.R")

dim(data_with_gnetic_score)

# Observational analysis with pre-pregnancy smoking -----------------------------------------------
lm_b663_0 = lm(kz030 ~ b663_yn, data = data_with_gnetic_score)
summary(lm_b663_0)

lm_b663_1 = lm(kz030 ~ b663_yn + ptnr_sm + mother_age+ kz021 + mothers_height + parity + mothers_prepreg_weight, data = data_with_gnetic_score)
summary(lm_b663_1)

lm_b663_2 = lm(kz030 ~ b663_yn + mother_age+ kz021 + mothers_height + parity + mothers_prepreg_weight, data = data_with_gnetic_score)
summary(lm_b663_2)

# Observational analysis with smoking in the first three months of pregnancy -----------------------------------------------
lm_b665_0 = lm(kz030 ~ b665_yn, data = data_with_gnetic_score)
summary(lm_b665_0)

lm_b665_1 = lm(kz030 ~ b665_yn + ptnr_sm + mother_age+ kz021 + mothers_height + parity + mothers_prepreg_weight, data = data_with_gnetic_score)
summary(lm_b665_1)

lm_b665_2 = lm(kz030 ~ b665_yn + mother_age+ kz021 + mothers_height + parity + mothers_prepreg_weight, data = data_with_gnetic_score)
summary(lm_b665_2)

# Plot results: 

obs_res_data = rbind(summary(lm_b663_0)$coeff[2,],
                    summary(lm_b663_1)$coeff[2,],
                    summary(lm_b663_2)$coeff[2,],                    
                    summary(lm_b665_0)$coeff[2,], 
                    summary(lm_b665_1)$coeff[2,],
                    summary(lm_b665_2)$coeff[2,]) 
obs_res_data = as.data.frame(obs_res_data)
colnames(obs_res_data) = c("Est", "SE", "Tval", "Pval")
obs_res_data$Smoking_exp = c("Pre-preg sm","Pre-preg sm","Pre-preg sm", "1st 3 months", "1st 3 months", "1st 3 months")
obs_res_data$Name = c("No adjustment", "All covariates", "All covariates (except partner smoking)", "No adjustment", "All covariates", "All covariates (except partner smoking)")

obs_res_data$Lower = obs_res_data$Est -1.96* obs_res_data$SE
obs_res_data$Higher = obs_res_data$Est +1.96* obs_res_data$SE
obs_res_data$n = rbind(dim(lm_b663_0$model)[1],
                    dim(lm_b663_1$model)[1],
                    dim(lm_b663_2$model)[1],                    
                    dim(lm_b665_0$model)[1], 
                    dim(lm_b665_1$model)[1],
                    dim(lm_b665_2$model)[1])
write.table(obs_res_data, file = paste(PROJECT_DIR, "Results/observational_res.csv", sep = ""), row.names = FALSE)

# Plot: 
p = ggplot(obs_res_data, aes(Est, Name)) +
    geom_point(aes(color=factor(Smoking_exp)), position=position_dodge(width=1), size=2) +
    geom_errorbarh(data = obs_res_data, aes(xmin=Lower, xmax=Higher, color=factor(Smoking_exp)),  height=.3, position=position_dodge(width=1)) + # position=position_dodge(width=1),
    geom_vline(xintercept=-100, linetype="dashed") +
    scale_x_continuous("Estimates") +
    guides(color=guide_legend(reverse = TRUE, title="Method")) +
    theme_classic() + #+ scale_color_manual(values=cols_1) + # "#A6A6A6" "#767171"
    theme(plot.margin=margin(10,10,10,10)) + 
    theme(text=element_text(size=18)) + labs(y=NULL) 

ggsave(paste(PROJECT_DIR, "Plots/observational_sm_bw.png", sep = ""), p, width=10, height=4, dpi=300)

obs_res_data_mod = obs_res_data %>%
                        filter(Name != "All covariates (except partner smoking)")

p = ggplot(obs_res_data_mod, aes(Est, Name)) +
    geom_point(aes(color=factor(Smoking_exp)), position=position_dodge(width=1), size=2) +
    geom_errorbarh(data = obs_res_data_mod, aes(xmin=Lower, xmax=Higher, color=factor(Smoking_exp)),  height=.3, position=position_dodge(width=1)) + # position=position_dodge(width=1),
    geom_vline(xintercept=-100, linetype="dashed") +
    scale_x_continuous("Estimates") +
    guides(color=guide_legend(reverse = TRUE, title="Method")) +
    theme_classic() + #+ scale_color_manual(values=cols_1) + # "#A6A6A6" "#767171"
    theme(plot.margin=margin(10,10,10,10)) + 
    theme(text=element_text(size=18)) + labs(y=NULL) 

ggsave(paste(PROJECT_DIR, "Plots/observational_sm_bw_mod.png", sep = ""), p, width=10, height=4, dpi=300)

