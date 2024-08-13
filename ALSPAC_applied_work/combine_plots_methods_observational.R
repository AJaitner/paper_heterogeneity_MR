rm(list = ls())
library(tidyverse)
library(here)

PROJECT_DIR = here("ALSPAC_applied_work/")

res_1st_3m = read.table(paste(PROJECT_DIR, "Results/1st3M_m1_m2_adj_diff_exp_out_exc_mheight_outcome_stage2.csv", sep =""))
res_pre_preg = read.table(paste(PROJECT_DIR, "Results/pre_m1_m2_adj_diff_exp_out_stage2.csv", sep =""))

res_obs = read.table(paste(PROJECT_DIR, "Results/observational_res.csv", sep =""), header = TRUE)

res_st_mr = read.table(paste(PROJECT_DIR, "Results/standard_MR_results.csv", sep =""), header = TRUE)
res_st_mr_conf = res_st_mr %>% filter(Confounders == "PC1-10, conf")

res_meth = rbind(res_1st_3m, res_pre_preg)
res_meth = as.data.frame(res_meth)
res_meth$smoking_var_char = c(rep("S = Smoking in the 1st 3 months", 6), rep("S = Smoking before pregnancy", 6))
res_meth$smoking_var_char = factor(res_meth$smoking_var_char, levels = c("S = Smoking before pregnancy", "S = Smoking in the 1st 3 months"))
# Add observational results
res_obs = rbind(c("Observational", "Observational", res_obs$Est[2], res_obs$SE[2], 
                res_obs$Lower[2], res_obs$Higher[2],res_obs$Pval[2],res_obs$n[2], "b663_yn",NA, "S = Smoking before pregnancy"),
                c("Observational", "Observational", res_obs$Est[5],res_obs$SE[5], 
                res_obs$Lower[5], res_obs$Higher[5],res_obs$Pval[5],res_obs$n[5], "b665_yn", NA, "S = Smoking in the 1st 3 months"))
res_obs = as.data.frame(res_obs)
for(i in 3:7){
    res_obs[,i] = as.numeric(res_obs[,i])
}
colnames(res_obs) = names(res_meth)

# Add standard MR results
res_st_mr_conf = rbind(c("Instrument G", "Standard MR", res_st_mr_conf$Est_2[4], res_st_mr_conf$Std_E_2[4], 
                res_st_mr_conf$Est_2[4]-1.96*res_st_mr_conf$Std_E_2[4], res_st_mr_conf$Est_2[4]+1.96*res_st_mr_conf$Std_E_2[4],res_st_mr_conf$P_val_2[4], res_st_mr_conf$n_stage1[4], "b665_yn",NA, "S = Smoking in the 1st 3 months"),
                c("Instrument G2", "Standard MR", res_st_mr_conf$Est_2[3], res_st_mr_conf$Std_E_2[3], 
                res_st_mr_conf$Est_2[3]-1.96*res_st_mr_conf$Std_E_2[3], res_st_mr_conf$Est_2[3]+1.96*res_st_mr_conf$Std_E_2[3],res_st_mr_conf$P_val_2[3], res_st_mr_conf$n_stage1[3], "b665_yn",NA, "S = Smoking in the 1st 3 months"),
                c("Instrument G2", "Standard MR", res_st_mr_conf$Est_2[1], res_st_mr_conf$Std_E_2[1], 
                res_st_mr_conf$Est_2[1]-1.96*res_st_mr_conf$Std_E_2[1], res_st_mr_conf$Est_2[1]+1.96*res_st_mr_conf$Std_E_2[1],res_st_mr_conf$P_val_2[1], res_st_mr_conf$n_stage1[1], "b663_yn", NA, "S = Smoking before pregnancy")
                )
res_st_mr_conf = as.data.frame(res_st_mr_conf)
for(i in 3:8){
    res_st_mr_conf[,i] = as.numeric(res_st_mr_conf[,i])
}
colnames(res_st_mr_conf) = names(res_meth)

res_meth_obs = rbind(res_meth, res_obs, res_st_mr_conf)

res_meth_obs$label_name = paste0(round(res_meth_obs$Est_value, 0)," (SE: ", round(res_meth_obs$SE, 0), ")" )

write.table(res_meth_obs, file = paste(PROJECT_DIR, "Results/MR_meth_ob_results_combined.csv", sep = ""),  row.names = FALSE)

cols_1 <- c("Method_1" = "#B32070","Method_2" = "#360FC5","Standard MR"="#005b57", "Observational"="#00C0B8")

p = ggplot(data=res_meth_obs,
    aes(x = Est_name,y = Est_value))+
    geom_point(aes(col=factor(Method, c("Method_2", "Method_1","Standard MR", "Observational")), size = 1/(SE^2)), position=position_dodge(width=1))+
    geom_hline(yintercept=0, linetype="dashed") +
    geom_text(aes(label = label_name, color=factor(Method, c("Method_2", "Method_1","Standard MR", "Observational"))),position=position_dodge(width=1),  #hjust = 0.5,
              vjust = -1.5, hjust = 0.35,show.legend = FALSE, size = 8/.pt) +
    xlab('')+ ylab("Estimate of change in birth weight [g]")+
    geom_errorbar(aes(ymin=Lower_CI, ymax=Higher_CI,col=factor(Method, c("Method_2", "Method_1","Standard MR", "Observational"))),width=0.4,cex=1, position=position_dodge(width=1))+ 
    facet_wrap(~smoking_var_char,strip.position="right",nrow=2,scales = "free_y") +
    scale_x_discrete(labels=c(
                            'Beta_1-Beta_0'=expression(beta*1-beta*0),
                            'Beta_1'=expression(beta*1),
                            'Beta_0'=expression(beta*0),
                            'Instrument G' = 'Instrument G', 
                            'Instrument G2' = 'Instrument G2',
                            'Observational'='Observational')) +
    coord_flip() +
    theme_bw()+
    scale_color_manual(values=cols_1) +
    guides(color=guide_legend(reverse = TRUE, title="Method")) +
    scale_radius(range=c(2.5,5)) +
    guides(size = "none")
   
ggsave(paste(PROJECT_DIR, "plots/sm_meth_one_plot.png", sep =""), plot = p) 

