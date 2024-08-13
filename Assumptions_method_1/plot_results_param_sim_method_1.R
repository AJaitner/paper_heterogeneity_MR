library(tidyverse)
library(ggplot2)
library(here)
library(reshape2)
library(xtable)
library(cowplot)

rm(list = ls())

# Load Functions ---------------------------------------------------------------
# set path for saving
PATH = here("Assumptions_method_1")
RESULTS_PATH = "Results/" 
setwd(PATH)

#read_data_function = function(RESULTS_PATH){
beta_0_matrix_scen1 = as.data.frame(read.table(paste(RESULTS_PATH,"lm_Y_sen1_sum_beta_0.csv", sep = "")))
beta_1_matrix_scen1 = as.data.frame(read.table(paste(RESULTS_PATH,"lm_Y_sen1_sum_beta_1.csv", sep = "")))
beta1_beta0 = beta_1_matrix_scen1 - beta_0_matrix_scen1

beta_0_sd = as.data.frame(read.table(paste(RESULTS_PATH,"sd_lm_beta_0.csv", sep = "")))
beta_1_sd = as.data.frame(read.table(paste(RESULTS_PATH,"sd_lm_beta_1.csv", sep = "")))
beta1_beta0_sd = as.data.frame(read.table(paste(RESULTS_PATH,"sd_beta1_beta0_per_n.csv", sep = "")))

fstat_stage_1_MR_G2 = as.data.frame(read.table(paste(RESULTS_PATH, "fstat_stage_1_MR_G2.csv", sep = "")))
fstat_stage_1_MR_G = as.data.frame(read.table(paste(RESULTS_PATH, "fstat_stage_1_MR_G.csv", sep = "")))

# Data properties: 
mean_S_per_n = colMeans(as.data.frame(read.table(paste(RESULTS_PATH,"mean_S_per_n.csv", sep = ""))))
mean_G_per_n = colMeans(as.data.frame(read.table(paste(RESULTS_PATH,"mean_G_per_n.csv", sep = ""))))
mean_G2_per_n = colMeans(as.data.frame(read.table(paste(RESULTS_PATH,"mean_G2_per_n.csv", sep = ""))))
mean_Y_per_n = colMeans(as.data.frame(read.table(paste(RESULTS_PATH,"mean_Y_per_n.csv", sep = ""))))

sd_S_per_n = colMeans(as.data.frame(read.table(paste(RESULTS_PATH,"sd_S_per_n.csv", sep = ""))))
sd_G_per_n = colMeans(as.data.frame(read.table(paste(RESULTS_PATH,"sd_G_per_n.csv", sep = ""))))
sd_G2_per_n = colMeans(as.data.frame(read.table(paste(RESULTS_PATH,"sd_G2_per_n.csv", sep = ""))))
sd_Y_per_n = colMeans(as.data.frame(read.table(paste(RESULTS_PATH,"sd_Y_per_n.csv", sep = ""))))

parameter_matrix_scenario_1 = read.table(paste(RESULTS_PATH,"parameter_matrix_scenario_1.csv", sep = ""))
true_values_beta0 = parameter_matrix_scenario_1$beta.0 
true_values_beta1 = parameter_matrix_scenario_1$beta.1
true_values_beta1_beta0 = true_values_beta1 - true_values_beta0

N = dim(beta_0_matrix_scen1)[1]
num_scen = dim(beta_0_matrix_scen1)[2]
n = 20000

beta_0_est_mean = colMeans(beta_0_matrix_scen1)
beta_0_est_sd = colMeans(beta_0_sd)/sqrt(N)

beta_1_est_mean = colMeans(beta_1_matrix_scen1)
beta_1_est_sd = colMeans(beta_1_sd)/sqrt(N)

beta1_beta0_est_mean = colMeans(beta1_beta0)
beta_1_est_sd = colMeans(beta1_beta0_sd)/sqrt(N)

# save mean results with parameter values
res_table_latex = cbind(rep(n,num_scen ), rep(N, num_scen), parameter_matrix_scenario_1,
                colMeans(fstat_stage_1_MR_G2), colMeans(fstat_stage_1_MR_G),
                beta_0_est_mean, beta_0_est_sd, beta_1_est_mean, beta_1_est_sd, beta1_beta0_est_mean, beta_1_est_sd, 
                mean_S_per_n,mean_G_per_n, mean_G2_per_n, mean_Y_per_n, sd_S_per_n, sd_G_per_n, sd_G2_per_n,sd_Y_per_n )
colnames(res_table_latex)= c("n", "N", names(parameter_matrix_scenario_1), 
                            "FSTAT_G2", "FSTAT_G", 
                            "BETA_0_EST", "SD BETA_0_EST",  "BETA_1_EST", "SD BETA_1_EST",  "BETA1_BETA0_EST", "SD BETA1_BETA0_EST", 
                            "mean_S_per_n","mean_G_per_n", "mean_G2_per_n", "mean_Y_per_n", "sd_S_per_n", "sd_G_per_n", "sd_G2_per_n","sd_Y_per_n" )
res_table_latex = as.data.frame(res_table_latex)
res_table_latex
write.table(res_table_latex, file = paste(RESULTS_PATH, "res_table_method_1.csv", sep = ""),  row.names = FALSE)

colnames(beta_0_matrix_scen1) = c("1", "2", "3", "4", "5", "6", "7", "8", "9")
set_colours = c("#66c2a5","#fc8d62", "#8da0cb")
# one plot
# Beta_0
beta_0_matrix_scen1_melt <- melt(beta_0_matrix_scen1[,1:3]) 

p0_all_1 <- ggplot(aes(x=value, colour=variable), data=beta_0_matrix_scen1_melt) +
            geom_density(show.legend=FALSE) +  stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta0, linetype='dashed') + 
            ylim(0,0.02) + xlim(-500,500) +labs(x = expression(beta*0), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

beta_0_matrix_scen1_melt <- melt(beta_0_matrix_scen1[,4:6]) 
p0_all_2 <- ggplot(aes(x=value, colour=variable), data=beta_0_matrix_scen1_melt) +
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta0, linetype='dashed') +
            ylim(0,0.02) + xlim(-500,500) +labs(x = expression(beta*0), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

beta_0_matrix_scen1_melt <- melt(beta_0_matrix_scen1[,7:9]) 
p0_all_3 <- ggplot(aes(x=value, colour=variable), data=beta_0_matrix_scen1_melt) +
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta0, linetype='dashed') +
            ylim(0,0.02) + xlim(-500,500) +labs(x = expression(beta*0), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

p0_all = plot_grid(p0_all_1,p0_all_2,p0_all_3, ncol = 1)

ggsave(paste(PATH, "Plots/beta_0_density_all_settings_m1.png", sep =""), plot = p0_all)

colnames(beta_1_matrix_scen1) = c("1", "2", "3", "4", "5", "6", "7", "8", "9")
# Beta_1
beta_1_matrix_scen1_melt <- melt(beta_1_matrix_scen1[,1:3]) 

p1_all_1 <- ggplot(aes(x=value, colour=variable), data=beta_1_matrix_scen1_melt) +
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta1, linetype='dashed') +
            ylim(0,0.03) + xlim(-500,500) + labs(x = expression(beta*1), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

beta_1_matrix_scen1_melt <- melt(beta_1_matrix_scen1[,4:6]) 

p1_all_2 <- ggplot(aes(x=value, colour=variable), data=beta_1_matrix_scen1_melt) +
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta1, linetype='dashed') +
            ylim(0,0.03) + xlim(-500,500) +labs(x = expression(beta*1), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

beta_1_matrix_scen1_melt <- melt(beta_1_matrix_scen1[,7:9]) 

p1_all_3 <- ggplot(aes(x=value, colour=variable), data=beta_1_matrix_scen1_melt) +
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta1, linetype='dashed') +
            ylim(0,0.03) + xlim(-500,500) +labs(x = expression(beta*1), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

p1_all = plot_grid(p1_all_1,p1_all_2,p1_all_3, ncol = 1)
ggsave(paste(PATH, "Plots/beta_1_density_all_settings_m1.png", sep =""), plot = p1_all)

# Beta_0 and beta_1 in one plot
p0_all_no_legend = plot_grid(p0_all_1  + theme(legend.position = "none"),p0_all_2 + theme(legend.position = "none") ,p0_all_3 + theme(legend.position = "none") , ncol = 1)

p1_p0_combined_leg = plot_grid(p0_all_no_legend, p1_all, rel_widths = c(1, 1.1))
ggsave(paste(PATH, "Plots/beta_0_beta_1_combined_density_all_settings_m1.png", sep =""), plot = p1_p0_combined_leg, width = 10, height = 6, dpi = 300)
