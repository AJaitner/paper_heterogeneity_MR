library(tidyverse)
library(ggplot2)
library(here)
library(reshape2)
library(xtable)
library(cowplot)

rm(list = ls())

# Load Functions ---------------------------------------------------------------
# set path for saving
PATH = here("Assumptions_method_2")
PATH_RESULTS = paste0(PATH, "Results/")
setwd(PATH)

rgmte_est = as.data.frame(read.table(paste(RESULTS_PATH,"rgmte_est.csv", sep = "")))
beta_0_est = as.data.frame(read.table(paste(RESULTS_PATH,"beta_0_est.csv", sep = "")))
beta1 = rgmte_est + beta_0_est

sd_rgmte = as.data.frame(read.table(paste(RESULTS_PATH,"sd_rgmte.csv", sep = "")))
sd_beta_0 = as.data.frame(read.table(paste(RESULTS_PATH,"sd_beta_0.csv", sep = "")))

fstat_stage_1_MR_G2 = as.data.frame(read.table(paste(RESULTS_PATH,"fstat_stage_1_MR.csv", sep = "")))

parameter_matrix_scenario_2 = read.table(paste(RESULTS_PATH,"parameter_matrix_scenario_2.csv", sep = ""))
true_values_beta0 = parameter_matrix_scenario_2$beta.0 
true_values_beta1 = parameter_matrix_scenario_2$beta.1
true_values_beta1_beta0 = true_values_beta1 - true_values_beta0

N = dim(rgmte_est)[1]
num_scen = dim(rgmte_est)[2]

beta_0_est_mean = colMeans(beta_0_est)
beta_0_est_sd = colMeans(sd_beta_0)/sqrt(N)

rgmte_est_mean = colMeans(rgmte_est)
rgmte_est_sd = colMeans(sd_rgmte)/sqrt(N)


# save mean results with parameter values
res_table_latex = cbind(rep(20000,num_scen ), rep(N, num_scen), parameter_matrix_scenario_2, beta_0_est_mean, beta_0_est_sd, rgmte_est_mean, rgmte_est_sd, colMeans(fstat_stage_1_MR_G2))
colnames(res_table_latex)= c("n", "N", names(parameter_matrix_scenario_2),  "BETA_0_EST", "SD BETA_0_EST", "BETA1_BETA0_EST", "SD BETA1_BETA0_EST", "FSTAT_G2" )
res_table_latex = as.data.frame(res_table_latex)
res_table_latex

write.table(res_table_latex, file = paste(RESULTS_PATH, "res_table_method_2.csv", sep = ""),  row.names = FALSE)

colnames(beta_0_est) = c("1", "2", "3", "4", "5", "6", "7", "8")

set_colours = c("#e78ac3","#66c2a5","#fc8d62", "#8da0cb")
# one plot
# Beta_0
beta_0_matrix_scen1_melt <- melt(beta_0_est[,1:4]) 

p0_all_1 <- ggplot(aes(x=value, colour=variable), data=beta_0_matrix_scen1_melt) +
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta0, linetype='dashed') + 
            ylim(0,0.008) + xlim(-300,100) + labs(x = expression(beta*0), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

beta_0_matrix_scen1_melt <- melt(beta_0_est[,5:8]) 
p0_all_2 <- ggplot(aes(x=value, colour=variable), data=beta_0_matrix_scen1_melt) +
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta0, linetype='dashed') +
            ylim(0,0.008) + xlim(-300,100) + labs(x = expression(beta*0), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

p0_all = plot_grid(p0_all_1,p0_all_2, ncol = 1)

ggsave(paste(PATH, "Plots/beta_0_density_all_settings_m2.png", sep =""), plot = p0_all)


colnames(rgmte_est) = c("1", "2", "3", "4", "5", "6", "7", "8")
# Beta_1 - Beta_0
beta1_beta0_melt <- melt(rgmte_est[,1:4]) 

p1_p0_all_1 <- ggplot(aes(x=value, colour=variable), data=beta1_beta0_melt) + 
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta1_beta0, linetype='dashed') +
            ylim(0,0.03) + xlim(-150,-50) + labs(x = expression(beta*1-beta*0), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)

beta1_beta0_melt <- melt(rgmte_est[,5:8]) 

p1_p0_all_2 <- ggplot(aes(x=value, colour=variable), data=beta1_beta0_melt) + 
            geom_density(show.legend=FALSE) + stat_density(geom="line",position="identity", size =1)+
            geom_vline(xintercept=true_values_beta1_beta0, linetype='dashed') +
            ylim(0,0.03) + xlim(-150,-50) + labs(x = expression(beta*1-beta*0), colour="Scenario") + theme_bw() + 
            scale_colour_manual(values = set_colours)


p1_p0_all = plot_grid(p1_p0_all_1,p1_p0_all_2, ncol = 1)

ggsave(paste(PATH, "Plots/beta1_beta0_density_all_settings_m2.png", sep =""), plot = p1_p0_all)


p0_all_no_legend = plot_grid(p0_all_1  + theme(legend.position = "none"),p0_all_2 + theme(legend.position = "none") ,ncol = 1)

p1_p0_combined_leg = plot_grid(p0_all_no_legend, p1_p0_all, rel_widths = c(1, 1.1))
ggsave(paste(PATH, "Plots/beta_0_beta_diff_combined_density_all_settings_m2.png", sep =""), plot = p1_p0_combined_leg, width = 10, height = 6, dpi = 300)

































































































































































