library(tidyverse)
library(ggplot2)

library(reshape2)
library(xtable)
library(cowplot)
library(here)

rm(list = ls())

# Load Functions ---------------------------------------------------------------
# set path for saving
PATH = here("Sample_size_sim/")
PATH_RESULTS = paste(PATH, "Results_method", sep = "")
PATH_PLOTS = paste(PATH, "Plots_combined_100_200/", sep = "")

source(paste(PATH, "plot_functions.R", sep=""))

setwd(PATH)

res_tab_1 = as.data.frame(read.table(paste(PATH_RESULTS,"_1_100_200/", "res_table_method_1.csv", sep = "")))
res_tab_2 = as.data.frame(read.table(paste(PATH_RESULTS,"_2_100_200/", "res_table_method_2.csv", sep = "")))

res_table_beta0_beta1_meth1 = rearrange_coverage_data(res_tab_1, 1)
res_table_beta0_beta1_meth2 = rearrange_coverage_data(res_tab_2, 2)
 
res_tab_combined = bind_rows(res_table_beta0_beta1_meth1, res_table_beta0_beta1_meth2)


####################################################################################################################
# Plote seperated for small and bigger n 
res_tab_combined_small_n = res_tab_combined %>% filter(n <= 800)
res_tab_combined_midrange_n = res_tab_combined %>% filter(n >=800) %>% filter(n<=10000) 
res_tab_combined_big_n = res_tab_combined %>% filter(n>=8000)

set_colours_beta1 = c("1" = "#0000CD", "2" = "#2F4F4F", "3" = "#00EEEE")
set_colours_beta0 = c( "1"= "#8B1A1A", "2" = "#FF1493", "3" = "#EE00EE")
set_colours_betadiff = c("1" = "#556B2F", "2" = "#00FF00", "3" = "#4EEE94")

# Beta_1 seperate plots -----------------------------------------------------------------------------------------------------------
# small
res_table_small_beta1 = res_tab_combined_small_n %>% filter(EST == "coverage_beta1")
p_small_n_b1 = plot_b_partrange_n_meth(res_table_small_beta1, 0.95, set_colours_beta1,res_table_small_beta1$n )
p_small_n_b1 = p_small_n_b1 + theme(legend.position = "none") + labs(y = expression(beta*1)) + ylim(0.94,0.96)

# midrange
res_table_midrange_beta1 = res_tab_combined_midrange_n %>% filter(EST == "coverage_beta1")
p_midrange_n_b1 = plot_b_partrange_n_meth(res_table_midrange_beta1, 0.95, set_colours_beta1, c(800,2000,3000,4000,5000,6000,7000,8000,9000,10000))
p_midrange_n_b1 = p_midrange_n_b1 + theme(legend.position = "none") + labs(y = expression(beta*1)) + ylim(0.94,0.96)

# big
res_table_big_beta1 = res_tab_combined_big_n %>% filter(EST == "coverage_beta1")
p_big_n_b1 = plot_b_partrange_n_meth(res_table_big_beta1, 0.95, set_colours_beta1, c(8000, 20000, 50000, 80000))
p_big_n_b1 = p_big_n_b1 + labs(y = expression(beta*1)) + ylim(0.94,0.96)

p_n_b1 = plot_grid(p_small_n_b1,p_midrange_n_b1, p_big_n_b1, ncol = 1 )

# Beta_0 seperate plots -----------------------------------------------------------------------------------------------------------
# small
res_table_small_beta0 = res_tab_combined_small_n %>% filter(EST == "coverage_beta0")
p_small_n_b0 = plot_b_partrange_n_meth(res_table_small_beta0, 0.95, set_colours_beta0,res_table_small_beta0$n )
p_small_n_b0 = p_small_n_b0 +  theme(legend.position = "none") + labs(y = expression(beta*0)) + ylim(0.94,0.96)

# midrange
res_table_midrange_beta0 = res_tab_combined_midrange_n %>% filter(EST == "coverage_beta0")
p_midrange_n_b0 = plot_b_partrange_n_meth(res_table_midrange_beta0, 0.95, set_colours_beta0, c(800,2000,3000,4000,5000,6000,7000,8000,9000,10000))
p_midrange_n_b0 =p_midrange_n_b0 +  theme(legend.position = "none") + labs(y = expression(beta*0)) + ylim(0.94,0.96)

# big
res_table_big_beta0 = res_tab_combined_big_n %>% filter(EST == "coverage_beta0")
p_big_n_b0 = plot_b_partrange_n_meth(res_table_big_beta0, 0.95, set_colours_beta0, c(8000, 20000, 50000, 80000))
p_big_n_b0 = p_big_n_b0 + labs(y = expression(beta*0)) + ylim(0.94,0.96)

p_n_b0 = plot_grid(p_small_n_b0,p_midrange_n_b0, p_big_n_b0, ncol = 1 )

# Beta_1-Beta_0 seperate plots -----------------------------------------------------------------------------------------------------------
# small
res_table_small_betadiff = res_tab_combined_small_n %>% filter(EST == "coverage_beta_diff")
p_small_n_bdiff = plot_b_partrange_n_meth(res_table_small_betadiff, 0.95, set_colours_betadiff,res_table_small_betadiff$n )
p_small_n_bdiff = p_small_n_bdiff +  theme(legend.position = "none") + labs(y = expression(beta*1*-beta*0)) + ylim(0.94,0.96)

# midrange
res_table_midrange_betadiff = res_tab_combined_midrange_n %>% filter(EST == "coverage_beta_diff")
p_midrange_n_bdiff = plot_b_partrange_n_meth(res_table_midrange_betadiff, 0.95, set_colours_betadiff, c(800,2000,3000,4000,5000,6000,7000,8000,9000,10000))
p_midrange_n_bdiff = p_midrange_n_bdiff +  theme(legend.position = "none") + labs(y = expression(beta*1*-beta*0)) + ylim(0.93,0.97)

# big
res_table_big_betadiff = res_tab_combined_big_n %>% filter(EST == "coverage_beta_diff")
p_big_n_bdiff = plot_b_partrange_n_meth(res_table_big_betadiff, 0.95, set_colours_betadiff, c(8000, 20000, 50000, 80000))
p_big_n_bdiff = p_big_n_bdiff+ labs(y = expression(beta*1*-beta*0)) + ylim(0.94,0.96)

p_n_bdiff = plot_grid(p_small_n_bdiff,p_midrange_n_bdiff, p_big_n_bdiff, ncol = 1 )

# save all the plots in one figure: 
p_all = plot_grid(p_n_b1, p_n_b0, p_n_bdiff, ncol =3)
ggsave(paste(PATH_PLOTS, "n_coverage_beta_all_meth.png", sep =""), plot = p_all, width = 15, height = 6, dpi = 300)

