library(ggplot2)
library(tidyverse)
library(here)

rm(list = ls())

PATH = here("sample_size_sim", "Low_effect_diff/")
PATH_PLOTS = paste(PATH, "Results_low_effect/", sep = "")

meth_1_res = read.table(paste(PATH, "Results_low_effect/sample_size_low_effect_meth1.csv", sep = ""), sep = ",", header = TRUE)
meth_1_res_df = as.data.frame(meth_1_res)
meth_1_res_df$method = rep(1, dim(meth_1_res_df)[1])

p = ggplot(meth_1_res_df, aes(x=n, y =power_rgmte, group= factor(True_beta_diff))) + 
        geom_point(aes(colour= factor(True_beta_diff))) +
        geom_line(aes(colour= factor(True_beta_diff))) +  
        geom_ribbon(aes(ymin=power_rgmte_LCI, ymax=power_rgmte_HCI, fill=factor(True_beta_diff)), alpha=0.2, show.legend=FALSE)+
        geom_hline(yintercept=80, linetype='dashed') + 
        labs(y = "Power", colour=expression(beta*1-beta*0)) +
        scale_y_continuous(limits = c(0,100), breaks= c(0,20,40,60,80,100)) +
        scale_x_continuous(labels = scales::comma)+
        theme_bw() #+ 
        #scale_fill_manual(values=set_colours) + 
        #scale_colour_manual(values = set_colours)

ggsave(paste(PATH_PLOTS, "meth_1_power.png", sep =""), plot = p)


meth_2_res = read.table(paste(PATH, "Results_low_effect/sample_size_low_effect_meth2.csv", sep = ""), sep = " ", header = TRUE)
meth_2_res_df = as.data.frame(meth_2_res)
meth_2_res_df$method = rep(2, dim(meth_2_res_df)[1])

p = ggplot(meth_2_res_df, aes(x=n, y =power_rgmte, group= factor(True_beta_diff))) + 
        geom_point(aes(colour= factor(True_beta_diff))) +
        geom_line(aes(colour= factor(True_beta_diff))) + 
        geom_ribbon(aes(ymin=power_rgmte_LCI, ymax=power_rgmte_HCI, fill=factor(True_beta_diff)), alpha=0.2, show.legend=FALSE)+
        geom_hline(yintercept=80, linetype='dashed') + 
        labs(y = "Power", colour=expression(beta*1-beta*0)) + 
        scale_y_continuous(limits = c(0,100), breaks= c(0,20,40,60,80,100)) +
        scale_x_continuous(labels = scales::comma)+
        theme_bw() #+ 
        #scale_fill_manual(values=set_colours) + 
        #scale_colour_manual(values = set_colours)

ggsave(paste(PATH_PLOTS, "meth_2_power.png", sep =""), plot = p)

# Combine plots in one
variable_selection = c("n", "True_beta_diff", "power_rgmte", "power_rgmte_LCI", "power_rgmte_HCI", "method")
meth_1_2_res_df = rbind(meth_1_res_df[,variable_selection], meth_2_res_df[,variable_selection])
meth_1_2_res_df = meth_1_2_res_df %>% filter(n != 10000)

p = ggplot(meth_1_2_res_df, aes(x=n, y =power_rgmte, colour= factor(True_beta_diff), shape = factor(method))) + 
        geom_point(aes(colour= factor(True_beta_diff)), size = 2) +
        geom_line(aes(colour= factor(True_beta_diff))) + 
        geom_ribbon(aes(ymin=power_rgmte_LCI, ymax=power_rgmte_HCI, fill=factor(True_beta_diff)), colour = NA, alpha=0.2, show.legend=FALSE)+
        geom_hline(yintercept=80, linetype='dashed') + 
        labs(y = "Power", colour=expression(beta*1-beta*0), shape = "Method") + 
        scale_y_continuous(limits = c(0,100), breaks= c(0,20,40,60,80,100)) +
        scale_x_continuous(limits = c(0,500000),breaks = meth_1_2_res_df[1:7,1],  labels = scales::comma) + # breaks= c(7000, 50000,100000, 200000, 300000, 500000),
        #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
        theme_bw() #+ 
        #scale_fill_manual(values=set_colours) + 
        #scale_colour_manual(values = set_colours)

ggsave(paste(PATH_PLOTS, "meth_1_2_power.png", sep =""), plot = p)
