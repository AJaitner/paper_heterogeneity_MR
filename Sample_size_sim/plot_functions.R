rearrange_data = function(res_table_full, method){
    res_table_beta0 = res_table_full %>% 
        select("n", "BETA_0_EST", "LCI_BETA_0", "HCI_BETA_0") %>% 
        add_column(EST = "Beta_0") %>%
        add_column(METH = method) %>%
        dplyr::rename(c(EST_VAL = "BETA_0_EST", LCI = "LCI_BETA_0", HCI = "HCI_BETA_0"))
    res_table_beta1 = res_table_full %>% 
        select("n", "BETA_1_EST", "LCI_BETA_1", "HCI_BETA_1") %>% 
        add_column(EST = "Beta_1") %>%
        add_column(METH = method) %>%
        dplyr::rename(EST_VAL = "BETA_1_EST", LCI = "LCI_BETA_1", HCI = "HCI_BETA_1")
    res_table_beta1beta0 = res_table_full %>% 
        select("n", "BETA1_BETA0_EST", "LCI_BETA1_BETA0", "HCI_BETA1_BETA0") %>% 
        add_column(EST = "Beta1_Beta0") %>%
        add_column(METH = method) %>%
        dplyr::rename(EST_VAL = "BETA1_BETA0_EST", LCI = "LCI_BETA1_BETA0", HCI = "HCI_BETA1_BETA0")

    res_table_beta0_beta1 = rbind(res_table_beta0,res_table_beta1, res_table_beta1beta0)
    return(res_table_beta0_beta1)
}

rearrange_coverage_data = function(res_table_full, method){
    res_table_beta0 = res_table_full %>% 
        select("n", "coverage_beta0", "coverage_beta0_lower", "coverage_beta0_higher") %>% 
        add_column(EST = "coverage_beta0") %>%
        add_column(METH = method) %>%
        dplyr::rename(c(EST_VAL = "coverage_beta0", LCI = "coverage_beta0_lower", HCI = "coverage_beta0_higher"))
    res_table_beta1 = res_table_full %>% 
        select("n", "coverage_beta1", "coverage_beta1_lower", "coverage_beta1_higher") %>% 
        add_column(EST = "coverage_beta1") %>%
        add_column(METH = method) %>%
        dplyr::rename(c(EST_VAL = "coverage_beta1", LCI = "coverage_beta1_lower", HCI = "coverage_beta1_higher") )
    res_table_beta1beta0 = res_table_full %>% 
        select("n", "coverage_beta_diff", "coverage_beta_diff_lower", "coverage_beta_diff_higher") %>% 
        add_column(EST = "coverage_beta_diff") %>%
        add_column(METH = method) %>%
        dplyr::rename(c(EST_VAL = "coverage_beta_diff", LCI = "coverage_beta_diff_lower", HCI = "coverage_beta_diff_higher"))

    res_table_beta0_beta1 = rbind(res_table_beta0,res_table_beta1, res_table_beta1beta0)
    return(res_table_beta0_beta1)
}

rearrange_power_data = function(res_table_full, method){
    res_table_beta0 = res_table_full %>% 
        select("n", "power_beta_0", "power_beta_0_LCI", "power_beta_0_HCI") %>% 
        add_column(EST = "power_beta_0") %>%
        add_column(METH = method) %>%
        dplyr::rename(c(EST_VAL = "power_beta_0", LCI = "power_beta_0_LCI", HCI = "power_beta_0_HCI"))
    res_table_beta1 = res_table_full %>% 
        select("n", "power_beta_1", "power_beta_1_LCI", "power_beta_1_HCI") %>% 
        add_column(EST = "power_beta_1") %>%
        add_column(METH = method) %>%
        dplyr::rename(c(EST_VAL = "power_beta_1", LCI = "power_beta_1_LCI", HCI = "power_beta_1_HCI")) 
    res_table_beta1beta0 = res_table_full %>% 
        select("n", "power_rgmte", "power_rgmte_LCI", "power_rgmte_HCI") %>% 
        add_column(EST = "power_rgmte") %>%
        add_column(METH = method) %>%
        dplyr::rename(c(EST_VAL = "power_rgmte", LCI = "power_rgmte_LCI", HCI = "power_rgmte_HCI"))

    res_table_beta0_beta1 = rbind(res_table_beta0,res_table_beta1, res_table_beta1beta0)
    return(res_table_beta0_beta1)
}

plot_b1_b0_fullrange_noci = function(data, true_values_beta1, true_values_beta0, set_colours,break_num ){
    p = ggplot(data, aes(x=n, y =EST_VAL, group= factor(EST))) + 
        geom_point(aes(colour= factor(EST))) +
        geom_line(aes(colour= factor(EST))) + 
        #geom_ribbon(aes(ymin=LCI, ymax=HCI, fill=factor(EST)), alpha=0.2) +
        geom_hline(yintercept=true_values_beta1, linetype='dashed') +
        geom_hline(yintercept=true_values_beta0, linetype='dashed') +
        scale_x_continuous(breaks = break_num ) + 
        theme_bw() + 
        scale_fill_manual(values=set_colours) + 
        scale_colour_manual(values = set_colours)
    return(p)

}

plot_b1_b0_partrange_n = function(data, true_values_beta1, true_values_beta0, set_colours, break_num ){
    p_n = ggplot(data, aes(x=n, y =EST_VAL, group= factor(EST))) + 
        geom_point(aes(colour= factor(EST))) +
        geom_line(aes(colour= factor(EST))) + 
        geom_ribbon(aes(ymin=LCI, ymax=HCI, fill=factor(EST)), alpha=0.2) +
        geom_hline(yintercept=true_values_beta1, linetype='dashed') +
        geom_hline(yintercept=true_values_beta0, linetype='dashed') +
        scale_x_continuous(breaks = break_num) + 
        theme_bw() + 
        scale_fill_manual(name = "Method", values=set_colours) + 
        scale_colour_manual(name = "Method", values = set_colours)
    return(p_n)
}

plot_b_partrange_n_meth = function(data, true_value, set_colours, break_num ){
    p_n = ggplot(data, aes(x=n, y =EST_VAL, group= factor(METH))) + 
        geom_point(aes(colour= factor(METH))) +
        geom_line(aes(colour= factor(METH))) + 
        geom_ribbon(aes(ymin=LCI, ymax=HCI, fill=factor(METH)), alpha=0.2) +
        geom_hline(yintercept=true_value, linetype='dashed') +
        scale_x_continuous(breaks = break_num) + 
        theme_bw() + 
        scale_fill_manual(name = "Method", values=set_colours) + 
        scale_colour_manual(name = "Method", values = set_colours)
    return(p_n)
}

# plot_cov_partrange_n_meth = function(data, EST_VAL, LCI, HCI, true_value, set_colours, break_num ){
#     p_n = ggplot(data, aes(x=n, y =EST_VAL, group= factor(METH))) + 
#         geom_point(aes(colour= factor(METH))) +
#         geom_line(aes(colour= factor(METH))) + 
#         geom_ribbon(aes(ymin=LCI, ymax=HCI, fill=factor(METH)), alpha=0.2) +
#         geom_hline(yintercept=true_value, linetype='dashed') +
#         scale_x_continuous(breaks = break_num) + 
#         theme_classic() + 
#         scale_fill_manual(name = "Method", values=set_colours) + 
#         scale_colour_manual(name = "Method", values = set_colours)
#     return(p_n)
# }