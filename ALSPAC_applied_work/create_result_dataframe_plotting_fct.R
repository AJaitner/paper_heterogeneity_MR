create_results_dataframe_plotting = function(method_1_res, method_2_res, smoking_var_char){

    print_results_dataframe = data.frame(Est_name = c("Beta_1", "Beta_0", "Beta_1-Beta_0", "Beta_1", "Beta_0", "Beta_1-Beta_0"), 
                                        Method = c("Method_1","Method_1","Method_1","Method_2", "Method_2", "Method_2"), 
                                        Est_value = rep(NA, 6), 
                                        SE = rep(NA, 6), 
                                        Lower_CI = rep(NA,6), 
                                        Higher_CI = rep(NA, 6),
                                        Pval = rep(NA,6), 
                                        n = rep(NA, 6), 
                                        Smoking_var = rep(smoking_var_char,6))
    # Add Est values
    method_1_betas = c(method_1_res[[4]]$coef["S_hat_G",1], method_1_res[[4]]$coef["S_hat_1_G",1], (method_1_res[[4]]$coef["S_hat_G",1] -method_1_res[[4]]$coef["S_hat_1_G",1]))
    method_2_betas = c((method_2_res[[3]]$coef["S:G",1] + method_2_res[[5]]$coef["S_hat",1]) , method_2_res[[5]]$coef["S_hat",1],method_2_res[[3]]$coef["S:G",1])

     # calculate the standard error for the beta_1 - beta_0
    cov_mat = vcov(method_1_res[[6]])
    var_beta1_beta0 = cov_mat["S_hat_G",2] + cov_mat["S_hat_1_G",3] -2*cov_mat["S_hat_G",3]
    sd_beta1_beta0 = sqrt(var_beta1_beta0)


    # SE: 
    method_1_se = c(method_1_res[[4]]$coef["S_hat_G",2], method_1_res[[4]]$coef["S_hat_1_G",2],sd_beta1_beta0)
    sd_beta_1_meth2 = sqrt(method_2_res[[3]]$coef["S:G",2]**2 + method_2_res[[5]]$coef["S_hat",2]**2)
    method_2_se = c(sd_beta_1_meth2, method_2_res[[5]]$coef["S_hat",2],method_2_res[[3]]$coef["S:G",2])

    # CI
    method_1_lower_CI = method_1_betas-1.96*method_1_se
    method_1_higher_CI = method_1_betas+1.96*method_1_se

    method_2_lower_CI = method_2_betas-1.96*method_2_se
    method_2_higher_CI = method_2_betas+1.96*method_2_se

    # Pval 
    method_1_pval = c(method_1_res[[4]]$coef["S_hat_G",4], method_1_res[[4]]$coef["S_hat_1_G",4], NA)
    method_2_pval = c(NA, method_2_res[[5]]$coef["S_hat",4],method_2_res[[3]]$coef["S:G",4])

    # n 
    method_1_n = rep(method_1_res[[7]],3)
    method_2_n = rep(method_2_res[[11]],3)

    # E_G_S1 
    method_1_E_G_S1 = rep(method_1_res[[8]],3)
    method_2_E_G_S1 = rep(method_2_res[[12]],3)

    print_results_dataframe$Est_value = c(method_1_betas, method_2_betas)
    print_results_dataframe$SE = c(method_1_se, method_2_se)
    print_results_dataframe$Lower_CI = c(method_1_lower_CI, method_2_lower_CI)
    print_results_dataframe$Higher_CI = c(method_1_higher_CI, method_2_higher_CI)
    print_results_dataframe$Pval = c(method_1_pval, method_2_pval)
    print_results_dataframe$n = c(method_1_n, method_2_n)
    print_results_dataframe$E_G_S1 = c(method_1_E_G_S1, method_2_E_G_S1)
    # Stage 1 ----------------------------------------------------------------------------------------
    print_stage1_dataframe = data.frame(Method = c("Method_1","Method_1","Method_2", "Method_2"),
                                        Inst = c("G", "G2", "G", "G2"), 
                                        Est_value = rep(NA, 4), 
                                        SE = rep(NA, 4), 
                                        Lower_CI = rep(NA,4), 
                                        Higher_CI = rep(NA, 4),
                                        Pval = rep(NA,4), 
                                        Fstat = rep(NA, 4), 
                                        Smoking_var = rep(smoking_var_char,4))

    # Est_value
    print_stage1_dataframe$Est_value =  c(method_1_res[[3]]$coef["G",1], method_1_res[[3]]$coef["G2",1], NA, method_2_res[[4]]$coef["G2",1])
    
    # SE
    print_stage1_dataframe$SE =  c(method_1_res[[3]]$coef["G",2], method_1_res[[3]]$coef["G2",2], NA, method_2_res[[4]]$coef["G2",2])
    
    print_stage1_dataframe$Lower_CI = print_stage1_dataframe$Est_value -1.96*print_stage1_dataframe$SE
    print_stage1_dataframe$Higher_CI = print_stage1_dataframe$Est_value +1.96*print_stage1_dataframe$SE
    # Pval
    print_stage1_dataframe$Pval =  c(method_1_res[[3]]$coef["G",4], method_1_res[[3]]$coef["G2",4], NA, method_2_res[[4]]$coef["G2",4])
    
    
    # Fstat G2
    method_1_g2_fstat = method_1_res[[3]]$coef["G2",1]**2/method_1_res[[3]]$coef["G2",2]**2
    method_1_g_fstat = method_1_res[[3]]$coef["G",1]**2/method_1_res[[3]]$coef["G",2]**2
    method_2_g2_fstat = method_2_res[[4]]$coef["G2",1]**2/method_2_res[[4]]$coef["G2",2]**2
    print_stage1_dataframe$Fstat = c(method_1_g_fstat, method_1_g2_fstat, NA, method_2_g2_fstat)
 
    return(list(print_stage1_dataframe, print_results_dataframe))

}