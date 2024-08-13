read_res_MR = function(model_1, model_2, exposure, outcome, genetic_instrument, confounder_char){
  
    res_help_1 = summary(model_1)$coef[2,] # results stage 1
    res_help_2 = summary(model_2)$coef[2,] # results stage 2
    f_stat = summary(model_1)$coef[2,1]**2/summary(model_1)$coef[2,2]**2
    n_stage1 = nobs(model_1)
    n_stage2 = nobs(model_2)
    res = c(genetic_instrument, exposure, outcome, f_stat, res_help_1,n_stage1, res_help_2,n_stage2, confounder_char)

    return(res)
}