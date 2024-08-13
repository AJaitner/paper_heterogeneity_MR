method_1_fct = function(Y, S, G, G2, Z_exp, Z_out, data){

    ## check inputs
	if (class(Y) != "character")  stop("Outcome Y needs to be a variable name i.e. a string (class `character`)")
	if (class(S) != "character")  stop("Treatment T needs to be a variable name i.e. a string (class `character`)")
	if (class(G) != "character")  stop("Genotype G needs to be a variable name i.e. a string (class `character`)")
	if (class(G2) != "character")  stop("Genotype G needs to be a variable name i.e. a string (class `character`)")	
    if (class(Z_exp) != "character")  stop("Covariates Z needs to be a formula i.e. a string (class `character`) of variable(s) in data.frame D e.g. \"age+sex\"")
    if (class(Z_out) != "character")  stop("Covariates Z needs to be a formula i.e. a string (class `character`) of variable(s) in data.frame D e.g. \"age+sex\"")

	## create variables named Y, T and G for formulas, and compute T* (interaction between T and G)
	data[,"Y"]=data[,Y]
	data[,"S"]=data[,S]
	data[,"G"]=data[,G]
    data[,"G2"]=data[,G2]

    Zs_exp=strsplit(Z_exp,"[+]|[*]")[[1]]
    Zs_out=strsplit(Z_out,"[+]|[*]")[[1]]

    ## subset dataset to columns specified and remove NAs
	data=data[,colnames(data) %in% c("Y","S","G","G2", Zs_exp, Zs_out)]
	data=as.data.frame(na.omit(data))
    n = dim(data)[1]
    # Stage 1: predict S with G and G2
    # Use G and G2 for estimation of SD
    glm_s_g2_g= glm(as.formula(paste0("S ~ G2 + G +", Z_exp)), family="binomial", data = data)
    data$S_hat = glm_s_g2_g$fitted.values 
    glm_s_g2_g_sum = summary(glm_s_g2_g)

    # Stage 2: use genetically prediced smoking with linear interaction model to estimate beta_0 and beta_1 
    data$S_hat_G = data$S_hat * data$G
    data$S_hat_1_G = data$S_hat*(1-data$G)
    lm_Y_sen1 = lm(as.formula(paste0("Y~S_hat_G + S_hat_1_G + ", Z_out)), data = data)
    lm_Y_sen1_sum = summary(lm_Y_sen1)


    beta_1 = lm_Y_sen1_sum$coef["S_hat_G",1]
    beta_0 = lm_Y_sen1_sum$coef["S_hat_1_G",1]

    E_G_S1 = mean(data$G[data$S ==1], na.rm = TRUE)
    
    result_list = list(beta_1, beta_0,glm_s_g2_g_sum, lm_Y_sen1_sum, glm_s_g2_g, lm_Y_sen1,n, E_G_S1)

    return(result_list)

}


method_2_fct = function(Y, S, G, G2, Z_exp, Z_out, data){

    ## check inputs
	if (class(Y) != "character")  stop("Outcome Y needs to be a variable name i.e. a string (class `character`)")
	if (class(S) != "character")  stop("Treatment T needs to be a variable name i.e. a string (class `character`)")
	if (class(G) != "character")  stop("Genotype G needs to be a variable name i.e. a string (class `character`)")
	if (class(G2) != "character")  stop("Genotype G needs to be a variable name i.e. a string (class `character`)")	
    if (class(Z_exp) != "character")  stop("Covariates Z needs to be a formula i.e. a string (class `character`) of variable(s) in data.frame D e.g. \"age+sex\"")
    if (class(Z_out) != "character")  stop("Covariates Z needs to be a formula i.e. a string (class `character`) of variable(s) in data.frame D e.g. \"age+sex\"")


    ## create variables named Y, T and G for formulas, and compute T* (interaction between T and G)
	data[,"Y"]=data[,Y]
	data[,"S"]=data[,S]
	data[,"G"]=data[,G]
    data[,"G2"]=data[,G2]

    Zs_exp=strsplit(Z_exp,"[+]|[*]")[[1]]
    Zs_out=strsplit(Z_out,"[+]|[*]")[[1]]

    ## subset dataset to columns specified and remove NAs
	data=data[,colnames(data) %in% c("Y","S","G","G2", Zs_exp, Zs_out)]
	data=as.data.frame(na.omit(data))
    n = dim(data)[1]
  # Estimation of GMTE using RGMTE method from Jacks code
    rgmte_est_lm = lm(as.formula(paste0("Y~S*G + ", Z_out)), data = data)
    rgmte_est_lm_sum = summary(rgmte_est_lm)
    GMTE = rgmte_est_lm_sum$coef["S:G",1]

    # save beta_RGTME for G2
    rgmte_est_G2_lm = lm(as.formula(paste0("G2~S*G + ", Z_exp)), data = data)
    rgmte_est_G2_lm_sum = summary(rgmte_est_G2_lm)
        
    data$Y_new = data$Y-GMTE*(data$S*data$G)
    
    # Estimation of S_hat
    glm_s_g2 = glm(as.formula(paste0("S~G2 + ", Z_exp)), family ="binomial", data = data)
    glm_s_g2_sum = summary(glm_s_g2)
    data$S_hat = glm_s_g2$fitted.values

    # Estimation of beta_0
    lm_ynew_shat = lm(as.formula(paste0("Y_new ~ S_hat + ", Z_out)), data = data)
    lm_ynew_shat_sum = summary(lm_ynew_shat)

    beta_0 = lm_ynew_shat_sum$coefficients["S_hat",1]
    E_G_S1 = mean(data$G[data$S ==1], na.rm = TRUE)
    
    result_list = list(GMTE, beta_0, rgmte_est_lm_sum,glm_s_g2_sum,lm_ynew_shat_sum, rgmte_est_G2_lm_sum, rgmte_est_lm, glm_s_g2, lm_ynew_shat, rgmte_est_G2_lm, n, E_G_S1 )

    return(result_list)

}

standard_MR = function(Y, S, I, Z_exp, Z_out, data){

    ## check inputs
	if (class(Y) != "character")  stop("Outcome Y needs to be a variable name i.e. a string (class `character`)")
	if (class(S) != "character")  stop("Treatment T needs to be a variable name i.e. a string (class `character`)")
	if (class(I) != "character")  stop("Genotype G needs to be a variable name i.e. a string (class `character`)")
    if (class(Z_exp) != "character")  stop("Covariates Z needs to be a formula i.e. a string (class `character`) of variable(s) in data.frame D e.g. \"age+sex\"")
    if (class(Z_out) != "character")  stop("Covariates Z needs to be a formula i.e. a string (class `character`) of variable(s) in data.frame D e.g. \"age+sex\"")

	## create variables named Y, T and G for formulas, and compute T* (interaction between T and G)
	data[,"Y"]=data[,Y]
	data[,"S"]=data[,S]
	data[,"I"]=data[,I]

    Zs_exp=strsplit(Z_exp,"[+]|[*]")[[1]]
    Zs_out=strsplit(Z_out,"[+]|[*]")[[1]]

    ## subset dataset to columns specified and remove NAs
	data=data[,colnames(data) %in% c("Y","S","I", Zs_exp, Zs_out)]
	data=as.data.frame(na.omit(data))
    n = dim(data)[1]
    # Stage 1: predict S with G and G2
    # Use G and G2 for estimation of SD
    step_1= glm(as.formula(paste0("S ~ I +", Z_exp)), family="binomial", data = data)
    data$S_hat = step_1$fitted.values 
    step_1_sum = summary(step_1)

    # Stage 2: use genetically prediced smoking with linear interaction model to estimate beta_0 and beta_1 
    step_2 = lm(as.formula(paste0("Y~S_hat +", Z_out)), data = data)
    step_2_sum = summary(step_2)
    
    beta = step_2_sum$coef["S_hat",1]

    result_list = list(beta, step_1, step_2, n)

    return(result_list)

}