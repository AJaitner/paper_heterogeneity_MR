data_gen_alspac_twoG = function(n,parameter_matrix, scenario){

  # if(missing(par.Y0)){
  #   par.Y0 = 1
  # }
  
  par.U0 = parameter_matrix$gamma.U0
  par.UG = parameter_matrix$gamma.UG
  par.S0 = parameter_matrix$gamma.S0
  par.SG = parameter_matrix$gamma.SG
  par.SG2 = parameter_matrix$gamma.SG2
  par.SU = parameter_matrix$gamma.SU
  par.Y0 = parameter_matrix$gamma.Y0
  par.YG = parameter_matrix$gamma.YG
  par.YU = parameter_matrix$gamma.YU
  par.YG2 = parameter_matrix$gamma.YG2
  par.UG2 = parameter_matrix$gamma.UG2
  
  beta.1 = parameter_matrix$beta.1
  beta.0 = parameter_matrix$beta.0
  
  # Genetic variant
  G = rbinom(n,1,0.55) # 3297 out of 7359 with G = 0 in ALSPAC (1: 3231, 2: 831)
  G2 = rbinom(n,1,0.4) # SNP for smoking initiation
  
  # Generate U
  e.U = rnorm(n,0,1)
  U = par.U0[scenario]+ par.UG[scenario] * G + par.UG2[scenario] * G2 + e.U
  
  # Generate smoking during pregnancy (not taking smoking before pregnancy into account)
  e.S = rnorm(n,0,0.5)
  eta = par.S0[scenario] + par.SG[scenario] *G + par.SG2[scenario] *G2 + par.SU[scenario] * U + e.S
  p.S = exp(eta)/(1 + exp(eta))
  S = rbinom(n,1,p.S)

  e.Y = rnorm(n,0,470)
  Y = par.Y0[scenario]  + beta.1[scenario] *S * G + beta.0[scenario] * S *(1-G) + par.YG[scenario] * G + par.YG2[scenario] + par.YU[scenario]*U + e.Y
  
  paramters = c(n,parameter_matrix[scenario,])
  
  data = as.data.frame(cbind(G, G2, U, S, Y, eta, p.S, e.U, e.S, e.Y))
  
  return(list(data,paramters))
  
  }