  ## Load required packages
  pacman::p_load(dplyr, tidyr, readxl, janitor, psych, readr, googlesheets4, metafor, netmeta)
  options(max.print = 1000000)
  
  ## Prepare component binaries for cNMA
  cNMA_data_4.1_icW_test <- cNMA_data_4.1_icW
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_test %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim, measure_name, domain, effect_size, variance, intervention_n, comparison_n)
  tabyl(cNMA_data_4.1_icW_short$intervention_prelim)
  tabyl(cNMA_data_4.1_icW_short$comparison_prelim)
  
  cNMA_data_4.1_icW_short$FF <- 0
  cNMA_data_4.1_icW_short$RS <- 0
  cNMA_data_4.1_icW_short$NL <- 0
  cNMA_data_4.1_icW_short$TES <- 0
  cNMA_data_4.1_icW_short$VF <- 0
  cNMA_data_4.1_icW_short$BAU <- 0
  
  tabyl(cNMA_data_4.1_icW_short$FF)
  tabyl(cNMA_data_4.1_icW_short$RS)
  tabyl(cNMA_data_4.1_icW_short$NL)
  tabyl(cNMA_data_4.1_icW_short$TES)  
  tabyl(cNMA_data_4.1_icW_short$VF)
  tabyl(cNMA_data_4.1_icW_short$BAU)
   
  tabyl(cNMA_data_4.1_icW_short$FF)
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(FF = ifelse(intervention_prelim=="F",1, FF))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(FF = ifelse(intervention_prelim=="NL+TES+FF+RS",1, FF))
  tabyl(cNMA_data_4.1_icW_short$FF)
  
  tabyl(cNMA_data_4.1_icW_short$RS)
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+FF+RS",1, RS))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+VF+RS",1, RS))  
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(RS = ifelse(intervention_prelim=="TES+RS",1, RS))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(RS = ifelse(intervention_prelim=="VF+RS",1, RS))   
  tabyl(cNMA_data_4.1_icW_short$RS)
  
  tabyl(cNMA_data_4.1_icW_short$NL)
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+FF+RS",1, NL))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+VF+RS",1, NL))  
  tabyl(cNMA_data_4.1_icW_short$NL)  

  tabyl(cNMA_data_4.1_icW_short$TES)
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+FF+RS",1, TES))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+VF+RS",1, TES))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(TES = ifelse(intervention_prelim=="TES+RS",1, TES))
  tabyl(cNMA_data_4.1_icW_short$TES)  

  tabyl(cNMA_data_4.1_icW_short$VF)
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(VF = ifelse(intervention_prelim=="NL+TES+VF+RS",1, VF))
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(VF = ifelse(intervention_prelim=="VF+RS",1, VF))
  tabyl(cNMA_data_4.1_icW_short$VF)  

  tabyl(cNMA_data_4.1_icW_short$BAU)
  cNMA_data_4.1_icW_short <- cNMA_data_4.1_icW_short %>% mutate(BAU = ifelse(comparison_prelim=="BAU",-1, BAU))
  tabyl(cNMA_data_4.1_icW_short$BAU) 

  str(cNMA_data_4.1_icW_short)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies 
  # There are the following dependencis within these data: i) multiple contrasts within a study (cluster=study_id); ii) multiple outcome measures within a domain (obs=measure); and iii) multiple outcome domains within a contrast (type=domain).
  # V_es_anxiety <- vcalc(vi = via, cluster = refid_int, obs = measure, grp1 = int, grp2 = comp, w1 = n_int, w2 = n_comp, time1 = timing,
  #                       rho = 0.8, phi = 0.8, data = dat_anxiety)
  V_list <- vcalc(variance, cluster= study_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=intervention_prelim, grp2=comparison_prelim, w1=intervention_n, w2=comparison_n, data=cNMA_data_4.1_icW_short)
  
  
  ## Fit 1-step additive CNMA
  # List all individual intervention components as mods, except for the comparator of interest (in this example, it is the no treatment condition, BAU)
  # For random, specify intervention (contrast) is nested within study
  mod_addCNMA_icW<- rma.mv(effect_size, V_list, 
                                     mods = ~ 0 + FF + RS + NL + TES + VF,
                                     random = ~ 1 | study_id/es_id, 
                                     rho=0.60, 
                                     data=cNMA_data_4.1_icW_short)
  summary(mod_addCNMA_icW)
  
  res_mod_icW_cNMA <- rma.mv(effect_size, V_list, 
                             mods = ~ FF + RS + NL + TES + VF - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons
                             random = ~ 1 | study_id/es_id, 
                             rho=0.60, 
                             data=cNMA_data_4.1_icW_short)
  summary(res_mod_icW_cNMA) 
  
  ## Generate estimate and CI for any combination of components 
  # The number of values corresponds to the number of components specified in mods (so, still leave off comparator of interest). 
  # Change values to 1 for components to include in estimate (eg, changing the first three values to 1 would give the estimate and CI for the act + cog + emo combination)
  predict(mod_addCNMA_icW, newmods = c(0, #FF
                                       0, #RS
                                       0, #NL
                                       0, #TES
                                       0), #VF
          digits = 3)
  
  
  ## Calculate Multiple I2
  # Insert V and mod objects, then run the last four lines
  M <- mod_addCNMA_icW
  V <- V_list
  
  W <- solve(V)
  X <- model.matrix(M)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  round(100 * M$tau2 / (M$tau2 + (M$k - M$p) / sum(diag(P))), 1)
  
  
  
  
  