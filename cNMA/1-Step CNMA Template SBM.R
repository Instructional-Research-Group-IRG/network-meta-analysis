# Load required packages
  pacman::p_load(dplyr, tidyr, readxl, janitor, psych, readr, googlesheets4, metafor, netmeta)
  options(max.print = 1000000)
  
# Load final CNMA dataset with effect sizes and standard errors converted to 4.1
  cNMA_data_4.1 <- read_csv("cNMA_data_4.1.csv")
  cNMA_data_4.1 <- cNMA_data_4.1 %>% mutate(variance_final = ((standard_error_final^2) * (intervention_n + comparison_n))) # Calculate variance from standard error and sample size
  describe (cNMA_data_4.1$standard_error_final)
  describe (cNMA_data_4.1$variance_final)
  
# Merge intervention_prelim & comparison_prelim from NNMA database to cNMA_data_4.1 until intervention and comparison columns are added to the CNMA database 
  
  ## Load in observations used for NMA
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set
  NMA_data_analysis_subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") & aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") & (TvsT==1 | TvsT==0))
  
  ## Modify data to match CNMA for merging
  NMA_data_analysis_subset <- NMA_data_analysis_subset %>% rename(study_id=record_id)  
  class(NMA_data_analysis_subset$contrast_id)
  NMA_data_analysis_subset <- NMA_data_analysis_subset %>% mutate(contrast_id = as.character(contrast_id))
  class(NMA_data_analysis_subset$contrast_id)
  tabyl(NMA_data_analysis_subset$contrast_id)
  NMA_data_analysis_subset$contrast_id <- gsub("_disagg", "", NMA_data_analysis_subset$contrast_id) # "_disagg" removed from contrast IDs in the CNMA Master Database
  NMA_data_analysis_subset$contrast_id <- tolower(NMA_data_analysis_subset$contrast_id) # Convert contrast IDs to lowercase to match CNMA Master Database
  NMA_data_analysis_subset$contrast_id <- gsub("87213_t2", "87213_a", NMA_data_analysis_subset$contrast_id) # "87213_t2" changed to "87213_a" in the CNMA Master Database
  tabyl(NMA_data_analysis_subset$contrast_id)
  NMA_data_analysis_subset_short <- NMA_data_analysis_subset %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim)
  
  ## Merge on intervention_prelim & comparison_prelim to cNMA_data_4.1
  cNMA_data_4.1 %>% count()
  cNMA_data_4.1_NMAmerge <- cNMA_data_4.1 %>%
    inner_join(NMA_data_analysis_subset_short, by = c("study_id", "contrast_id", "es_id")) #Use inner join because we only want to retain observations of the CNMA database that have matches in the NNMA database.
  tabyl(cNMA_data_4.1_NMAmerge$intervention_prelim)
  tabyl(cNMA_data_4.1_NMAmerge$comparison_prelim)
  cNMA_data_4.1_NMAmerge %>% count()
  cNMA_data_4.1_NMAmerge <- cNMA_data_4.1_NMAmerge %>% filter(!is.na(intervention_prelim) & !is.na(comparison_prelim)) # Drop rows where intervention_prelim or comparison_prelim is NA  
  cNMA_data_4.1_NMAmerge %>% count()
  tabyl(cNMA_data_4.1_NMAmerge$intervention_prelim)
  tabyl(cNMA_data_4.1_NMAmerge$comparison_prelim)
  assert_character(cNMA_data_4.1_NMAmerge$intervention_content, any.missing = FALSE) 
  cNMA_data_4.1_NMAmerge <- cNMA_data_4.1_NMAmerge %>% filter(intervention_prelim!=comparison_prelim) #Drop observations with the exact same intervention bundle.
  cNMA_data_4.1_NMAmerge %>% count()
  
# Execute CNMA for Whole Numbers domain
  
  ## Filter dataset to Whole Numbers domain
  tabyl(cNMA_data_4.1_NMAmerge$intervention_content)
  cNMA_data_4.1_NMAmerge_icW <- cNMA_data_4.1_NMAmerge %>% filter(intervention_content == "W")
  tabyl(cNMA_data_4.1_NMAmerge_icW$intervention_content)
  
  ## Prepare component binaries for cNMA
  cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim, measure_name, domain, effect_size_final, variance_final, intervention_n, comparison_n)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$intervention_prelim)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$comparison_prelim)
  
  cNMA_data_4.1_NMAmerge_icW_short$FF <- 0
  cNMA_data_4.1_NMAmerge_icW_short$RS <- 0
  cNMA_data_4.1_NMAmerge_icW_short$NL <- 0
  cNMA_data_4.1_NMAmerge_icW_short$TES <- 0
  cNMA_data_4.1_NMAmerge_icW_short$VF <- 0
  cNMA_data_4.1_NMAmerge_icW_short$BAU <- 0
  
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$FF)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$RS)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$NL)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$TES)  
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$VF)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$BAU)
   
    ### FF
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="F",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="FF",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="NL+TES+FF+RS",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="VF+FF+RS",1, FF))
    
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(comparison_prelim=="FF" & FF==0,-1, ifelse(comparison_prelim=="FF" & FF==1, 0, FF)))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(comparison_prelim=="FF+RS" & FF==0,-1, ifelse(comparison_prelim=="FF+RS" & FF==1, 0, FF)))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(comparison_prelim=="NL+FF+RS" & FF==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & FF==1, 0, FF)))
    
    cNMA_data_4.1_NMAmerge_icW_short_FF <- cNMA_data_4.1_NMAmerge_icW_short %>% select(intervention_prelim, comparison_prelim, FF)
    print(cNMA_data_4.1_NMAmerge_icW_short_FF, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$FF)
  
    ### RS
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+FF+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+VF+RS",1, RS))  
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="TES+RS",1, RS))     
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="VF+FF+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="VF+RS",1, RS))
    
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(comparison_prelim=="FF+RS" & RS==0,-1, ifelse(comparison_prelim=="FF+RS" & RS==1, 0, RS)))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(comparison_prelim=="NL+FF+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & RS==1, 0, RS)))
    
    cNMA_data_4.1_NMAmerge_icW_short_RS <- cNMA_data_4.1_NMAmerge_icW_short %>% select(intervention_prelim, comparison_prelim, RS)
    print(cNMA_data_4.1_NMAmerge_icW_short_RS, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$RS)
    
    ### NL
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+RS",1, NL))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+FF+RS",1, NL))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+VF+RS",1, NL))  
    
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(comparison_prelim=="NL+FF+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & NL==1, 0, NL)))
    
    cNMA_data_4.1_NMAmerge_icW_short_NL <- cNMA_data_4.1_NMAmerge_icW_short %>% select(intervention_prelim, comparison_prelim, NL)
    print(cNMA_data_4.1_NMAmerge_icW_short_NL, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$NL)
    
    ### TES
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+FF+RS",1, TES))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+VF+RS",1, TES))  
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(TES = ifelse(intervention_prelim=="TES+RS",1, TES))  
 
    cNMA_data_4.1_NMAmerge_icW_short_TES <- cNMA_data_4.1_NMAmerge_icW_short %>% select(intervention_prelim, comparison_prelim, TES)
    print(cNMA_data_4.1_NMAmerge_icW_short_TES, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$TES)
    
    ### VF
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(VF = ifelse(intervention_prelim=="NL+TES+VF+RS",1, VF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(VF = ifelse(intervention_prelim=="VF+FF+RS",1, VF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(VF = ifelse(intervention_prelim=="VF+RS",1, VF))
    
    cNMA_data_4.1_NMAmerge_icW_short_VF <- cNMA_data_4.1_NMAmerge_icW_short %>% select(intervention_prelim, comparison_prelim, VF)
    print(cNMA_data_4.1_NMAmerge_icW_short_VF, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$VF)
    
    ### BAU
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$BAU)
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(BAU = ifelse(comparison_prelim=="BAU",-1, BAU))
    
    cNMA_data_4.1_NMAmerge_icW_short_BAU <- cNMA_data_4.1_NMAmerge_icW_short %>% select(intervention_prelim, comparison_prelim, BAU)
    print(cNMA_data_4.1_NMAmerge_icW_short_BAU, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$BAU) 

  str(cNMA_data_4.1_NMAmerge_icW_short)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies- Whole Numbers domain
  V_list <- vcalc(variance_final, cluster= study_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=intervention_prelim, grp2=comparison_prelim, w1=intervention_n, w2=comparison_n, data=cNMA_data_4.1_NMAmerge_icW_short)
  
  ## Fit 1-step additive CNMA- Whole Numbers domain
  mod_addCNMA_icW<- rma.mv(effect_size_final, V_list, 
                                     mods = ~ FF + RS + NL + TES + VF - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons
                                     random = ~ 1 | study_id/es_id, 
                                     rho=0.60, 
                                     data=cNMA_data_4.1_NMAmerge_icW_short)
  summary(mod_addCNMA_icW)


# Execute CNMA for Rational Numbers domain
  
  ## Filter dataset to Whole Numbers domain
  tabyl(cNMA_data_4.1_NMAmerge$intervention_content)
  cNMA_data_4.1_NMAmerge_icR <- cNMA_data_4.1_NMAmerge %>% filter(intervention_content == "R")
  tabyl(cNMA_data_4.1_NMAmerge_icR$intervention_content)
  
  ## Prepare component binaries for cNMA
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim, measure_name, domain, effect_size_final, variance_final, intervention_n, comparison_n)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$intervention_prelim)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$comparison_prelim)
  
  cNMA_data_4.1_NMAmerge_icR_short$FF <- 0
  cNMA_data_4.1_NMAmerge_icR_short$RS <- 0
  cNMA_data_4.1_NMAmerge_icR_short$NL <- 0
  cNMA_data_4.1_NMAmerge_icR_short$TES <- 0
  cNMA_data_4.1_NMAmerge_icR_short$VF <- 0
  cNMA_data_4.1_NMAmerge_icR_short$BAU <- 0
  
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$FF)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$RS)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$NL)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$TES)  
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$VF)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$BAU)
  
  ### FF
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="FF",1, FF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="NL+TES+FF+RS",1, FF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="VF+FF+RS",1, FF))
  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(comparison_prelim=="FF+RS" & FF==0,-1, ifelse(comparison_prelim=="FF+RS" & FF==1, 0, FF)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(comparison_prelim=="NL+FF+RS" & FF==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & FF==1, 0, FF)))

  cNMA_data_4.1_NMAmerge_icR_short_FF <- cNMA_data_4.1_NMAmerge_icR_short %>% select(intervention_prelim, comparison_prelim, FF)
  print(cNMA_data_4.1_NMAmerge_icR_short_FF, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$FF)
  
  ### RS
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+FF+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+VF+RS",1, RS))  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="VF+FF+RS",1, RS))
  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(comparison_prelim=="FF+RS" & RS==0,-1, ifelse(comparison_prelim=="FF+RS" & RS==1, 0, RS)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(comparison_prelim=="NL+FF+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & RS==1, 0, RS)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(comparison_prelim=="NL+TES+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+TES+RS" & RS==1, 0, RS)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(comparison_prelim=="RS" & RS==0,-1, ifelse(comparison_prelim=="RS" & RS==1, 0, RS)))
  
  cNMA_data_4.1_NMAmerge_icR_short_RS <- cNMA_data_4.1_NMAmerge_icR_short %>% select(intervention_prelim, comparison_prelim, RS)
  print(cNMA_data_4.1_NMAmerge_icR_short_RS, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$RS)
  
  ### NL
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+RS",1, NL))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+FF+RS",1, NL))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+RS",1, NL))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+VF+RS",1, NL))  
  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(comparison_prelim=="NL+FF+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & NL==1, 0, NL)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(comparison_prelim=="NL+TES+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+TES+RS" & NL==1, 0, NL)))
  
  cNMA_data_4.1_NMAmerge_icR_short_NL <- cNMA_data_4.1_NMAmerge_icR_short %>% select(intervention_prelim, comparison_prelim, NL)
  print(cNMA_data_4.1_NMAmerge_icR_short_NL, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$NL)
  
  ### TES
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+FF+RS",1, TES))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+RS",1, TES)) 
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+VF+RS",1, TES))  
  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(TES = ifelse(comparison_prelim=="NL+TES+RS" & TES==0,-1, ifelse(comparison_prelim=="NL+TES+RS" & TES==1, 0, TES)))
  
  cNMA_data_4.1_NMAmerge_icR_short_TES <- cNMA_data_4.1_NMAmerge_icR_short %>% select(intervention_prelim, comparison_prelim, TES)
  print(cNMA_data_4.1_NMAmerge_icR_short_TES, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$TES)
  
  ### VF
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(VF = ifelse(intervention_prelim=="NL+TES+VF+RS",1, VF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(VF = ifelse(intervention_prelim=="VF+FF+RS",1, VF))
  
  cNMA_data_4.1_NMAmerge_icR_short_VF <- cNMA_data_4.1_NMAmerge_icR_short %>% select(intervention_prelim, comparison_prelim, VF)
  print(cNMA_data_4.1_NMAmerge_icR_short_VF, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$VF)
  
  ### BAU
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$BAU)
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(BAU = ifelse(comparison_prelim=="BAU",-1, BAU))
  
  cNMA_data_4.1_NMAmerge_icR_short_BAU <- cNMA_data_4.1_NMAmerge_icR_short %>% select(intervention_prelim, comparison_prelim, BAU)
  print(cNMA_data_4.1_NMAmerge_icR_short_BAU, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$BAU) 
  
  str(cNMA_data_4.1_NMAmerge_icR_short)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies- Whole Numbers domain
  V_list <- vcalc(variance_final, cluster= study_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=intervention_prelim, grp2=comparison_prelim, w1=intervention_n, w2=comparison_n, data=cNMA_data_4.1_NMAmerge_icR_short)
  
  
  ## Fit 1-step additive CNMA- Whole Numbers domain
  mod_addCNMA_icR<- rma.mv(effect_size_final, V_list, 
                           mods = ~ FF + RS + NL + TES + VF - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons
                           random = ~ 1 | study_id/es_id, 
                           rho=0.60, 
                           data=cNMA_data_4.1_NMAmerge_icR_short)
  summary(mod_addCNMA_icR)  

  
  
  
  
  