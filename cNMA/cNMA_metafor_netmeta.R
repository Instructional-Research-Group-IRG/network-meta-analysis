# This script performs a component network meta-analysis (NMA) of mathematics education interventions using outcomes in the whole and ration numbers domains. 
# Sample: all nodes
# Variable for defining outcome domain: intervention_content
# Disaggregated by domain: Yes

# Load required packages

  ## Install 'devel' version of metafor package
  ##install.packages("remotes") 
  ##remotes::install_github("wviechtb/metafor") 
  
  ## Install and load other required packages
  ##install.packages("pacman") 
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor, naniar, igraph, multcomp, broom, gridExtra, ggplot2, writexl, readr, grid, gridExtra, cowplot, extrafont, psych)

# Load (read) data (i.e., copy data to 'dat')
  cNMA_data_4.1 <- read_csv('cNMA_data_4.1.csv')
  
# Merge on intervention_prelim & comparison_prelim  
  NMA_data_analysis_subset_grpID_short <- NMA_data_analysis_subset_grpID  %>% rename(study_id=record_id)
  NMA_data_analysis_subset_grpID_short <- NMA_data_analysis_subset_grpID_short %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim)
  cNMA_data_4.1 <- cNMA_data_4.1 %>%
    right_join(NMA_data_analysis_subset_grpID_short, by = c("study_id", "contrast_id", "es_id")) #Use left join because we want to retain the observations in the CNMA Master Database

# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Whole Numbers (W)"

  ## Subset analysis data frame further to just the Whole Numbers (W) intervention content (icW)
  tabyl(cNMA_data_4.1$intervention_content)
  tabyl(cNMA_data_4.1$domain)
  cNMA_data_4.1_domain <- cNMA_data_4.1 %>% dplyr::select(study_id, contrast_id, es_id, domain, intervention_content)
  
  cNMA_data_4.1_icW <- cNMA_data_4.1 %>% filter(intervention_content == "W")
  tabyl(cNMA_data_4.1_icW$intervention_content)
  cNMA_data_4.1_icW_c <- cNMA_data_4.1_icW %>% distinct(contrast_id, .keep_all = TRUE)
  cNMA_data_4.1_icW_c %>% count()
  
  cNMA_data_4.1_icW <- cNMA_data_4.1_icW %>% mutate(sample_size= intervention_n + comparison_n)
  cNMA_data_4.1_icW <- cNMA_data_4.1_icW %>% mutate(variance = ((standard_error_final^2) * sample_size)) # SE^2 x n
  describe(cNMA_data_4.1_icW$standard_error_final)
  describe(cNMA_data_4.1_icW$variance)
  cNMA_data_4.1_icW_Vcheck <- cNMA_data_4.1_icW %>% dplyr::select(study_id, contrast_id, es_id, domain, intervention_content, sample_size, standard_error_final, variance) # Check for negative variances
  #View(cNMA_data_4.1_icW_Vcheck)
  
  ## Pooling ESs at the contrast level using metafor
  mod_es_icW <- rma.mv(yi = effect_size_final, V = variance, data = cNMA_data_4.1_icW,
                       random = ~ 1 | study_id/contrast_id/es_id,
                       mods = ~ factor(contrast_id) - 1,
                       rho = 0.60,
                       method = "ML")
  
  ## Extracting and organizing ESs for CNMAs
  dat_es_icW <- tibble(contrast_id = word(names(coef(mod_es_icW)), 2, sep = "[)]"),
                       pooled_es = mod_es_icW$beta[, 1],
                       pooled_se = mod_es_icW$se) %>% 
        left_join(cNMA_data_4.1_icW, by = "contrast_id") %>% 
        dplyr::select(study_id, contrast_id:pooled_se, intervention_prelim, comparison_prelim) %>% 
        distinct()
  
  ## Base NMA and Additive CNMA
  mod_baseNMA_icW <- netmeta(pooled_es, pooled_se, intervention_prelim, comparison_prelim, studlab = contrast_id,
                         data = dat_es_icW, ref = "BAU", sm = "SMD", common = FALSE, method.tau = "REML")
  
  mod_addCNMA_icW <- netcomb(mod_baseNMA_icW, inactive = "BAU")
  mod_addCNMA_icW