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

# Make necessary data transformations before running CNMA
  
  ## Correct column classes
  class(cNMA_data_4.1$study_id)
  class(cNMA_data_4.1$contrast_id)
  class(cNMA_data_4.1$es_id)
  
  cNMA_data_4.1 <- cNMA_data_4.1 %>% mutate(contrast_id = as.character(contrast_id))
  class(cNMA_data_4.1$contrast_id)
  tabyl(cNMA_data_4.1$contrast_id)
  
  ## Merge on intervention_prelim & comparison_prelim to cNMA_data_4.1 until intervention and comparison columns are added to the CNMA database 
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set
  
  NMA_data_analysis_subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") & aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") & (TvsT==1 | TvsT==0))
  NMA_data_analysis_subset <- NMA_data_analysis_subset  %>% rename(study_id=record_id)  
  
  class(NMA_data_analysis_subset$contrast_id)
  NMA_data_analysis_subset <- NMA_data_analysis_subset %>% mutate(contrast_id = as.character(contrast_id))
  class(NMA_data_analysis_subset$contrast_id)
  tabyl(NMA_data_analysis_subset$contrast_id)
  NMA_data_analysis_subset$contrast_id <- gsub("_disagg", "", NMA_data_analysis_subset$contrast_id) # "_disagg" dropped from contrast IDs in the CNMA Master Database
  NMA_data_analysis_subset$contrast_id <- tolower(NMA_data_analysis_subset$contrast_id) # Convert contrast IDs to lowercase to match CNMA Master Database
  NMA_data_analysis_subset$contrast_id <- gsub("87213_t2", "87213_a", NMA_data_analysis_subset$contrast_id) # "87213_t2" changed to "87213_a" in the CNMA Master Database
  tabyl(NMA_data_analysis_subset$contrast_id)
  
  NMA_data_analysis_subset_short <- NMA_data_analysis_subset %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim)
  
  cNMA_data_4.1 <- cNMA_data_4.1 %>%
    inner_join(NMA_data_analysis_subset_short, by = c("study_id", "contrast_id", "es_id")) #Use left join because we want to retain the observations in the CNMA Master Database
  cNMA_data_4.1 <- cNMA_data_4.1 %>% filter(!is.na(intervention_prelim) & !is.na(comparison_prelim)) # Drop rows where intervention_prelim or comparison_prelim is NA
  
  ## Calculate variance from standard error and sample size
  cNMA_data_4.1 <- cNMA_data_4.1 %>% mutate(sample_size= intervention_n + comparison_n)
  cNMA_data_4.1 <- cNMA_data_4.1 %>% mutate(variance = ((standard_error_final^2) * sample_size)) # SE^2 x n
  describe(cNMA_data_4.1$standard_error_final)
  describe(cNMA_data_4.1$variance)
  cNMA_data_4.1_Vcheck <- cNMA_data_4.1 %>% dplyr::select(study_id, contrast_id, es_id, domain, intervention_content, sample_size, standard_error_final, variance, intervention_prelim, comparison_prelim)
  #View(cNMA_data_4.1_Vcheck)
  
  ## Drop rows with same intervention bundle on both sides of the contrast
  cNMA_data_4.1 <- cNMA_data_4.1 %>% filter(intervention_prelim!=comparison_prelim) 
  
# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Whole Numbers (W)"

  ## Subset analysis data frame further to just the Whole Numbers (W) intervention content (icW)
  tabyl(cNMA_data_4.1$intervention_content)
  tabyl(cNMA_data_4.1$domain)
  cNMA_data_4.1 %>% count(domain, intervention_content, sort = TRUE)

  cNMA_data_4.1_icW <- cNMA_data_4.1 %>% filter(intervention_content == "W")
  tabyl(cNMA_data_4.1_icW$intervention_content)
  cNMA_data_4.1_icW_c <- cNMA_data_4.1_icW %>% distinct(contrast_id, .keep_all = TRUE)
  cNMA_data_4.1_icW_c %>% count()
  
  ## Temporarily drop contrasts with multiple measures but different intervention bundles across measures.
  cNMA_data_4.1_icW <- cNMA_data_4.1_icW %>% filter(!contrast_id %in% c("90512_a","87463_a","90429_a","90429_b","90512_b",74201, 86432, 87196, 87211, 87222, 87244, 87462, 88222, 88224, 88229, 89408, 90578))
  
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
  
# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Rational Numbers (R)"
  
  ## Subset analysis data frame further to just the Rational Numbers (R) intervention content (icR)
  tabyl(cNMA_data_4.1$intervention_content)
  tabyl(cNMA_data_4.1$domain)
  cNMA_data_4.1 %>% count(domain, intervention_content, sort = TRUE)
  
  cNMA_data_4.1_icR <- cNMA_data_4.1 %>% filter(intervention_content == "R")
  tabyl(cNMA_data_4.1_icR$intervention_content)
  cNMA_data_4.1_icR_c <- cNMA_data_4.1_icR %>% distinct(contrast_id, .keep_all = TRUE)
  cNMA_data_4.1_icR_c %>% count()
  
  ## Temporarily drop contrasts with multiple measures but different intervention bundles across measures.
  cNMA_data_4.1_icR <- cNMA_data_4.1_icR %>% filter(!contrast_id %in% c("90582_b", 82135, 88692, 88693, 88774, 88775, 89198, 89814, 90583, 86189, 86431, 88709, 88710, 89453, 89454, 89179))
  
  ## Pooling ESs at the contrast level using metafor
  mod_es_icR <- rma.mv(yi = effect_size_final, V = variance, data = cNMA_data_4.1_icR,
                       random = ~ 1 | study_id/contrast_id/es_id,
                       mods = ~ factor(contrast_id) - 1,
                       rho = 0.60,
                       method = "ML")
  
  ## Extracting and organizing ESs for CNMAs
  dat_es_icR <- tibble(contrast_id = word(names(coef(mod_es_icR)), 2, sep = "[)]"),
                       pooled_es = mod_es_icR$beta[, 1],
                       pooled_se = mod_es_icR$se) %>% 
    left_join(cNMA_data_4.1_icR, by = "contrast_id") %>% 
    dplyr::select(study_id, contrast_id:pooled_se, intervention_prelim, comparison_prelim) %>% 
    distinct()
  
  ## Base NMA and Additive CNMA
  mod_baseNMA_icR <- netmeta(pooled_es, pooled_se, intervention_prelim, comparison_prelim, studlab = contrast_id,
                             data = dat_es_icR, ref = "BAU", sm = "SMD", common = FALSE, method.tau = "REML", )
  
  mod_addCNMA_icR <- netcomb(mod_baseNMA_icR, inactive = "BAU")
  mod_addCNMA_icR  