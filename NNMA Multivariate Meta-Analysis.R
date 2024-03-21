#Load libraries
  library(googlesheets4)
  library(tidyverse)
  library(metafor)
  library(robumeta)
  library(clubSandwich)
  library(weightr)
  library(assertable)
  library(janitor)
  library(naniar)
  library(glmulti)
  library(MuMIn)

#Primary Database 
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0")
  NNMA_Data <- subset(NNMA_Data, !is.na(record_id)) #There are no true rows after row 608 (not counting the headers/column names as row 1). Those after row 608 are loaded in despite having no record IDs because column C is incorrectly filled out with "FALSE" after row 608.
  
  ##This subset is specific to the meta-regression model. It is a preliminary subset that includes only BAU controls.
  NNMA_Data_Subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") &
                             aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") &
                             comparison_prelim=="BAU")
  
  ##Replace all NA values in the moderators with 0 to avoid the "Processing terminated since k <= 1" error
  NNMA_Data_Subset <- NNMA_Data_Subset %>% replace_na(list(NL_TX = 0, SE_TX = 0, VF_TX = 0, F_TX = 0, BX_TX = 0, RS_TX = 0, TE_TX = 0))

#Create unique group ID for each independent group within a study (record ID)

  ##Keep only record ID, intervention/comparison bundle, intervention/comparison sample size 
  NNMA_Data_grpID <- NNMA_Data_Subset %>% select(record_id, intervention = intervention_prelim, comparison = comparison_prelim, intervention_n, comparison_n)
  str(NNMA_Data_grpID)
  
  ## For each assignment group, combine component bundle and sample size into a single character variable to distinguish groups with the same sample sizes but different component bundles. 
  ## In such cases, these are presumably two unique groups of individuals because they received different component bundles despite happening to have the same sample sizes. 
  ## There may be no cases within the same record ID in which two different component bundles happen to have the same group sample sizes but let's control for that just in case.
  str(NNMA_Data_grpID)
  NNMA_Data_grpID <- NNMA_Data_grpID %>% mutate(intervention_n_chr = as.character(intervention_n))
  NNMA_Data_grpID <- NNMA_Data_grpID %>% unite("int_n_chr_bundle" , c(intervention, intervention_n_chr), remove = FALSE)
  NNMA_Data_grpID <- NNMA_Data_grpID %>% mutate(comparison_n_chr = as.character(comparison_n))
  NNMA_Data_grpID <- NNMA_Data_grpID %>% unite("com_n_chr_bundle" , c(comparison,comparison_n_chr), remove = FALSE)
  str(NNMA_Data_grpID)
  NNMA_Data_grpID
  
  NNMA_Data_grpID <- NNMA_Data_grpID %>% select(record_id, int_n_chr_bundle, com_n_chr_bundle)
  NNMA_Data_grpID
  
  ## Reshape long so that total number of unique groups within each record ID can be counted and assigned a unique group ID 
  NNMA_Data_grpID_long <- NNMA_Data_grpID %>% pivot_longer(-record_id, names_to="assignment", values_to="bundle_samplesize")
  NNMA_Data_grpID_long
  
  ## Keep only unique combinations of assignment bundle + group sample size within record ID so that unique group IDs can be assigned.
  ## Do this regardless of intervention/comparison group assignment because the same combination of bundle + sample size presumably should be considered one unique group regardless of group assignment.
  ## For example, one combination of bundle + sample size could have been assigned to intervention for one contrast but to comparison for another contrast of the same record ID. 
  ## In this case, this would be the same unique group of individuals in both contrasts despite the different group assignments and therefore should have the same group ID (not two different ones) to capture all dependencies.
  ## There may be no cases of this in the data but let's control for it just in case by removing bundle + group sample size duplicates within each study regardless of group assignment.
  NNMA_Data_grpID_long_unique <- NNMA_Data_grpID_long %>% group_by(record_id) %>% distinct(bundle_samplesize, .keep_all = TRUE) %>% select(-assignment) %>% ungroup()
  NNMA_Data_grpID_long_unique
  
  ##Create unique group ID for each combination of bundle + sample size, by record ID (study)
  NNMA_Data_grpID_long_unique_withids <- NNMA_Data_grpID_long_unique %>% group_by(record_id) %>% mutate(group_id=row_number()) %>% ungroup()
  NNMA_Data_grpID_long_unique_withids
  
  ## Merge group IDs onto main data set
  ## Note that above, we created {NNMA_Data_grpID_long_unique_withids} which is effectively a master list of the group IDs in which each group ID is a unique combination of bundle + sample size within each record ID 
  ##   regardless of group assignment.
  ## The main data set has intervention and comparison groups in separate columns. But because the master group ID list is agnostic to group assignment, we can use it to merge on group IDs to both intervention and comparison 
  ##   groups in the following two steps:
  ##     i) once for the intervention groups; then ii) a second time for the comparison groups. 
  ## If our logic and subsequent group ID coding above are correct, the result should be each intervention and comparison group receiving a unique group ID (i.e., there should be no missing group ID values after the merges); 
  ##   AND there should be no intervention and comparison groups of the same contrast (i.e., within the same row) with the same group ID.
  ## However, as noted above, the same group (i.e., the same bundle + sample size combination) in different contrasts (rows) of the same record ID should receive the same group ID regardless of their group assignment 
  ##   across those contrasts because they are the same unique group of individuals, otherwise we do not capture all dependencies.
  
  ## Merge unique group IDs onto the intervention groups
  NNMA_Data_grpID_long_unique_withids_Imerge <- NNMA_Data_grpID_long_unique_withids %>% separate(bundle_samplesize, c("intervention_prelim","intervention_n"), "_")
  NNMA_Data_grpID_long_unique_withids_Imerge
  NNMA_Data_grpID_long_unique_withids_Imerge$intervention_n <- as.numeric(NNMA_Data_grpID_long_unique_withids_Imerge$intervention_n)
  NNMA_Data_grpID_long_unique_withids_Imerge <- NNMA_Data_grpID_long_unique_withids_Imerge %>% rename(group1_id = group_id)
  str(NNMA_Data_grpID_long_unique_withids_Imerge)
  NNMA_Data_Subset$intervention_prelim <- NNMA_Data_Subset$intervention_prelim %>% replace_na("NA") #This facilitates the merge below. When the IDs were created, intervention and intervention_n were combined then separated, which creates "NA" character values from the original true NA values, which don't match in a merge.
  NNMA_Data_Subset_grpID <- NNMA_Data_Subset %>% left_join(NNMA_Data_grpID_long_unique_withids_Imerge, by = c("record_id","intervention_prelim","intervention_n"))
  NNMA_Data_Subset_grpID %>% group_by(group1_id) %>% count()
  
  ## Merge unique group IDs onto the comparison groups  
  NNMA_Data_grpID_long_unique_withids_Cmerge <- NNMA_Data_grpID_long_unique_withids %>% separate(bundle_samplesize, c("comparison_prelim","comparison_n"), "_")  
  NNMA_Data_grpID_long_unique_withids_Cmerge
  NNMA_Data_grpID_long_unique_withids_Cmerge$comparison_n <- as.numeric(NNMA_Data_grpID_long_unique_withids_Cmerge$comparison_n)
  NNMA_Data_grpID_long_unique_withids_Cmerge <- NNMA_Data_grpID_long_unique_withids_Cmerge %>% rename(group2_id = group_id)
  str(NNMA_Data_grpID_long_unique_withids_Cmerge)
  NNMA_Data_Subset_grpID$comparison_prelim <- NNMA_Data_Subset_grpID$comparison_prelim %>% replace_na("NA") #This facilitates the merge below. When the IDs were created, comparison_prelim and comparison were combined then separated, which creates "NA" character values from the original true NA values, which don't match in a merge.
  NNMA_Data_Subset_grpID <- NNMA_Data_Subset_grpID %>% left_join(NNMA_Data_grpID_long_unique_withids_Cmerge, by = c("record_id","comparison_prelim","comparison_n"))
  NNMA_Data_Subset_grpID %>% group_by(group2_id) %>% count()
  
  ## Validate merge results
  assert_values(NNMA_Data_Subset_grpID, colnames= c("group1_id","group2_id"), test = "not_na", test_val = NA)
  assert_values(NNMA_Data_Subset_grpID, colnames= "group1_id", test="not_equal", test_val= "group2_id")
  NNMA_Data_Subset_grpID_check <- NNMA_Data_Subset_grpID %>% select(record_id, contrast_id, aggregated, measure_type, measure_name, wwc_rating, intervention_prelim, intervention_n, group1_id, comparison_prelim, comparison_n, group2_id)
  NNMA_Data_Subset_grpID_check %>% print(n = Inf) 
  
  ## Restore "NA" (non-missing) values to their true <NA> (missing) values because the unite then separate functions used above changed the values from <NA> to "NA"
  tabyl(NNMA_Data_Subset_grpID$intervention_prelim)
  tabyl(NNMA_Data_Subset_grpID$comparison_prelim)
  NNMA_Data_Subset_grpID <- NNMA_Data_Subset_grpID %>% replace_with_na_at(.vars = c("intervention_prelim","comparison_prelim"), condition = ~.x %in% common_na_strings)
  tabyl(NNMA_Data_Subset_grpID$intervention_prelim)
  tabyl(NNMA_Data_Subset_grpID$comparison_prelim)

#Create covariance matrix
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NNMA_Data_Subset_grpID)

#Run multivariate meta-regression
  NNMA_MVmodel <- rma.mv(yi = effect_size, 
                          V = V_list, 
                          random = ~ 1 | record_id/contrast_name/domain/es_id,
                          test =  "t", 
                          data = NNMA_Data_Subset_grpID, 
                          method = "REML")
  summary(NNMA_MVmodel)  

  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_MVmodel,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf
  
  ##Find prediction interval
  (PI_low <- coef(NNMA_MVmodel) - 1.96*sqrt(NNMA_MVmodel$sigma2[1] + NNMA_MVmodel$sigma2[2]))
  (PI_high <- coef(NNMA_MVmodel) + 1.96*sqrt(NNMA_MVmodel$sigma2[1] + NNMA_MVmodel$sigma2[2]))

  ##Add moderators (as.numeric(unlist(final_domain)))
  NNMA_MVmod_model <- rma.mv(yi = effect_size, 
                             V = V_list, 
                             random = ~ 1 | record_id/contrast_name/domain/es_id,
                             mods = ~ NL_TX + SE_TX + VF_TX + F_TX + BX_TX + RS_TX - 1,
                             test =  "t", 
                             data = NNMA_Data_Subset_grpID, 
                             method = "REML")
  summary(NNMA_MVmod_model)
  
  ##Use RVE
  NNMA_mvMODcf <- coef_test(NNMA_MVmod_model,
                            cluster = NNMA_Data_Subset_grpID$record_id, 
                            vcov = "CR2")
  NNMA_mvMODcf

  ##Calculate Correlation Matrix
    cor(NNMA_Data_Subset_grpID[c("NL_TX", "SE_TX", "VF_TX", "F_TX", "BX_TX", "RS_TX")])

  
# Meta-regression model selection methods: Hierarchical, Step-wise, and Multi-model inference
  ## https://en.wikipedia.org/wiki/Akaike_information_criterion // Primer on model selection using AIC
  
  ## Hierarchical
    ### Notes: For this model, we would run multiple models, one after another, adding a new covariate each time based on their theoretical importance. 
    ###        For the sake of setting up the analysis, let’s use the following variables in the following order: 1. NL_TX; 2.	VF_TX; 3.	RS_TX; 4.	SE_TX; 5.	F_TX
    
    ### Model 1: NL_TX
    NNMA_MVmod_Hmodel1 <- rma.mv(yi = effect_size, 
                                V = V_list, 
                                random = ~ 1 | record_id/contrast_name/domain/es_id,
                                mods = ~ NL_TX - 1,
                                test =  "t", 
                                data = NNMA_Data_Subset_grpID, 
                                method = "REML")
    summary(NNMA_MVmod_Hmodel1)   
    
    ### Model 2: NL_TX + VF_TX 
    NNMA_MVmod_Hmodel2 <- rma.mv(yi = effect_size, 
                                V = V_list, 
                                random = ~ 1 | record_id/contrast_name/domain/es_id,
                                mods = ~ NL_TX + VF_TX - 1,
                                test =  "t", 
                                data = NNMA_Data_Subset_grpID, 
                                method = "REML")
    summary(NNMA_MVmod_Hmodel2)
    
    ### Model 3: NL_TX + VF_TX + RS_TX
    NNMA_MVmod_Hmodel3 <- rma.mv(yi = effect_size, 
                                V = V_list, 
                                random = ~ 1 | record_id/contrast_name/domain/es_id,
                                mods = ~ NL_TX + VF_TX + RS_TX - 1,
                                test =  "t", 
                                data = NNMA_Data_Subset_grpID, 
                                method = "REML")
    summary(NNMA_MVmod_Hmodel3)  
    
    ### Model 4: NL_TX + VF_TX + RS_TX + SE_TX
    NNMA_MVmod_Hmodel4 <- rma.mv(yi = effect_size, 
                                V = V_list, 
                                random = ~ 1 | record_id/contrast_name/domain/es_id,
                                mods = ~ NL_TX + VF_TX + RS_TX + SE_TX - 1,
                                test =  "t", 
                                data = NNMA_Data_Subset_grpID, 
                                method = "REML")
    summary(NNMA_MVmod_Hmodel4)
    
    ### Model 5: NL_TX + VF_TX + RS_TX + SE_TX + F_TX
    NNMA_MVmod_Hmodel5 <- rma.mv(yi = effect_size, 
                                V = V_list, 
                                random = ~ 1 | record_id/contrast_name/domain/es_id,
                                mods = ~ NL_TX + VF_TX + RS_TX + SE_TX + F_TX - 1,
                                test =  "t", 
                                data = NNMA_Data_Subset_grpID, 
                                method = "REML")
    summary(NNMA_MVmod_Hmodel5)
 
  ## Step-wise 
    ### Notes: First we would look at each covariate in isolation. Then, run a series of models (as above) but where each is added in order of highest explanation of variability to least.
    NNMA_MVmod_model_nomods <- rma.mv(yi = effect_size, 
                                     V = V_list, 
                                     random = ~ 1 | record_id/contrast_name/domain/es_id,
                                     test =  "t", 
                                     data = NNMA_Data_Subset_grpID, 
                                     method = "REML")
    summary(NNMA_MVmod_model_nomods)
    
    NNMA_MVmod_model_NL_TX <- rma.mv(yi = effect_size, 
                                     V = V_list, 
                                     random = ~ 1 | record_id/contrast_name/domain/es_id,
                                     mods = ~ NL_TX - 1,
                                     test =  "t", 
                                     data = NNMA_Data_Subset_grpID, 
                                     method = "REML")
    summary(NNMA_MVmod_model_NL_TX)
    
    NNMA_MVmod_model_VF_TX <- rma.mv(yi = effect_size, 
                                     V = V_list, 
                                     random = ~ 1 | record_id/contrast_name/domain/es_id,
                                     mods = ~ VF_TX - 1,
                                     test =  "t", 
                                     data = NNMA_Data_Subset_grpID, 
                                     method = "REML")
    summary(NNMA_MVmod_model_VF_TX)   
    
    NNMA_MVmod_model_RS_TX <- rma.mv(yi = effect_size, 
                                     V = V_list, 
                                     random = ~ 1 | record_id/contrast_name/domain/es_id,
                                     mods = ~ RS_TX - 1,
                                     test =  "t", 
                                     data = NNMA_Data_Subset_grpID, 
                                     method = "REML")
    summary(NNMA_MVmod_model_RS_TX)  
    
    NNMA_MVmod_model_SE_TX <- rma.mv(yi = effect_size, 
                                     V = V_list, 
                                     random = ~ 1 | record_id/contrast_name/domain/es_id,
                                     mods = ~ SE_TX - 1,
                                     test =  "t", 
                                     data = NNMA_Data_Subset_grpID, 
                                     method = "REML")
    summary(NNMA_MVmod_model_SE_TX) 
    
    NNMA_MVmod_model_F_TX <- rma.mv(yi = effect_size, 
                                     V = V_list, 
                                     random = ~ 1 | record_id/contrast_name/domain/es_id,
                                     mods = ~ F_TX - 1,
                                     test =  "t", 
                                     data = NNMA_Data_Subset_grpID, 
                                     method = "REML")
    summary.rma(NNMA_MVmod_model_F_TX)    
    
    ### Review variation explained in each model
    fitstats.rma(NNMA_MVmod_model_nomods, NNMA_MVmod_model_NL_TX, NNMA_MVmod_model_VF_TX, NNMA_MVmod_model_RS_TX, NNMA_MVmod_model_SE_TX, NNMA_MVmod_model_F_TX)
      
      #### Note: rma.mv does not produce R-squared or related values (NA is returned). Calculate a psuedo-Rsquared
      ####       Refer to https://stat.ethz.ch/pipermail/r-sig-meta-analysis/2017-September/000232.html for the calculation of psuedo-R squared: psuedo-R2= (sum(res0$sigma2) - sum(res1$sigma2)) / sum(res0$sigma2). Gives the proportional reduction in variance.
    
      R2_NL_TX <- (sum(NNMA_MVmod_model_nomods$sigma2) - sum(NNMA_MVmod_model_NL_TX$sigma2)) / sum(NNMA_MVmod_model_nomods$sigma2)
      R2_VF_TX <- (sum(NNMA_MVmod_model_nomods$sigma2) - sum(NNMA_MVmod_model_VF_TX$sigma2)) / sum(NNMA_MVmod_model_nomods$sigma2)
      R2_RS_TX <- (sum(NNMA_MVmod_model_nomods$sigma2) - sum(NNMA_MVmod_model_RS_TX$sigma2)) / sum(NNMA_MVmod_model_nomods$sigma2)
      R2_SE_TX <- (sum(NNMA_MVmod_model_nomods$sigma2) - sum(NNMA_MVmod_model_SE_TX$sigma2)) / sum(NNMA_MVmod_model_nomods$sigma2)
      R2_F_TX  <- (sum(NNMA_MVmod_model_nomods$sigma2) - sum(NNMA_MVmod_model_F_TX$sigma2)) / sum(NNMA_MVmod_model_nomods$sigma2)
    
      R2_NL_TX
      R2_VF_TX
      R2_RS_TX
      R2_SE_TX
      R2_F_TX
      
      #### Alternate psuedo-R suqared method: 1 - (log likelihood for full model / log likelihood for constant-only model)
      logLik_nomods <- logLik(NNMA_MVmod_model_nomods)
      logLik_NL_TX <- logLik(NNMA_MVmod_model_NL_TX)
      logLik_VF_TX <- logLik(NNMA_MVmod_model_VF_TX)      
      logLik_RS_TX <- logLik(NNMA_MVmod_model_RS_TX)
      logLik_SE_TX <- logLik(NNMA_MVmod_model_SE_TX)
      logLik_F_TX <- logLik(NNMA_MVmod_model_F_TX)
      
      R2v2_NL_TX = 1 - (logLik_NL_TX / logLik_nomods)
      R2v2_VF_TX = 1 - (logLik_VF_TX / logLik_nomods)
      R2v2_RS_TX = 1 - (logLik_RS_TX / logLik_nomods)
      R2v2_SE_TX = 1 - (logLik_SE_TX / logLik_nomods)
      R2v2_F_TX = 1 - (logLik_F_TX / logLik_nomods)
      
      R2v2_NL_TX
      R2v2_VF_TX
      R2v2_RS_TX
      R2v2_SE_TX
      R2v2_F_TX
      
    ### Execute step-wise regressions 
    ### Note: Use deviance as a proxy for variance explained for now (lower deviance = better model fit): NL_TX -> SE_TX -> RS_TX -> F_TX -> VF_TX 
    ###       Unclear how to interpret the two versions of psuedo R-squared.
      ### Model 1: NL_TX
      NNMA_MVmod_Smodel1 <- rma.mv(yi = effect_size, 
                                   V = V_list, 
                                   random = ~ 1 | record_id/contrast_name/domain/es_id,
                                   mods = ~ NL_TX - 1,
                                   test =  "t", 
                                   data = NNMA_Data_Subset_grpID, 
                                   method = "REML")
      summary(NNMA_MVmod_Smodel1)   
      
      ### Model 2: NL_TX + SE_TX 
      NNMA_MVmod_Smodel2 <- rma.mv(yi = effect_size, 
                                   V = V_list, 
                                   random = ~ 1 | record_id/contrast_name/domain/es_id,
                                   mods = ~ NL_TX + SE_TX - 1,
                                   test =  "t", 
                                   data = NNMA_Data_Subset_grpID, 
                                   method = "REML")
      summary(NNMA_MVmod_Smodel2)
      
      ### Model 3: NL_TX + SE_TX + RS_TX
      NNMA_MVmod_Smodel3 <- rma.mv(yi = effect_size, 
                                   V = V_list, 
                                   random = ~ 1 | record_id/contrast_name/domain/es_id,
                                   mods = ~ NL_TX + SE_TX + RS_TX - 1,
                                   test =  "t", 
                                   data = NNMA_Data_Subset_grpID, 
                                   method = "REML")
      summary(NNMA_MVmod_Smodel3)  
      
      ### Model 4: NL_TX + SE_TX + RS_TX + F_TX
      NNMA_MVmod_Smodel4 <- rma.mv(yi = effect_size, 
                                   V = V_list, 
                                   random = ~ 1 | record_id/contrast_name/domain/es_id,
                                   mods = ~ NL_TX + SE_TX + RS_TX + F_TX - 1,
                                   test =  "t", 
                                   data = NNMA_Data_Subset_grpID, 
                                   method = "REML")
      summary(NNMA_MVmod_Smodel4)
      
      ### Model 5: NL_TX + SE_TX + RS_TX + F_TX + VF_TX
      NNMA_MVmod_Smodel5 <- rma.mv(yi = effect_size, 
                                   V = V_list, 
                                   random = ~ 1 | record_id/contrast_name/domain/es_id,
                                   mods = ~ NL_TX + SE_TX + RS_TX + F_TX + VF_TX - 1,
                                   test =  "t", 
                                   data = NNMA_Data_Subset_grpID, 
                                   method = "REML")
      summary(NNMA_MVmod_Smodel5)
    
  ## Multi-model inference: model selection using glmulti
    ### Notes: Use glmulti package to run models of all possible combinations of the moderators
    ###        The following code is based on the following authored by the creator of the metafor package: https://www.metafor-project.org/doku.php/tips:model_selection_with_glmulti_and_mumin
    
    ### "remove rows where at least one of the values of these 7 moderator variables is missing"
    dat_glmulti <- NNMA_Data_Subset_grpID
    dat_glmulti <- dat_glmulti[!apply(dat_glmulti[,c("NL_TX", "VF_TX", "RS_TX", "SE_TX", "F_TX")], 1, anyNA),]
    
    ### "define a function that (a) takes a model formula and dataset as input and (b) then fits a mixed-effects meta-regression model to the given data using maximum likelihood estimation:"
    rma.glmulti <- function(formula, data, ...) {
    rma.mv(formula, variance,
           random = ~ 1 | record_id/contrast_name/domain/es_id,
           data=data, method="ML", ...) #(?) Do we want to use REML as above? "It is important to use ML (instead of REML) estimation, since log-likelihoods (and hence information criteria) are not directly comparable for models with different fixed effects (although see Gurka, 2006, for a different perspective on this)."
    }
    
    ### Fit all possible models
    system.time(res_glmulti <- glmulti(effect_size ~ NL_TX + VF_TX	+ RS_TX	+ SE_TX	+ F_TX, data=dat_glmulti,
                                       level=1, fitfunction=rma.glmulti, crit="aicc", confsetsize=32)) # With level = 1, we stick to models with main effects only. This implies that there are 2^5= 32 possible models in the candidate set to consider. 
    ### Review results
    print(res_glmulti)
    plot(res_glmulti) 
    
    ### Look at the top models
    top <- weightable(res_glmulti)
    top <- top[top$aicc <= min(top$aicc) + 2,] 
    top
    
    ### Review details of "top" model
    summary(res_glmulti@objects[[1]])
    
    ### Review relative variable importance
    plot(res_glmulti, type="s")
    
    ### Multimodel Inference
    eval(metafor:::.glmulti)
    coef(res_glmulti, varweighting="Johnson")
    
  ## Multi-model inference: model selection using MuMIN
    eval(metafor:::.MuMIn)
    
    # fit all possible models
    system.time(res_mumin <- dredge(NNMA_MVmod_model, trace=2)) # For this we enter the "full" model from above
    res_mumin
    
    # multimodel inference
    summary(model.avg(res_mumin))
    
    # for easier comparison with the results from glmulti
    #round(mmi[colnames(model.avg(res_mumin)$coefficients),], 4)