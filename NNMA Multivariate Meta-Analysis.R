#Load libraries
  library(googlesheets4)
  library(tidyverse)
  library(metafor)
  library(robumeta)
  library(clubSandwich)
  library(weightr)

#Primary Database 
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0")

  ##This subset is specific to the meta-regression model. It is a preliminary subset that includes only BAU controls.
  NNMA_Data_Subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") &
                             aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") &
                             comparison_prelim=="BAU")
  
  ##Replace all NA values in the moderators with 0 to avoid the "Processing terminated since k <= 1" error
  NNMA_Data_Subset <- NNMA_Data_Subset %>% replace_na(list(NL_TX = 0, SE_TX = 0, VF_TX = 0, F_TX = 0, BX_TX = 0, RS_TX = 0, TE_TX = 0))

#Create unique group ID for each independent group within a study (record ID)

  ##Keep only record ID, intervention/comparison bundle, intervention/comparison sample size 
  NMA_data_grpID <- NNMA_Data_Subset %>% select(record_id, intervention = intervention_prelim, comparison = comparison_prelim, intervention_n, comparison_n)
  str(NMA_data_grpID)
  
  ## For each assignment group, combine component bundle and sample size into a single character variable to distinguish groups with the same sample sizes but different component bundles. 
  ## In such cases, these are presumably two unique groups of individuals because they received different component bundles despite happening to have the same sample sizes. 
  ## There may be no cases within the same record ID in which two different component bundles happen to have the same group sample sizes but let's control for that just in case.
  str(NMA_data_grpID)
  NMA_data_grpID <- NMA_data_grpID %>% mutate(intervention_n_chr = as.character(intervention_n))
  NMA_data_grpID <- NMA_data_grpID %>% unite("int_n_chr_bundle" , c(intervention, intervention_n_chr), remove = FALSE)
  NMA_data_grpID <- NMA_data_grpID %>% mutate(comparison_n_chr = as.character(comparison_n))
  NMA_data_grpID <- NMA_data_grpID %>% unite("com_n_chr_bundle" , c(comparison,comparison_n_chr), remove = FALSE)
  str(NMA_data_grpID)
  NMA_data_grpID
  
  NMA_data_grpID <- NMA_data_grpID %>% select(record_id, int_n_chr_bundle, com_n_chr_bundle)
  NMA_data_grpID
  
  ## Reshape long so that total number of unique groups within each record ID can be counted and assigned a unique group ID 
  NMA_data_grpID_long <- NMA_data_grpID %>% pivot_longer(-record_id, names_to="assignment", values_to="bundle_samplesize")
  NMA_data_grpID_long
  
  ## Keep only unique combinations of assignment bundle + group sample size within record ID so that unique group IDs can be assigned.
  ## Do this regardless of intervention/comparison group assignment because the same combination of bundle + sample size presumably should be considered one unique group regardless of group assignment.
  ## For example, one combination of bundle + sample size could have been assigned to intervention for one contrast but to comparison for another contrast of the same record ID. 
  ## In this case, this would be the same unique group of individuals in both contrasts despite the different group assignments and therefore should have the same group ID (not two different ones) to capture all dependencies.
  ## There may be no cases of this in the data but let's control for it just in case by removing bundle + group sample size duplicates within each study regardless of group assignment.
  NMA_data_grpID_long_unique <- NMA_data_grpID_long %>% group_by(record_id) %>% distinct(bundle_samplesize, .keep_all = TRUE) %>% select(-assignment) %>% ungroup()
  NMA_data_grpID_long_unique
  
  ##Create unique group ID for each combination of bundle + sample size, by record ID (study)
  NMA_data_grpID_long_unique_withids <- NMA_data_grpID_long_unique %>% group_by(record_id) %>% mutate(group_id=row_number()) %>% ungroup()
  NMA_data_grpID_long_unique_withids
  
  ## Merge group IDs onto main data set
  ## Note that above, we created {NMA_data_grpID_long_unique_withids} which is effectively a master list of the group IDs in which each group ID is a unique combination of bundle + sample size within each record ID 
  ##   regardless of group assignment.
  ## The main data set has intervention and comparison groups in separate columns. But because the master group ID list is agnostic to group assignment, we can use it to merge on group IDs to both intervention and comparison 
  ##   groups in the following two steps:
  ##     i) once for the intervention groups; then ii) a second time for the comparison groups. 
  ## If our logic and subsequent group ID coding above are correct, the result should be each intervention and comparison group receiving a unique group ID (i.e., there should be no missing group ID values after the merges); 
  ##   AND there should be no intervention and comparison groups of the same contrast (i.e., within the same row) with the same group ID.
  ## However, as noted above, the same group (i.e., the same bundle + sample size combination) in different contrasts (rows) of the same record ID should receive the same group ID regardless of their group assignment 
  ##   across those contrasts because they are the same unique group of individuals, otherwise we do not capture all dependencies.
  
  ## Merge unique group IDs onto the intervention groups
  NMA_data_grpID_long_unique_withids_Imerge <- NMA_data_grpID_long_unique_withids %>% separate(bundle_samplesize, c("intervention_prelim","intervention_n"), "_")
  NMA_data_grpID_long_unique_withids_Imerge
  NMA_data_grpID_long_unique_withids_Imerge$intervention_n <- as.numeric(NMA_data_grpID_long_unique_withids_Imerge$intervention_n)
  NMA_data_grpID_long_unique_withids_Imerge <- NMA_data_grpID_long_unique_withids_Imerge %>% rename(group1_id = group_id)
  str(NMA_data_grpID_long_unique_withids_Imerge)
  NNMA_Data_Subset$intervention_prelim <- NNMA_Data_Subset$intervention_prelim %>% replace_na("NA") #This facilitates the merge below. When the IDs were created, intervention and intervention_n were combined then separated, which creates "NA" character values from the original true NA values, which don't match in a merge.
  NNMA_Data_Subset_grpID <- NNMA_Data_Subset %>% left_join(NMA_data_grpID_long_unique_withids_Imerge, by = c("record_id","intervention_prelim","intervention_n"))
  NNMA_Data_Subset_grpID %>% group_by(group1_id) %>% count()
  
  ## Merge unique group IDs onto the comparison groups  
  NMA_data_grpID_long_unique_withids_Cmerge <- NMA_data_grpID_long_unique_withids %>% separate(bundle_samplesize, c("comparison_prelim","comparison_n"), "_")  
  NMA_data_grpID_long_unique_withids_Cmerge
  NMA_data_grpID_long_unique_withids_Cmerge$comparison_n <- as.numeric(NMA_data_grpID_long_unique_withids_Cmerge$comparison_n)
  NMA_data_grpID_long_unique_withids_Cmerge <- NMA_data_grpID_long_unique_withids_Cmerge %>% rename(group2_id = group_id)
  str(NMA_data_grpID_long_unique_withids_Cmerge)
  NNMA_Data_Subset$comparison_prelim <- NNMA_Data_Subset$comparison_prelim %>% replace_na("NA") #This facilitates the merge below. When the IDs were created, comparison_prelim and comparison were combined then separated, which creates "NA" character values from the original true NA values, which don't match in a merge.
  NNMA_Data_Subset_grpID <- NNMA_Data_Subset_grpID %>% left_join(NMA_data_grpID_long_unique_withids_Cmerge, by = c("record_id","comparison_prelim","comparison_n"))
  NNMA_Data_Subset_grpID %>% group_by(group2_id) %>% count()
  
  ## Validate merge results
  assert_values(NNMA_Data_Subset_grpID, colnames= c("group1_id","group2_id"), test = "not_na", test_val = NA)
  assert_values(NNMA_Data_Subset_grpID, colnames= "group1_id", test="not_equal", test_val= "group2_id")
  NNMA_Data_Subset_grpID_check <- NNMA_Data_Subset_grpID %>% select(record_id, contrast_id, aggregated, measure_type, measure_name, wwc_rating, intervention_prelim, intervention_n, group1_id, comparison_prelim, comparison_n, group2_id)
  NNMA_Data_Subset_grpID_check %>% print(n = Inf) 

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

#Add moderators (as.numeric(unlist(final_domain)))
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

#Calculate Correlation Matrix
  cor(NNMA_Data_Subset_grpID[c("NL_TX", "SE_TX", "VF_TX", "F_TX", "BX_TX", "RS_TX")])
