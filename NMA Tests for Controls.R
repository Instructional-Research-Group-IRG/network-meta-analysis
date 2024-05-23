
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
library(broom)

#Primary Database 
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0")
  ##NNMA_Data <- subset(NNMA_Data, !is.na(record_id)) #There are no true rows after row 608 (not counting the headers/column names as row 1). Those after row 608 are loaded in despite having no record IDs because column C is incorrectly filled out with "FALSE" after row 608.
  
  ##This subset is specific to the meta-regression model. It is a preliminary subset that includes only non-TvsT studies.
  NNMA_Data_Subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") &
                               aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR"))
  
  ##Replace all NA values in the moderators with 0 to avoid the "Processing terminated since k <= 1" error
  NNMA_Data_Subset <- NNMA_Data_Subset %>% replace_na(list(NL_TX = 0, EX_TX = 0, VF_TX = 0, FF_TX = 0, RS_TX = 0))
  
  ##Correct variable types loaded in as lists (variables as lists cannot be used as variables in the meta-regressions below)
  convert_to_character <- function(x) {
    as.character(x)
  }
  NNMA_Data_Subset[c("group_size_category","grade_level","ongoing_training","research_lab","FWOF_TX")] <- lapply(NNMA_Data_Subset[c("group_size_category","grade_level","ongoing_training","research_lab","FWOF_TX")], convert_to_character)
  convert_to_factor <- function(x) {
    as.factor(x)
  } 
  NNMA_Data_Subset[c("group_size_category","grade_level","ongoing_training","research_lab")] <- lapply(NNMA_Data_Subset[c("group_size_category","grade_level","ongoing_training","research_lab")], convert_to_factor)
  tab_list_to_factor <- function(x) {
    tabyl(x)
  }
  lapply(NNMA_Data_Subset[c("group_size_category","grade_level","ongoing_training","research_lab")], tab_list_to_factor)
  NNMA_Data_Subset$FWOF_TX <- gsub("NULL", "0", NNMA_Data_Subset$FWOF_TX)
  convert_to_numeric <- function(x) {
    as.numeric(x)
  }
  NNMA_Data_Subset[c("FWOF_TX","grade_level","research_lab","group_size_category","ongoing_training")] <- lapply(NNMA_Data_Subset[c("FWOF_TX","grade_level","research_lab","group_size_category","ongoing_training")], convert_to_numeric)

#Create unique group ID for each independent group within a study (record ID)

  ##Keep only record ID, intervention/comparison bundle, intervention/comparison sample size 
  NNMA_Data_grpID <- NNMA_Data_Subset %>% dplyr::select(record_id, intervention = intervention_prelim, comparison = comparison_prelim, intervention_n, comparison_n)
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

  NNMA_Data_grpID <- NNMA_Data_grpID %>% dplyr::select(record_id, int_n_chr_bundle, com_n_chr_bundle)
  NNMA_Data_grpID
  
  ## Reshape long so that total number of unique groups within each record ID can be counted and assigned a unique group ID 
  NNMA_Data_grpID_long <- NNMA_Data_grpID %>% pivot_longer(-record_id, names_to="assignment", values_to="bundle_samplesize")
  NNMA_Data_grpID_long
  
  ## Keep only unique combinations of assignment bundle + group sample size within record ID so that unique group IDs can be assigned.
  ## Do this regardless of intervention/comparison group assignment because the same combination of bundle + sample size presumably should be considered one unique group regardless of group assignment.
  ## For example, one combination of bundle + sample size could have been assigned to intervention for one contrast but to comparison for another contrast of the same record ID. 
  ## In this case, this would be the same unique group of individuals in both contrasts despite the different group assignments and therefore should have the same group ID (not two different ones) to capture all dependencies.
  ## There may be no cases of this in the data but let's control for it just in case by removing bundle + group sample size duplicates within each study regardless of group assignment.
  NNMA_Data_grpID_long_unique <- NNMA_Data_grpID_long %>% group_by(record_id) %>% distinct(bundle_samplesize, .keep_all = TRUE) %>% dplyr::select(-assignment) %>% ungroup()
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
  NNMA_Data_Subset_grpID_check <- NNMA_Data_Subset_grpID %>% dplyr::select(record_id, contrast_id, aggregated, measure_type, measure_name, wwc_rating, intervention_prelim, intervention_n, group1_id, comparison_prelim, comparison_n, group2_id)
  NNMA_Data_Subset_grpID_check %>% print(n = Inf) 
  
  ## Restore "NA" (non-missing) values to their true <NA> (missing) values because the unite then separate functions used above changed the values from <NA> to "NA"
  tabyl(NNMA_Data_Subset_grpID$intervention_prelim)
  tabyl(NNMA_Data_Subset_grpID$comparison_prelim)
  NNMA_Data_Subset_grpID <- NNMA_Data_Subset_grpID %>% replace_with_na_at(.vars = c("intervention_prelim","comparison_prelim"), condition = ~.x %in% common_na_strings)
  tabyl(NNMA_Data_Subset_grpID$intervention_prelim)
  tabyl(NNMA_Data_Subset_grpID$comparison_prelim)

#Create covariance matrix
V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NNMA_Data_Subset_grpID)

##Calculate Correlation Matrix (i.e., bivariate correlations among moderator variables and instructional components)
var_class <- function(x) {
  class(x)
}
lapply(NNMA_Data_Subset_grpID[c("NL_TX", "TES_TX", "VF_TX", "RS_TX", "FF_TX...62", "N_TX", "SEO_TX", "TV_TX", "RV_TX", "FWOF_TX")], var_class)
lapply(NNMA_Data_Subset_grpID[c("publication_year", "domain", "control_nature", "dosage_overall_hours_avg", "group_size_category", "grade_level")], var_class)
lapply(NNMA_Data_Subset_grpID[c("measure_developer", "interventionist", "ongoing_training", "research_lab", "intervention_content")], var_class)

# cor(NNMA_Data_Subset_grpID[c("NL_TX", "TES_TX", "VF_TX", "RS_TX", "FF_TX", "N_TX", "SEO_TX", "TV_TX", "RV_TX", "FWOF_TX",
#                              "publication_year", "domain", "control_nature", "dosage_overall_hours_avg", "group_size_category", "grade_level", 
#                              "measure_developer", "interventionist", "ongoing_training", "research_lab", "intervention_content")])


cor(NNMA_Data_Subset_grpID[c("NL_TX", "TES_TX", "VF_TX", "RS_TX", "FF_TX...62", "N_TX", "SEO_TX", "TV_TX", "RV_TX", "FWOF_TX",
                             "publication_year", "dosage_overall_hours_avg", "group_size_category", "grade_level",
                             "ongoing_training", "research_lab")])

#Run meta-regressions

####################################################################################################

  NNMA_control_dosage_overall_hours_avg <- rma.mv(yi = effect_size, 
                                  V = V_list, 
                                  random = ~ 1 | record_id/es_id,
                                  mods = ~ dosage_overall_hours_avg - 1,
                                  test =  "t", 
                                  data = NNMA_Data_Subset_grpID, 
                                  method = "REML")
  summary(NNMA_control_dosage_overall_hours_avg)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_dosage_overall_hours_avg,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf

####################################################################################################

  NNMA_control_group_size_category <- rma.mv(yi = effect_size, 
                                                  V = V_list, 
                                                  random = ~ 1 | record_id/es_id,
                                                  mods = ~ group_size_category - 1,
                                                  test =  "t", 
                                                  data = NNMA_Data_Subset_grpID, 
                                                  method = "REML")
  summary(NNMA_control_group_size_category)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_group_size_category,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf

####################################################################################################

  NNMA_control_grade_level <- rma.mv(yi = effect_size, 
                                             V = V_list, 
                                             random = ~ 1 | record_id/es_id,
                                             mods = ~ grade_level - 1,
                                             test =  "t", 
                                             data = NNMA_Data_Subset_grpID, 
                                             method = "REML")
  summary(NNMA_control_grade_level)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_grade_level,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf

####################################################################################################

  NNMA_control_measure_developer <- rma.mv(yi = effect_size, 
                                     V = V_list, 
                                     random = ~ 1 | record_id/es_id,
                                     mods = ~ measure_developer - 1,
                                     test =  "t", 
                                     data = NNMA_Data_Subset_grpID, 
                                     method = "REML")
  summary(NNMA_control_measure_developer)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_measure_developer,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf

####################################################################################################

  NNMA_control_interventionist <- rma.mv(yi = effect_size, 
                                           V = V_list, 
                                           random = ~ 1 | record_id/es_id,
                                           mods = ~ interventionist - 1,
                                           test =  "t", 
                                           data = NNMA_Data_Subset_grpID, 
                                           method = "REML")
  summary(NNMA_control_interventionist)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_interventionist,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf

####################################################################################################

  NNMA_control_year <- rma.mv(yi = effect_size, 
                                         V = V_list, 
                                         random = ~ 1 | record_id/es_id,
                                         mods = ~ publication_year - 1,
                                         test =  "t", 
                                         data = NNMA_Data_Subset_grpID, 
                                         method = "REML")
  summary(NNMA_control_year)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_year,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf

####################################################################################################

  NNMA_control_ongoing_training <- rma.mv(yi = effect_size, 
                              V = V_list, 
                              random = ~ 1 | record_id/es_id,
                              mods = ~ ongoing_training - 1,
                              test =  "t", 
                              data = NNMA_Data_Subset_grpID, 
                              method = "REML")
  summary(NNMA_control_ongoing_training)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_ongoing_training,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf

####################################################################################################

  NNMA_control_research_lab <- rma.mv(yi = effect_size, 
                                          V = V_list, 
                                          random = ~ 1 | record_id/es_id,
                                          mods = ~ research_lab - 1,
                                          test =  "t", 
                                          data = NNMA_Data_Subset_grpID, 
                                          method = "REML")
  summary(NNMA_control_research_lab)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_research_lab,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf

####################################################################################################
  
  NNMA_control_domain <- rma.mv(yi = effect_size, 
                                      V = V_list, 
                                      random = ~ 1 | record_id/es_id,
                                      mods = ~ domain - 1,
                                      test =  "t", 
                                      data = NNMA_Data_Subset_grpID, 
                                      method = "REML")
  summary(NNMA_control_domain)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_domain,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf  
  
####################################################################################################
  
  NNMA_control_control_nature <- rma.mv(yi = effect_size, 
                                      V = V_list, 
                                      random = ~ 1 | record_id/es_id,
                                      mods = ~ control_nature - 1,
                                      test =  "t", 
                                      data = NNMA_Data_Subset_grpID, 
                                      method = "REML")
  summary(NNMA_control_control_nature)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_control_nature,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf  
  
####################################################################################################
  
  NNMA_control_intervention_content <- rma.mv(yi = effect_size, 
                                      V = V_list, 
                                      random = ~ 1 | record_id/es_id,
                                      mods = ~ intervention_content - 1,
                                      test =  "t", 
                                      data = NNMA_Data_Subset_grpID, 
                                      method = "REML")
  summary(NNMA_control_intervention_content)  
  
  ##Use RVE for robustness
  mvcf <- coef_test(NNMA_control_intervention_content,
                    cluster = NNMA_Data_Subset_grpID$record_id, 
                    vcov = "CR2")
  mvcf  
 
# Benjamini-Hochvery adjustment   
  ## example of Benjamini-Hochvery adjustment for multiple comparisons
  ## this assumes you have a dataframe with the results of each moderator analysis at each level of
  ## all included moderators
  
  NNMA_control_dosage_overall_hours_avg_df <- tidy(NNMA_control_dosage_overall_hours_avg, conf.int = TRUE)
  NNMA_control_dosage_overall_hours_avg_df
  NNMA_control_dosage_overall_hours_avg_df$term <- gsub("overall", "dosage_overall_hours_avg", NNMA_control_dosage_overall_hours_avg_df$term)
  NNMA_control_dosage_overall_hours_avg_df
  
  NNMA_control_group_size_category_df <- tidy(NNMA_control_group_size_category, conf.int = TRUE)
  NNMA_control_group_size_category_df
  NNMA_control_group_size_category_df$term <- gsub("overall", "group_size_category", NNMA_control_group_size_category_df$term)
  NNMA_control_group_size_category_df  
  
  NNMA_control_grade_level_df <- tidy(NNMA_control_grade_level, conf.int = TRUE)
  NNMA_control_grade_level_df
  NNMA_control_grade_level_df$term <- gsub("overall", "grade_level", NNMA_control_grade_level_df$term)
  NNMA_control_grade_level_df    
  
  NNMA_control_measure_developer_df <- tidy(NNMA_control_measure_developer, conf.int = TRUE)
  NNMA_control_measure_developer_df
  
  NNMA_control_interventionist_df <- tidy(NNMA_control_interventionist, conf.int = TRUE)
  NNMA_control_interventionist_df
  
  NNMA_control_year_df <- tidy(NNMA_control_year, conf.int = TRUE)
  NNMA_control_year_df
  NNMA_control_year_df$term <- gsub("overall", "control_year", NNMA_control_year_df$term)
  NNMA_control_year_df  
  
  NNMA_control_ongoing_training_df <- tidy(NNMA_control_ongoing_training, conf.int = TRUE)
  NNMA_control_ongoing_training_df
  NNMA_control_ongoing_training_df$term <- gsub("overall", "ongoing_training", NNMA_control_ongoing_training_df$term)
  NNMA_control_ongoing_training_df    
  
  NNMA_control_research_lab_df <- tidy(NNMA_control_research_lab, conf.int = TRUE)  
  NNMA_control_research_lab_df
  NNMA_control_research_lab_df$term <- gsub("overall", "research_lab", NNMA_control_research_lab_df$term)
  NNMA_control_research_lab_df 
  
  NNMA_control_domain_df <- tidy(NNMA_control_domain, conf.int = TRUE)  
  NNMA_control_domain_df  
  
  NNMA_control_control_nature_df <- tidy(NNMA_control_control_nature, conf.int = TRUE)  
  NNMA_control_control_nature_df  
  
  NNMA_control_intervention_content_df <- tidy(NNMA_control_intervention_content, conf.int = TRUE)  
  NNMA_control_intervention_content_df
  
  mod_analysis <- rbind(NNMA_control_dosage_overall_hours_avg_df, NNMA_control_group_size_category_df, NNMA_control_grade_level_df, NNMA_control_measure_developer_df, NNMA_control_interventionist_df, NNMA_control_year_df, NNMA_control_ongoing_training_df, NNMA_control_research_lab_df, NNMA_control_domain_df, NNMA_control_control_nature_df, NNMA_control_intervention_content_df)
  mod_analysis
  
  mod_analysis_BHcorrection <- mod_analysis %>% 
    # this numbers each row, thus allowing you to determine how many tests you ran, for categorical
    # model this should reflecct one test for each level of the categorical model
    rowid_to_column(var = "order_id") %>% 
    ## filter if needed
    ## group_by(domain) %>% ## only use if you have multiple outcome domains, otherwise remove
    ## arrange(domain, p) %>% ## only arrange by p (p-value) if no multiple outcome domains
    arrange(p.value) %>% ## only arrange by p (p-value) if no multiple outcome domains
    mutate(rank = seq_along(p.value),
           BH_val = (rank/length(rank)*.10), # specifies BH corrected level
           p_less_BH = case_when(p.value < BH_val ~ "Yes",
                                 p.value >= BH_val ~ "No"),
           BH_sig = if_else(cumall(p_less_BH == "No"), "", if_else(p.value <= max(p.value[p_less_BH == "Yes"]), "*", ""))) %>% 
    ungroup() %>% 
    arrange(order_id)
  # here you can select your final dataframe
  # select(domain:moderator, df_4, coef, p, BH_sig, ES, CI_LB:CI_UB, k, nES, Comments) 
  
  print(mod_analysis_BHcorrection, n=Inf)
