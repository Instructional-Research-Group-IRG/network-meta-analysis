# Network meta-analysis (NMA) is a multivariate meta-regression technique for comparing multiple interventions from a network of studies 
# within a single analysis. In effect, NMAs allow for comparisons of interventions not evaluated within the same study. Because a network 
# meta-analysis accounts for all evidence, that is evidence from both direct and indirect comparisons, it produces more precise estimates 
# than those obtained from a traditional multivariate meta-regression, which only includes direct comparisons of interventions. NMA also 
# allows for the ranking of interventions.  

# Load required packages
  
  ## Install 'devel' version of metafor package
  #install.packages("remotes") 
  #remotes::install_github("wviechtb/metafor") 
  
  ## Install and load other required packages
  #install.packages("pacman")
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor)

# Load (read) data (i.e., copy data to 'dat')
  #dat <- read_sheet("https://docs.google.com/spreadsheets/d/1bWugw06yFyetIVYlhAHHzM_d3KGhegxxLBm-5463j2Q/edit#gid=0") #Test data
  NMA_data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set
  NMA_data <- subset(NMA_data, !is.na(record_id)) #There are no true rows after row 608 (not counting the headers/column names as row 1). Those after row 608 are loaded in despite having no record IDs because column C is incorrectly filled out with "FALSE" after row 608.
  
# Explore data  
  head(NMA_data)
  skim(NMA_data)
  
  ## Check for full duplicates
  dups <- anyDuplicated(NMA_data)
  assert("assert no duplicate entries", dups == 0) #No full duplicates. Data already in wide format.

  ## Review outcome measures by domain
  NMA_data %>% count(domain, measure_name) %>% print(n= Inf)
    
  ## Check ratings
  NMA_data %>% group_by(wwc_rating) %>% count() %>% ungroup()
  NMA_data %>% group_by(domain, wwc_rating) %>% count() %>% ungroup() %>% print(n= Inf)
  
# Subset data following NMA analysis specifications: "Analysis Notes for Seth 9-1-2023
  
  ## Tabulate variables upon which to subset data
  tabyl(NMA_data$aggregated)
  tabyl(NMA_data$measure_type)
  tabyl(NMA_data$wwc_rating)  
  tabyl(NMA_data$intervention_prelim)    
  tabyl(NMA_data$comparison_prelim) 
  
  ## Subset data for analysis 
  #NMA_data_analysis_subset <- NMA_data %>% filter(aggregated=="IN" & (measure_type=="Follow up (10-14 days)" | measure_type=="Main") & (wwc_rating=="MWR" | wwc_rating=="MWOR") & comparison_prelim=="BAU" & (NL...48==1 | SE...50==1 | V...53==1 | F...56==1 | BX...63==1 | RS...74==1))
  NMA_data_analysis_subset <- subset(NMA_data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") &
                               aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") &
                               comparison_prelim=="BAU" & (NL_TX==1 | SE_TX==1 | F_TX==1 | BX_TX==1 | RS_TX==1   | V_TX==1 )) #(NL...48==1 | SE...50==1 | V...53==1 | F...56==1 | BX...63==1 | RS...74==1))

  ## Retabulate variables upon which to subset data to verify correct subset
  tabyl(NMA_data_analysis_subset$aggregated)
  tabyl(NMA_data_analysis_subset$measure_type)
  tabyl(NMA_data_analysis_subset$wwc_rating)  
  tabyl(NMA_data_analysis_subset$intervention_prelim)    
  tabyl(NMA_data_analysis_subset$comparison_prelim) 
  
  #NMA_data_analysis_subset_check <- NMA_data_analysis_subset %>% select(intervention_prelim, NL...48, SE...50, V...53, F...56, BX...63, RS...74)
  #print(NMA_data_analysis_subset_check, n=Inf)
  
# Create unique group ID for each independent group within a study (record ID)
  
  ##Keep only record ID, intervention/comparison bundle, intervention/comparison sample size 
  NMA_data_grpID <- NMA_data_analysis_subset %>% select(record_id, intervention = intervention_prelim, comparison = comparison_prelim, intervention_n, comparison_n)
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
  
# Merge group IDs onto main data set
# Note that above, we created {NMA_data_grpID_long_unique_withids} which is effectively a master list of the group IDs in which each group ID is a unique combination of bundle + sample size within each record ID 
#   regardless of group assignment.
# The main data set has intervention and comparison groups in separate columns. But because the master group ID list is agnostic to group assignment, we can use it to merge on group IDs to both intervention and comparison 
#   groups in the following two steps:
#     i) once for the intervention groups; then ii) a second time for the comparison groups. 
# If our logic and subsequent group ID coding above are correct, the result should be each intervention and comparison group receiving a unique group ID (i.e., there should be no missing group ID values after the merges); 
#   AND there should be no intervention and comparison groups of the same contrast (i.e., within the same row) with the same group ID.
# However, as noted above, the same group (i.e., the same bundle + sample size combination) in different contrasts (rows) of the same record ID should receive the same group ID regardless of their group assignment 
#   across those contrasts because they are the same unique group of individuals, otherwise we do not capture all dependencies.
  
  ## Merge unique group IDs onto the intervention groups
  NMA_data_grpID_long_unique_withids_Imerge <- NMA_data_grpID_long_unique_withids %>% separate(bundle_samplesize, c("intervention_prelim","intervention_n"), "_")
  NMA_data_grpID_long_unique_withids_Imerge
  NMA_data_grpID_long_unique_withids_Imerge$intervention_n <- as.numeric(NMA_data_grpID_long_unique_withids_Imerge$intervention_n)
  NMA_data_grpID_long_unique_withids_Imerge <- NMA_data_grpID_long_unique_withids_Imerge %>% rename(group1_id = group_id)
  str(NMA_data_grpID_long_unique_withids_Imerge)
  NMA_data_analysis_subset$intervention_prelim <- NMA_data_analysis_subset$intervention_prelim %>% replace_na("NA") #This facilitates the merge below. When the IDs were created, intervention and intervention_n were combined then separated, which creates "NA" character values from the any original NA values, which don't match in a merge.
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset %>% left_join(NMA_data_grpID_long_unique_withids_Imerge, by = c("record_id","intervention_prelim","intervention_n"))
  NMA_data_analysis_subset_grpID %>% group_by(group1_id) %>% count()
  
  ## Merge unique group IDs onto the comparison groups  
  NMA_data_grpID_long_unique_withids_Cmerge <- NMA_data_grpID_long_unique_withids %>% separate(bundle_samplesize, c("comparison_prelim","comparison_n"), "_")  
  NMA_data_grpID_long_unique_withids_Cmerge
  NMA_data_grpID_long_unique_withids_Cmerge$comparison_n <- as.numeric(NMA_data_grpID_long_unique_withids_Cmerge$comparison_n)
  NMA_data_grpID_long_unique_withids_Cmerge <- NMA_data_grpID_long_unique_withids_Cmerge %>% rename(group2_id = group_id)
  str(NMA_data_grpID_long_unique_withids_Cmerge)
  NMA_data_analysis_subset$comparison_prelim <- NMA_data_analysis_subset$comparison_prelim %>% replace_na("NA") #This facilitates the merge below. When the IDs were created, comparison_prelim and comparison were combined then separated, which creates "NA" character values from the any original NA values, which don't match in a merge.
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% left_join(NMA_data_grpID_long_unique_withids_Cmerge, by = c("record_id","comparison_prelim","comparison_n"))
  NMA_data_analysis_subset_grpID %>% group_by(group2_id) %>% count()
  
  ## Validate merge results
  assert_values(NMA_data_analysis_subset_grpID, colnames= c("group1_id","group2_id"), test = "not_na", test_val = NA)
  assert_values(NMA_data_analysis_subset_grpID, colnames= "group1_id", test="not_equal", test_val= "group2_id")
  NMA_data_analysis_subset_grpID_check <- NMA_data_analysis_subset_grpID %>% select(record_id, contrast_id, aggregated, measure_type, measure_name, wwc_rating, intervention_prelim, intervention_n, group1_id, comparison_prelim, comparison_n, group2_id)
  NMA_data_analysis_subset_grpID_check %>% print(n = Inf) 

# calculate the variance-covariance matrix for multi-treatment studies
  var_covar_matrix <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID)
  var_covar_matrix 

# add contrast matrix to dataset
  NMA_data_analysis_subset_grpID <- contrmat(NMA_data_analysis_subset_grpID, grp1="group1_id", grp2="group2_id")
  NMA_data_analysis_subset_grpID
  
# execute network meta-analysis using a contrast-based random-effects model using
# BAU as the reference condition (and by setting rho=0.60, tau^2
# reflects the amount of heterogeneity for all treatment comparisons)
  class(NMA_data_analysis_subset_grpID$contrast_id) 
  NMA_data_analysis_subset_grpID$contrast_id <- as.character(NMA_data_analysis_subset_grpID$contrast_id)
  tabyl(NMA_data_analysis_subset_grpID$contrast_id)
  class(NMA_data_analysis_subset_grpID$contrast_id)
  
  class(NMA_data_analysis_subset_grpID$dosage_weekly_freq)
  NMA_data_analysis_subset_grpID$dosage_weekly_freq <- as.character(NMA_data_analysis_subset_grpID$dosage_weekly_freq)
  tabyl(NMA_data_analysis_subset_grpID$dosage_weekly_freq)
  class(NMA_data_analysis_subset_grpID$dosage_weekly_freq)
  
  ##Run preliminary standard NMA without moderators but have placeholder code below set up for moderators
  res <- rma.mv(effect_size, var_covar_matrix,
          random = ~ intervention_prelim | record_id, rho=0.60, data=NMA_data_analysis_subset_grpID)
  
  #res <- rma.mv(effect_size, var_covar_matrix, mods = ~ wwc_rating + dosage_weekly_freq - 1,
          #random = ~ contrast_id | record_id, rho=0.60, data=NMA_data_analysis_subset_grpID)
  
  res
  
  weights.rma.mv(res)
  forest(res)
  