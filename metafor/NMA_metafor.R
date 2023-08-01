# This is just a special case of the multivariate model

# Install 'devel' version of metafor package
  #install.packages("remotes")
  #remotes::install_github("wviechtb/metafor")

# Load required packages
  #install.packages("pacman")
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr)

# Load (read) data (i.e., copy data to 'dat')
  dat <- read_sheet("https://docs.google.com/spreadsheets/d/1bWugw06yFyetIVYlhAHHzM_d3KGhegxxLBm-5463j2Q/edit#gid=0")
  #dat <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0")
  
# Explore data  
  head(dat)
  skim(dat)
  
  ## Check for full duplicates
  dups <- anyDuplicated(dat)
  assert("assert no duplicate entries", dups == 0) #No full duplicates. Data already in wide format.

  ## Review outcome measures by domain
  dat %>% count(domain, measure_name)
    
  ## Check ratings
  dat %>% group_by(wwc_rating) %>% count() %>% ungroup()
  dat %>% group_by(domain, wwc_rating) %>% count() %>% ungroup()

  ratings_dnms <- dat %>% filter(wwc_rating == "dnm")
  ratings_dnms 
  dat_nodnm <- dat %>% filter(wwc_rating != "dnm") #Drop measure entries rated DNM
  dat_nodnm %>% group_by(wwc_rating) %>% count()
  
# Create unique group ID for each independent group within a study (record ID)
  
  ##Keep only record ID, intervention/comparison bundle, intervention/comparison sample size 
  dat_nodnm_grp <- dat_nodnm %>% select(record_id, intervention, comparison, intervention_n, comparison_n)
  
  ## For each assignment group, combine component bundle and sample size into a single character variable to distinguish groups with the same sample sizes but different component bundles. 
  ## In such cases, these are presumably two unique groups of individuals because they received different component bundles despite happening to have the same sample sizes. 
  ## There may be no cases within the same record ID in which two different component bundles happen to have the same group sample sizes but let's control for that just in case.
  str(dat_nodnm_grp)
  dat_nodnm_grp <- dat_nodnm_grp %>% mutate(intervention_n_chr = as.character(intervention_n))
  dat_nodnm_grp <- dat_nodnm_grp %>% unite("int_n_chr_bundle" , c(intervention, intervention_n_chr), remove = FALSE)
  dat_nodnm_grp <- dat_nodnm_grp %>% mutate(comparison_n_chr = as.character(comparison_n))
  dat_nodnm_grp <- dat_nodnm_grp %>% unite("com_n_chr_bundle" , c(comparison,comparison_n_chr), remove = FALSE)
  str(dat_nodnm_grp)
  dat_nodnm_grp
  
  dat_nodnm_grp <- dat_nodnm_grp %>% select(record_id, int_n_chr_bundle, com_n_chr_bundle)
  dat_nodnm_grp
  
  ## Reshape long so that total number of unique groups within each record ID can be counted and assigned a unique group ID 
  dat_nodnm_grp_long <- dat_nodnm_grp %>% pivot_longer(-record_id, names_to="assignment", values_to="bundle_samplesize")
  dat_nodnm_grp_long
  
  ## Keep only unique combinations of assignment bundle + group sample size within record ID so that unique group IDs can be assigned.
  ## Do this regardless of intervention/comparison group assignment because the same combination of bundle + sample size presumably should be considered one unique group regardless of group assignment.
  ## For example, one combination of bundle + sample size could have been assigned to intervention for one contrast but to comparison for another contrast of the same record ID. 
  ## In this case, this would be the same unique group of individuals in both contrasts despite the different group assignments and therefore should have the same group ID (not two different ones) to capture all dependencies.
  ## There may be no cases of this in the data but let's control for it just in case by removing bundle + group sample size duplicates within each study regardless of group assignment.
  dat_nodnm_grp_long_unique <- dat_nodnm_grp_long %>% group_by(record_id) %>% distinct(bundle_samplesize, .keep_all = TRUE) %>% select(-assignment) %>% ungroup()
  dat_nodnm_grp_long_unique

  ##Create unique group ID for each combination of bundle + sample size, by record ID (study)
  dat_nodnm_grp_long_unique_withids <- dat_nodnm_grp_long_unique %>% group_by(record_id) %>% mutate(group_id=row_number()) %>% ungroup()
  dat_nodnm_grp_long_unique_withids
  
# Merge group IDs onto main data set
# Note that above, we created {dat_nodnm_grp_long_unique_withids} which is effectively a master list of the group IDs in which each group ID is a unique combination of bundle + sample size within each record ID 
#   regardless of group assignment.
# The main data set has intervention and comparison groups in separate columns. But because the master group ID list is agnostic to group assignment, we can use it to merge on group IDs to both intervention and comparison 
#   groups in the following two steps:
#     i) once for the intervention groups; then ii) a second time for the comparison groups. 
# If our logic and subsequent group ID coding above are correct, the result should be each intervention and comparison group receiving a unique group ID (i.e., there should be no missing group ID values after the merges); 
#   AND there should be no intervention and comparison groups of the same contrast (i.e., within the same row) with the same group ID.
# However, as noted above, the same group (i.e., the same bundle + sample size combination) in different contrasts (rows) of the same record ID should receive the same group ID regardless of their group assignment 
#   across those contrasts because they are the same unique group of individuals, otherwise we do not capture all dependencies.
  
  ## Merge unique group IDs onto the intervention groups
  dat_nodnm_grp_long_unique_withids_Imerge <- dat_nodnm_grp_long_unique_withids %>% separate(bundle_samplesize, c("intervention","intervention_n"), "_")
  dat_nodnm_grp_long_unique_withids_Imerge
  dat_nodnm_grp_long_unique_withids_Imerge$intervention_n <- as.numeric(dat_nodnm_grp_long_unique_withids_Imerge$intervention_n)
  dat_nodnm_grp_long_unique_withids_Imerge <- dat_nodnm_grp_long_unique_withids_Imerge %>% rename(group1_id = group_id)
  str(dat_nodnm_grp_long_unique_withids_Imerge)
  dat_nodnm_grp2 <- dat_nodnm %>% left_join(dat_nodnm_grp_long_unique_withids_Imerge, by = c("record_id","intervention","intervention_n"))

  ## Merge unique group IDs onto the comparison groups  
  dat_nodnm_grp_long_unique_withids_Cmerge <- dat_nodnm_grp_long_unique_withids %>% separate(bundle_samplesize, c("comparison","comparison_n"), "_")  
  dat_nodnm_grp_long_unique_withids_Cmerge
  dat_nodnm_grp_long_unique_withids_Cmerge$comparison_n <- as.numeric(dat_nodnm_grp_long_unique_withids_Cmerge$comparison_n)
  dat_nodnm_grp_long_unique_withids_Cmerge <- dat_nodnm_grp_long_unique_withids_Cmerge %>% rename(group2_id = group_id)
  str(dat_nodnm_grp_long_unique_withids_Cmerge)
  dat_nodnm_grp2 <- dat_nodnm_grp2 %>% left_join(dat_nodnm_grp_long_unique_withids_Cmerge, by = c("record_id","comparison","comparison_n"))
 
  ## Validate merge results
  assert_values(dat_nodnm_grp2, colnames= c("group1_id","group2_id"), test = "not_na", test_val = NA)
  assert_values(dat_nodnm_grp2, colnames= "group1_id", test="not_equal", test_val= "group2_id")
  dat_nodnm_grp2_check <- dat_nodnm_grp2 %>% select(record_id, contrast_id, contrast, measure_name, wwc_rating, intervention_n, comparison_n, group1_id, group2_id)
  dat_nodnm_grp2_check %>% print(n = Inf) 

# calculate the variance-covariance matrix for multi-treatment studies
  var_covar_matrix <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=dat_nodnm_grp2)
  var_covar_matrix 

# add contrast matrix to dataset
  dat_nodnm_grp2 <- contrmat(dat_nodnm_grp2, grp1="intervention", grp2="comparison")
  #dat_nodnm_grp2 <- contrmat(dat_nodnm_grp2, grp1="group1_id", grp2="group2_id") #(?) Should we also use group ID instead of intervention/comparison? Though note results are the same but that may not be the case with the full
  dat_nodnm_grp2
  
# network meta-analysis using a contrast-based random-effects model using
# BAU as the reference condition (and by setting rho=0.60, tau^2
# reflects the amount of heterogeneity for all treatment comparisons)
  res <- rma.mv(effect_size, var_covar_matrix, mods = ~ wwc_rating + dosage - 1,
                random = ~ contrast | record_id, rho=0.60, data=dat_nodnm_grp2)
  res