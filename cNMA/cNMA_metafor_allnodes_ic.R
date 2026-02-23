# This script performs a component network meta-analysis (cNMA) of mathematics education interventions using outcomes in the whole and ration numbers domains. 
# Sample: all nodes
# Variable for defining outcome domain: intervention_content
# Disaggregated by domain: Yes

# Load required packages

  ## Install 'devel' version of metafor package
  ##install.packages("remotes") 
  ##remotes::install_github("wviechtb/metafor") 
  
  ## Install and load other required packages
  ##install.packages("pacman") 
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor, naniar, igraph, multcomp, broom, gridExtra, ggplot2, writexl, readr, grid, gridExtra, cowplot, extrafont)
  
  # Load (read) data (i.e., copy data to 'dat')
  #dat <- read_sheet("https://docs.google.com/spreadsheets/d/1bWugw06yFyetIVYlhAHHzM_d3KGhegxxLBm-5463j2Q/edit#gid=0") #Test data
  #NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1ayNoKwbxnUVa1XspqAWpS2YBFIwApvS5t3lld5Kx2VA/edit?gid=0#gid=0", sheet="Master Database") # <<Copy of NNMA Master Database - March 17, 11:18 AM>>; The master database changed after submission of publication in late March 2025. This recovered older version from 3/17/25 reproduces the results reported in the publication.
  
  ## Explore data  
  NNMA_Data %>% count() 
  head(NNMA_Data)
  skim(NNMA_Data)
  NNMA_Data$contrast_id <- as.character(NNMA_Data$contrast_id)
  NNMA_Data %>% count(record_id, contrast_id) %>% print(n= Inf)
  NNMA_Data %>% count(domain, measure_name) %>% print(n= Inf)
  
  ## Check ratings
  NNMA_Data %>% group_by(wwc_rating) %>% count() %>% ungroup()
  NNMA_Data %>% group_by(domain, wwc_rating) %>% count() %>% ungroup() %>% print(n= Inf)
  
# Subset data following NMA analysis specifications
  
  ## Tabulate variables upon which to subset data
  tabyl(NNMA_Data$aggregated)
  tabyl(NNMA_Data$measure_type)
  tabyl(NNMA_Data$wwc_rating)  
  tabyl(NNMA_Data$intervention_prelim)    
  tabyl(NNMA_Data$comparison_prelim) 
  
  ## Subset data for analysis 
  NMA_data_analysis_subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") & aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") & (TvsT==1 | TvsT==0))
  NMA_data_analysis_subset %>% count()
  
  ## Retabulate variables upon which to subset data to verify correct subset
  tabyl(NMA_data_analysis_subset$aggregated)
  tabyl(NMA_data_analysis_subset$measure_type)
  tabyl(NMA_data_analysis_subset$wwc_rating)  
  tabyl(NMA_data_analysis_subset$intervention_prelim)    
  tabyl(NMA_data_analysis_subset$comparison_prelim)
  
  ## Check for full duplicates
  dups <- anyDuplicated(NMA_data_analysis_subset)
  assert("assert no duplicate entries", dups == 0) #No full duplicates. Data already in wide format.
  
# Create unique group ID for each independent group of students included in either assignment group of the study-contrasts
  
  ## Keep only record ID, intervention/comparison bundle, intervention/comparison sample size 
  NMA_data_grpID <- NMA_data_analysis_subset %>% dplyr::select(record_id, intervention = intervention_prelim, comparison = comparison_prelim, intervention_n, comparison_n)
  str(NMA_data_grpID)
  
  ## For each assignment group, combine component bundle and sample size into a single character variable to distinguish groups with the same sample sizes but different component bundles. 
  ## In such cases, these are presumably two unique groups of individuals because they received different component bundles despite happening to have the same sample sizes. 
  ## There may be no cases within the same record ID in which two different component bundles happen to have the same group sample sizes but let's control for that just in case.
  str(NMA_data_grpID)
  NMA_data_grpID <- NMA_data_grpID %>% mutate(intervention_n_chr = as.character(intervention_n))
  NMA_data_grpID <- NMA_data_grpID %>% unite("int_n_chr_bundle" , c(intervention, intervention_n_chr), remove = FALSE)
  NMA_data_grpID <- NMA_data_grpID %>% mutate(comparison_n_chr = as.character(comparison_n))
  NMA_data_grpID <- NMA_data_grpID %>% unite("com_n_chr_bundle" , c(comparison, comparison_n_chr), remove = FALSE)
  str(NMA_data_grpID)
  NMA_data_grpID
  
  NMA_data_grpID <- NMA_data_grpID %>% dplyr::select(record_id, int_n_chr_bundle, com_n_chr_bundle)
  NMA_data_grpID
  
  ## Reshape long so that total number of unique groups within each record ID can be counted and assigned a unique group ID 
  NMA_data_grpID_long <- NMA_data_grpID %>% pivot_longer(-record_id, names_to="assignment", values_to="bundle_samplesize")
  NMA_data_grpID_long
  
  ## Keep only unique combinations of assignment bundle + group sample size within record ID so that unique group IDs can be assigned.
  ## Do this regardless of intervention/comparison group assignment because the same combination of bundle + sample size presumably should be considered one unique group regardless of group assignment.
  ## For example, one combination of bundle + sample size could have been assigned to intervention for one contrast but to comparison for another contrast of the same record ID. 
  ## In this case, this would be the same unique group of individuals in both contrasts despite the different group assignments and therefore should have the same group ID (not two different ones) to capture all dependencies.
  ## There may be no cases of this in the data but let's control for it just in case by removing bundle + group sample size duplicates within each study regardless of group assignment.
  NMA_data_grpID_long_unique <- NMA_data_grpID_long %>% group_by(record_id) %>% distinct(bundle_samplesize, .keep_all = TRUE) %>% dplyr::select(-assignment) %>% ungroup()
  NMA_data_grpID_long_unique
  
  ## Create unique group ID for each combination of bundle + sample size, by record ID (study)
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
  ## However, as noted above, the same group (i.e., the same bundle + sample size combination) in different contrasts (roRS) of the same record ID should receive the same group ID regardless of their group assignment 
  ##   across those contrasts because they are the same unique group of individuals, otherwise we do not capture all dependencies.
  
  ## Merge unique group IDs onto the intervention groups
  NMA_data_grpID_long_unique_withids_Imerge <- NMA_data_grpID_long_unique_withids %>% separate(bundle_samplesize, c("intervention_prelim","intervention_n"), "_")
  NMA_data_grpID_long_unique_withids_Imerge
  NMA_data_grpID_long_unique_withids_Imerge$intervention_n <- as.numeric(NMA_data_grpID_long_unique_withids_Imerge$intervention_n)
  NMA_data_grpID_long_unique_withids_Imerge <- NMA_data_grpID_long_unique_withids_Imerge %>% rename(group1_id = group_id)
  str(NMA_data_grpID_long_unique_withids_Imerge)
  NMA_data_analysis_subset_chrNA <- NMA_data_analysis_subset
  NMA_data_analysis_subset_chrNA$intervention_prelim <- NMA_data_analysis_subset$intervention_prelim %>% replace_na("NA") #This facilitates the merge below. When the IDs were created, intervention and intervention_n were combined then separated, which creates "NA" character values from the original true NA values, which don't match in a merge.
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_chrNA %>% left_join(NMA_data_grpID_long_unique_withids_Imerge, by = c("record_id","intervention_prelim","intervention_n"))
  NMA_data_analysis_subset_grpID %>% group_by(group1_id) %>% count()
  
  ## Merge unique group IDs onto the comparison groups  
  NMA_data_grpID_long_unique_withids_Cmerge <- NMA_data_grpID_long_unique_withids %>% separate(bundle_samplesize, c("comparison_prelim","comparison_n"), "_")  
  NMA_data_grpID_long_unique_withids_Cmerge
  NMA_data_grpID_long_unique_withids_Cmerge$comparison_n <- as.numeric(NMA_data_grpID_long_unique_withids_Cmerge$comparison_n)
  NMA_data_grpID_long_unique_withids_Cmerge <- NMA_data_grpID_long_unique_withids_Cmerge %>% rename(group2_id = group_id)
  str(NMA_data_grpID_long_unique_withids_Cmerge)
  NMA_data_analysis_subset_grpID$comparison_prelim <- NMA_data_analysis_subset_grpID$comparison_prelim %>% replace_na("NA") #This facilitates the merge below. When the IDs were created, intervention and intervention_n were combined then separated, which creates "NA" character values from the original true NA values, which don't match in a merge.
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% left_join(NMA_data_grpID_long_unique_withids_Cmerge, by = c("record_id","comparison_prelim","comparison_n"))
  NMA_data_analysis_subset_grpID %>% group_by(group2_id) %>% count()
  
  ## Validate merge results
  assert_values(NMA_data_analysis_subset_grpID, colnames= c("group1_id","group2_id"), test = "not_na", test_val = NA)
  assert_values(NMA_data_analysis_subset_grpID, colnames= "group1_id", test="not_equal", test_val= "group2_id")
  NMA_data_analysis_subset_grpID_check <- NMA_data_analysis_subset_grpID %>% dplyr::select(record_id, contrast_id, aggregated, measure_type, measure_name, wwc_rating, intervention_prelim, intervention_n, group1_id, comparison_prelim, comparison_n, group2_id)
  NMA_data_analysis_subset_grpID_check %>% print(n = Inf) 
  
  ## Restore "NA" (non-missing) values to their true <NA> (missing) values because the unite then separate functions used above changed the values from <NA> to "NA"
  tabyl(NMA_data_analysis_subset_grpID$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID$comparison_prelim)
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% replace_with_na_at(.vars = c("intervention_prelim","comparison_prelim"), condition = ~.x %in% common_na_strings)
  tabyl(NMA_data_analysis_subset_grpID$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID$comparison_prelim)
  NMA_data_analysis_subset_grpID %>% count()

# Additional modifications to NMA subset analysis data for running NMA with metafor  
  
  ## Convert variables to their intended types 
  convert_to_character <- function(x) {
    as.character(x)
  }
  NMA_data_analysis_subset_grpID[c("group_size_category","ongoing_training","research_lab","dosage_weekly_freq","grade_level")] <- lapply(NMA_data_analysis_subset_grpID[c("group_size_category","ongoing_training","research_lab","dosage_weekly_freq","grade_level")], convert_to_character)
  
  convert_to_factor <- function(x) {
    as.factor(x)
  }  
  NMA_data_analysis_subset_grpID[c("group_size_category","ongoing_training","research_lab","dosage_weekly_freq","intervention_prelim","comparison_prelim")] <- lapply(NMA_data_analysis_subset_grpID[c("group_size_category","ongoing_training","research_lab","dosage_weekly_freq","intervention_prelim","comparison_prelim")], convert_to_factor)
  NMA_data_analysis_subset_grpID[c("domain_numeric","control_nature_numeric","measure_developer_numeric","interventionist_numeric","TvsT")] <- lapply(NMA_data_analysis_subset_grpID[c("domain_numeric","control_nature_numeric","measure_developer_numeric","interventionist_numeric","TvsT")], convert_to_factor)
  
  NMA_data_analysis_subset_grpID$group_size_average <- as.character(NMA_data_analysis_subset_grpID$group_size_average)
  NMA_data_analysis_subset_grpID$group_size_average <- as.numeric(NMA_data_analysis_subset_grpID$group_size_average)
  
  NMA_data_analysis_subset_grpID$grade_level <- as.numeric(NMA_data_analysis_subset_grpID$grade_level)
  
  ## Correct domain names
  # tabyl(NMA_data_analysis_subset_grpID$domain)
  # NMA_data_analysis_subset_grpID$domain <- gsub("Rational Numbers", "Rational Number", NMA_data_analysis_subset_grpID$domain)
  # NMA_data_analysis_subset_grpID$domain <- gsub("Rational Number", "Rational Numbers", NMA_data_analysis_subset_grpID$domain)
  # NMA_data_analysis_subset_grpID$domain <- gsub("Whole Numbers", "Whole Number", NMA_data_analysis_subset_grpID$domain)
  # NMA_data_analysis_subset_grpID$domain <- gsub("Whole Number", "Whole Numbers", NMA_data_analysis_subset_grpID$domain)
  # tabyl(NMA_data_analysis_subset_grpID$domain)
  # class(NMA_data_analysis_subset_grpID$domain)
 
  ## Correct bundle acronym/code
  NMA_data_analysis_subset_grpID %>% count()
  class(NMA_data_analysis_subset_grpID$intervention_prelim)
  class(NMA_data_analysis_subset_grpID$comparison_prelim)
  NMA_data_analysis_subset_grpID$intervention_prelim <- as.character(NMA_data_analysis_subset_grpID$intervention_prelim)
  NMA_data_analysis_subset_grpID$comparison_prelim <- as.character(NMA_data_analysis_subset_grpID$comparison_prelim)
  tabyl(NMA_data_analysis_subset_grpID$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID$comparison_prelim)
  NMA_data_analysis_subset_grpID$intervention_prelim <- gsub("TES", "SE", NMA_data_analysis_subset_grpID$intervention_prelim)
  NMA_data_analysis_subset_grpID$comparison_prelim <- gsub("TES", "SE", NMA_data_analysis_subset_grpID$comparison_prelim)
  tabyl(NMA_data_analysis_subset_grpID$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID$comparison_prelim)  
  
  ## Drop intervention versus comparison contrasts that have the same bundles
  tabyl(NMA_data_analysis_subset_grpID$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID$comparison_prelim)
  NMA_data_analysis_subset_grpID %>% count()
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% filter(intervention_prelim!=comparison_prelim) #This also removes any roRS with <NA> values in columns intervention_prelim & comparison_prelim.
  tabyl(NMA_data_analysis_subset_grpID$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID$comparison_prelim)  
  NMA_data_analysis_subset_grpID %>% count()
  tabyl(NMA_data_analysis_subset_grpID$domain)
  NMA_data_analysis_subset_grpID$intervention_prelim <- as.factor(NMA_data_analysis_subset_grpID$intervention_prelim)
  NMA_data_analysis_subset_grpID$comparison_prelim <- as.factor(NMA_data_analysis_subset_grpID$comparison_prelim)
  class(NMA_data_analysis_subset_grpID$intervention_prelim)
  class(NMA_data_analysis_subset_grpID$comparison_prelim)  
  
  ## Drop roRS with missing values in the intervention and comparison columns (i.e., <NA>).
  NMA_data_analysis_subset_grpID %>% count()
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% drop_na(c(intervention_prelim, comparison_prelim)) 
  NMA_data_analysis_subset_grpID %>% count()
  
  ## Correct variable names
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% rename(contrast_name= contrast_name...15)
  # tabyl(NMA_data_analysis_subset_grpID$intervention_content...33)
  # tabyl(NMA_data_analysis_subset_grpID$intervention_content...36)
  # NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% rename(intervention_content= intervention_content...36)

  ## Check counts of final NMA analysis file
    
    ### NUmber of domains
    tabyl(NMA_data_analysis_subset_grpID$domain)
  
    ### Number of effect sizes
    NMA_data_analysis_subset_grpID %>% count()
    tabyl(NMA_data_analysis_subset_grpID$es_id)
    
    ### Number of contrasts
    NMA_data_analysis_subset_grpID_c <- NMA_data_analysis_subset_grpID %>% distinct(contrast_id, .keep_all = TRUE)
    NMA_data_analysis_subset_grpID_c %>% count()
    tabyl(NMA_data_analysis_subset_grpID_c$contrast_id)
    NMA_study_contrast_list <- NMA_data_analysis_subset_grpID_c %>% dplyr::select(record_id, "Abbreviated Citation", contrast_id, contrast_name) #Create list of study-contrasts included in NMA.
    print(NMA_study_contrast_list, n= Inf)
    #write_csv(NMA_study_contrast_list, 'NMA_study_contrast_list_all_nodes.csv')
    
    ### Number of studies
    NMA_data_analysis_subset_grpID_s <- NMA_data_analysis_subset_grpID %>% distinct(record_id, .keep_all = TRUE)
    NMA_data_analysis_subset_grpID_s %>% count()
    tabyl(NMA_data_analysis_subset_grpID_s$record_id)
     
# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Rhole Numbers (R)"
      
  ## Subset analysis data frame further to just the Whole Numbers (W) intervention content (icW)
  tabyl(NMA_data_analysis_subset_grpID$intervention_content)
  NMA_data_analysis_subset_grpID_icW <- NMA_data_analysis_subset_grpID %>% filter(intervention_content == "W")
  tabyl(NMA_data_analysis_subset_grpID_icW$intervention_content)
  NMA_data_analysis_subset_grpID_icW_c <- NMA_data_analysis_subset_grpID_icW %>% distinct(contrast_id, .keep_all = TRUE)
  NMA_data_analysis_subset_grpID_icW_c %>% count()
  
  ## Add contrast matrix to dataset
  NMA_data_analysis_subset_grpID_icW <- contrmat(NMA_data_analysis_subset_grpID_icW, grp1="intervention_prelim", grp2="comparison_prelim")
  str(NMA_data_analysis_subset_grpID_icW)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_icW)
  V_list    
  V_list_icW <- data.frame(V_list)
  #write_csv(V_list_icW, 'V_list_icW.csv')
        
  ## Run additive cNMA with the unique intervention components as moderators  
  tabyl(NMA_data_analysis_subset_grpID_icW$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID_icW$comparison_prelim)
  check_icW <- NMA_data_analysis_subset_grpID_icW %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim)
  print(check_icW)
  
    ### Prepare component binaries for cNMA
    cNMA_data_analysis_subset_grpID_icW <- NMA_data_analysis_subset_grpID_icW
    tabyl(cNMA_data_analysis_subset_grpID_icW$intervention_prelim)
    tabyl(cNMA_data_analysis_subset_grpID_icW$comparison_prelim)
    
    cNMA_data_analysis_subset_grpID_icW$FF <- 0
    cNMA_data_analysis_subset_grpID_icW$RS <- 0
    cNMA_data_analysis_subset_grpID_icW$NL <- 0
    cNMA_data_analysis_subset_grpID_icW$SE <- 0
    cNMA_data_analysis_subset_grpID_icW$VF <- 0
    cNMA_data_analysis_subset_grpID_icW$BAU <- 0
    
    tabyl(cNMA_data_analysis_subset_grpID_icW$FF)
    tabyl(cNMA_data_analysis_subset_grpID_icW$RS)
    tabyl(cNMA_data_analysis_subset_grpID_icW$NL)
    tabyl(cNMA_data_analysis_subset_grpID_icW$SE)  
    tabyl(cNMA_data_analysis_subset_grpID_icW$VF)
    tabyl(cNMA_data_analysis_subset_grpID_icW$BAU)
    
    ### FF
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="FF",1, FF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="NL+SE+FF+RS",1, FF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(intervention_prelim=="VF+FF+RS",1, FF))
    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(comparison_prelim=="FF" & FF==0,-1, ifelse(comparison_prelim=="FF" & FF==1, 0, FF)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(comparison_prelim=="FF+RS" & FF==0,-1, ifelse(comparison_prelim=="FF+RS" & FF==1, 0, FF)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(FF = ifelse(comparison_prelim=="NL+FF+RS" & FF==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & FF==1, 0, FF)))
    
    cNMA_data_analysis_subset_grpID_icW_FF <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, FF)
    print(cNMA_data_analysis_subset_grpID_icW_FF)
    tabyl(cNMA_data_analysis_subset_grpID_icW$FF)
    
    ### RS
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+RS",1, RS))    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+FF+RS",1, RS))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+RS",1, RS))   
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+VF+RS",1, RS))     
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="SE+RS",1, RS))     
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="VF+FF+RS",1, RS))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(intervention_prelim=="VF+RS",1, RS))    
    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(comparison_prelim=="FF+RS" & RS==0,-1, ifelse(comparison_prelim=="FF+RS" & RS==1, 0, RS)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(comparison_prelim=="NL+FF+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & RS==1, 0, RS)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(comparison_prelim=="NL+SE+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & RS==1, 0, RS)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(RS = ifelse(comparison_prelim=="RS" & RS==0,-1, ifelse(comparison_prelim=="RS" & RS==1, 0, RS)))
    
    cNMA_data_analysis_subset_grpID_icW_RS <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, RS)
    print(cNMA_data_analysis_subset_grpID_icW_RS)
    tabyl(cNMA_data_analysis_subset_grpID_icW$RS)    

    ### NL
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+RS",1, NL))    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+FF+RS",1, NL))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+RS",1, NL))   
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+VF+RS",1, NL))     

    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(comparison_prelim=="NL+FF+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & NL==1, 0, NL)))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(NL = ifelse(comparison_prelim=="NL+SE+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & NL==1, 0, NL)))

    cNMA_data_analysis_subset_grpID_icW_NL <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, NL)
    print(cNMA_data_analysis_subset_grpID_icW_NL)
    tabyl(cNMA_data_analysis_subset_grpID_icW$NL)
    
    ### SE
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+FF+RS",1, SE))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+RS",1, SE))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+VF+RS",1, SE))  
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(intervention_prelim=="SE+RS",1, SE))  
    
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(SE = ifelse(comparison_prelim=="NL+SE+RS" & SE==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & SE==1, 0, SE)))
    
    cNMA_data_analysis_subset_grpID_icW_SE <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, SE)
    print(cNMA_data_analysis_subset_grpID_icW_SE)
    tabyl(cNMA_data_analysis_subset_grpID_icW$SE)
    
    ### VF
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(VF = ifelse(intervention_prelim=="NL+SE+VF+RS",1, VF))
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(VF = ifelse(intervention_prelim=="VF+FF+RS",1, VF))   
    cNMA_data_analysis_subset_grpID_icW <- cNMA_data_analysis_subset_grpID_icW %>% mutate(VF = ifelse(intervention_prelim=="VF+RS",1, VF))     

    cNMA_data_analysis_subset_grpID_icW_VF <- cNMA_data_analysis_subset_grpID_icW %>% dplyr::select(intervention_prelim, comparison_prelim, VF)
    print(cNMA_data_analysis_subset_grpID_icW_VF)
    tabyl(cNMA_data_analysis_subset_grpID_icW$VF)    
    
    ### Fit NMA model assuming consistency (tau^2_omega=0)
    res_mod_icW_cnma <- rma.mv(effect_size, V_list, 
                            mods = ~ FF + RS + NL + SE + VF - 1, # BAU is excluded to serve as the reference level for the comparisons.
                            random = ~ 1 | record_id/es_id, 
                            rho=0.60, 
                            data=cNMA_data_analysis_subset_grpID_icW)
    summary(res_mod_icW_cnma) 
    
    ### Estimate all pairwise differences between treatments
    contr <- data.frame(t(combn(names(coef(res_mod_icW_cnma)), 2)))
    contr <- contrmat(contr, "X1", "X2")
    rownames(contr) <- paste(contr$X1, "-", contr$X2)
    contr <- as.matrix(contr[-c(1:2)])
    sav <- predict(res_mod_icW_cnma, newmods=contr)
    sav[["slab"]] <- rownames(contr)
    sav
        
    ### Create league table (create diagonal matrix from output sav)
    lt_info_df <- as.data.frame(sav, optional = TRUE)
    lt_info_df <- cbind(Comparison = rownames(lt_info_df), lt_info_df)
    lt_info_df2 <- lt_info_df %>% separate_wider_delim(Comparison, delim = ' - ', names = c('comp1', 'comp2'))
    round_digits <- function(x) {
      round(x, digits = 2)
    }
    convert_to_character <- function(x) {
      as.character(x)
    }
    lt_info_df2[c("pred","ci.lb","ci.ub")] <- lapply(lt_info_df2[c("pred","ci.lb","ci.ub")], round_digits)
    lt_info_df2[c("pred","ci.lb","ci.ub")] <- lapply(lt_info_df2[c("pred","ci.lb","ci.ub")], as.character)
    lt_info_df2$ci.lb <- paste("(", lt_info_df2$ci.lb, " ,", sep= "")
    lt_info_df2$ci.ub <- paste(lt_info_df2$ci.ub, ")", sep= "")
    lt_info_df2 <- lt_info_df2 %>% unite(pred_cis, pred, ci.lb, ci.ub, sep= " ", remove = FALSE )
    print(lt_info_df2)
    lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #This creates the league table formatted as "left vs top".
    lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
    print(lt_info_df3)
    write_csv(lt_info_df3, file = "cnma_league_table_icW_allnodes.csv")
    #write_xlsx(lt_info_df3, 'cnma_league_table_icW_allnodes.xlsx')
    
    ### Compute p-values
    contr <- data.frame(t(combn(c(names(coef(res_mod_icW_cnma)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
    contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
    b <- c(coef(res_mod_icW_cnma),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    vb <- bldiag(vcov(res_mod_icW_cnma),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    pvals <- apply(contr, 1, function(x) pnorm((x%*%b) / sqrt(t(x)%*%vb%*%x)))
    pvals
        
    ### Create table of p-values
    tab <- vec2mat(pvals, corr=FALSE)
    tab[lower.tri(tab)] <- t((1 - tab)[lower.tri(tab)])
    rownames(tab) <- colnames(tab) <- colnames(contr)
    round(tab, 2) # Like Table 2 in the following: https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-015-0060-8/tables/2
        
    ### Compute the P-scores
    pscores <- cbind(round(sort(apply(tab, 1, mean, na.rm=TRUE), decreasing=TRUE), 3))
    pscores
        
    ### Add P-scores to model output object
    res_mod_icW_cnma_df <- tidy(res_mod_icW_cnma, conf.int = TRUE)
    pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
    res_mod_icW_cnma_pscore <- res_mod_icW_cnma_df %>% left_join(pscores_df, by = c("term"))
    res_mod_icW_cnma_pscore <- res_mod_icW_cnma_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
    res_mod_icW_cnma_pscore
    

# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Rational Numbers (R)"
    
    ## Subset analysis data frame further to just the Rhole Numbers (R) intervention content (icR)
    tabyl(NMA_data_analysis_subset_grpID$intervention_content)
    NMA_data_analysis_subset_grpID_icR <- NMA_data_analysis_subset_grpID %>% filter(intervention_content == "R")
    tabyl(NMA_data_analysis_subset_grpID_icR$intervention_content)
    NMA_data_analysis_subset_grpID_icR_c <- NMA_data_analysis_subset_grpID_icR %>% distinct(contrast_id, .keep_all = TRUE)
    NMA_data_analysis_subset_grpID_icR_c %>% count()
    
    ## Add contrast matrix to dataset
    NMA_data_analysis_subset_grpID_icR <- contrmat(NMA_data_analysis_subset_grpID_icR, grp1="intervention_prelim", grp2="comparison_prelim")
    str(NMA_data_analysis_subset_grpID_icR)
    
    ## Calculate the variance-covariance matrix for multi-treatment studies
    V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_icR)
    V_list    
    V_list_icR <- data.frame(V_list)
    #write_csv(V_list_icR, 'V_list_icR.csv')
    
    ## Run additive cNMA with the unique intervention components as moderators  
    tabyl(NMA_data_analysis_subset_grpID_icR$intervention_prelim)
    tabyl(NMA_data_analysis_subset_grpID_icR$comparison_prelim)
    check_icR <- NMA_data_analysis_subset_grpID_icR %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim)
    print(check_icR)
    
      ### Prepare component binaries for cNMA
      cNMA_data_analysis_subset_grpID_icR <- NMA_data_analysis_subset_grpID_icR
      tabyl(cNMA_data_analysis_subset_grpID_icR$intervention_prelim)
      tabyl(cNMA_data_analysis_subset_grpID_icR$comparison_prelim)
      
      cNMA_data_analysis_subset_grpID_icR$FF <- 0
      cNMA_data_analysis_subset_grpID_icR$RS <- 0
      cNMA_data_analysis_subset_grpID_icR$NL <- 0
      cNMA_data_analysis_subset_grpID_icR$SE <- 0
      cNMA_data_analysis_subset_grpID_icR$VF <- 0
      cNMA_data_analysis_subset_grpID_icR$BAU <- 0
      
      tabyl(cNMA_data_analysis_subset_grpID_icR$FF)
      tabyl(cNMA_data_analysis_subset_grpID_icR$RS)
      tabyl(cNMA_data_analysis_subset_grpID_icR$NL)
      tabyl(cNMA_data_analysis_subset_grpID_icR$SE)  
      tabyl(cNMA_data_analysis_subset_grpID_icR$VF)
      tabyl(cNMA_data_analysis_subset_grpID_icR$BAU)
      
      ### FF
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="FF",1, FF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="NL+SE+FF+RS",1, FF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(intervention_prelim=="VF+FF+RS",1, FF))
      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(comparison_prelim=="FF" & FF==0,-1, ifelse(comparison_prelim=="FF" & FF==1, 0, FF)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(comparison_prelim=="FF+RS" & FF==0,-1, ifelse(comparison_prelim=="FF+RS" & FF==1, 0, FF)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(FF = ifelse(comparison_prelim=="NL+FF+RS" & FF==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & FF==1, 0, FF)))
      
      cNMA_data_analysis_subset_grpID_icR_FF <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, FF)
      print(cNMA_data_analysis_subset_grpID_icR_FF)
      tabyl(cNMA_data_analysis_subset_grpID_icR$FF)
      
      ### RS
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+RS",1, RS))    
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+FF+RS",1, RS))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+RS",1, RS))   
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="NL+SE+VF+RS",1, RS))     
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))    
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="SE+RS",1, RS))     
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="VF+FF+RS",1, RS))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(intervention_prelim=="VF+RS",1, RS))    
      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(comparison_prelim=="FF+RS" & RS==0,-1, ifelse(comparison_prelim=="FF+RS" & RS==1, 0, RS)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(comparison_prelim=="NL+FF+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & RS==1, 0, RS)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(comparison_prelim=="NL+SE+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & RS==1, 0, RS)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(RS = ifelse(comparison_prelim=="RS" & RS==0,-1, ifelse(comparison_prelim=="RS" & RS==1, 0, RS)))
      
      cNMA_data_analysis_subset_grpID_icR_RS <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, RS)
      print(cNMA_data_analysis_subset_grpID_icR_RS)
      tabyl(cNMA_data_analysis_subset_grpID_icR$RS)    
      
      ### NL
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+RS",1, NL))    
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+FF+RS",1, NL))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+RS",1, NL))   
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(intervention_prelim=="NL+SE+VF+RS",1, NL))     
      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(comparison_prelim=="NL+FF+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & NL==1, 0, NL)))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(NL = ifelse(comparison_prelim=="NL+SE+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & NL==1, 0, NL)))
      
      cNMA_data_analysis_subset_grpID_icR_NL <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, NL)
      print(cNMA_data_analysis_subset_grpID_icR_NL)
      tabyl(cNMA_data_analysis_subset_grpID_icR$NL)
      
      ### SE
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+FF+RS",1, SE))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+RS",1, SE))      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(intervention_prelim=="NL+SE+VF+RS",1, SE))  
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(intervention_prelim=="SE+RS",1, SE))  
      
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(SE = ifelse(comparison_prelim=="NL+SE+RS" & SE==0,-1, ifelse(comparison_prelim=="NL+SE+RS" & SE==1, 0, SE)))
      
      cNMA_data_analysis_subset_grpID_icR_SE <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, SE)
      print(cNMA_data_analysis_subset_grpID_icR_SE)
      tabyl(cNMA_data_analysis_subset_grpID_icR$SE)
      
      ### VF
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(VF = ifelse(intervention_prelim=="NL+SE+VF+RS",1, VF))
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(VF = ifelse(intervention_prelim=="VF+FF+RS",1, VF))   
      cNMA_data_analysis_subset_grpID_icR <- cNMA_data_analysis_subset_grpID_icR %>% mutate(VF = ifelse(intervention_prelim=="VF+RS",1, VF))     
      
      cNMA_data_analysis_subset_grpID_icR_VF <- cNMA_data_analysis_subset_grpID_icR %>% dplyr::select(intervention_prelim, comparison_prelim, VF)
      print(cNMA_data_analysis_subset_grpID_icR_VF)
      tabyl(cNMA_data_analysis_subset_grpID_icR$VF)    
      
      ### Fit NMA model assuming consistency (tau^2_omega=0)
      res_mod_icR_cnma <- rma.mv(effect_size, V_list, 
                                 mods = ~ FF + RS + NL + SE + VF - 1, # BAU is excluded to serve as the reference level for the comparisons.
                                 random = ~ 1 | record_id/es_id, 
                                 rho=0.60, 
                                 data=cNMA_data_analysis_subset_grpID_icR)
      summary(res_mod_icR_cnma) 
      
      ### Estimate all pairwise differences between treatments
      contr <- data.frame(t(combn(names(coef(res_mod_icR_cnma)), 2)))
      contr <- contrmat(contr, "X1", "X2")
      rownames(contr) <- paste(contr$X1, "-", contr$X2)
      contr <- as.matrix(contr[-c(1:2)])
      sav <- predict(res_mod_icR_cnma, newmods=contr)
      sav[["slab"]] <- rownames(contr)
      sav
      
      ### Create league table (create diagonal matrix from output sav)
      lt_info_df <- as.data.frame(sav, optional = TRUE)
      lt_info_df <- cbind(Comparison = rownames(lt_info_df), lt_info_df)
      lt_info_df2 <- lt_info_df %>% separate_wider_delim(Comparison, delim = ' - ', names = c('comp1', 'comp2'))
      round_digits <- function(x) {
        round(x, digits = 2)
      }
      convert_to_character <- function(x) {
        as.character(x)
      }
      lt_info_df2[c("pred","ci.lb","ci.ub")] <- lapply(lt_info_df2[c("pred","ci.lb","ci.ub")], round_digits)
      lt_info_df2[c("pred","ci.lb","ci.ub")] <- lapply(lt_info_df2[c("pred","ci.lb","ci.ub")], as.character)
      lt_info_df2$ci.lb <- paste("(", lt_info_df2$ci.lb, " ,", sep= "")
      lt_info_df2$ci.ub <- paste(lt_info_df2$ci.ub, ")", sep= "")
      lt_info_df2 <- lt_info_df2 %>% unite(pred_cis, pred, ci.lb, ci.ub, sep= " ", remove = FALSE )
      print(lt_info_df2)
      lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #This creates the league table formatted as "left vs top".
      lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
      print(lt_info_df3)
      write_csv(lt_info_df3, file = "cnma_league_table_icR_allnodes.csv")
      #write_xlsx(lt_info_df3, 'cnma_league_table_icR_allnodes.xlsx')
      
      ### Compute p-values
      contr <- data.frame(t(combn(c(names(coef(res_mod_icR_cnma)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
      contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
      b <- c(coef(res_mod_icR_cnma),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
      vb <- bldiag(vcov(res_mod_icR_cnma),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
      pvals <- apply(contr, 1, function(x) pnorm((x%*%b) / sqrt(t(x)%*%vb%*%x)))
      pvals
      
      ### Create table of p-values
      tab <- vec2mat(pvals, corr=FALSE)
      tab[lower.tri(tab)] <- t((1 - tab)[lower.tri(tab)])
      rownames(tab) <- colnames(tab) <- colnames(contr)
      round(tab, 2) # Like Table 2 in the following: https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-015-0060-8/tables/2
      
      ### Compute the P-scores
      pscores <- cbind(round(sort(apply(tab, 1, mean, na.rm=TRUE), decreasing=TRUE), 3))
      pscores
      
      ### Add P-scores to model output object
      res_mod_icR_cnma_df <- tidy(res_mod_icR_cnma, conf.int = TRUE)
      pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
      res_mod_icR_cnma_pscore <- res_mod_icR_cnma_df %>% left_join(pscores_df, by = c("term"))
      res_mod_icR_cnma_pscore <- res_mod_icR_cnma_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
      res_mod_icR_cnma_pscore        