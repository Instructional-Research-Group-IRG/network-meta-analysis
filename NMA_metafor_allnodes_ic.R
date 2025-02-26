# Network meta-analysis (NMA) is a multivariate meta-regression technique for comparing multiple interventions from a network of studies 
# within a single analysis. In effect, NMAs allow for comparisons of interventions not evaluated within the same study. Because a network 
# meta-analysis accounts for all evidence, that is evidence from both direct and indirect comparisons, it produces more precise estimates 
# than those obtained from a traditional multivariate meta-regression, which only includes direct comparisons of interventions. NMA also 
# allows for the ranking of interventions.  

# Load required packages
  
  ## Install 'devel' version of metafor package
  ##install.packages("remotes") 
  ##remotes::install_github("wviechtb/metafor") 
   
  ## Install and load other required packages
  ##install.packages("pacman") 
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor, naniar, igraph, multcomp, broom, gridExtra, ggplot2, writexl, readr, grid, gridExtra, cowplot, extrafont)

# Load (read) data (i.e., copy data to 'dat')
  #dat <- read_sheet("https://docs.google.com/spreadsheets/d/1bWugw06yFyetIVYlhAHHzM_d3KGhegxxLBm-5463j2Q/edit#gid=0") #Test data
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set

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
  ## However, as noted above, the same group (i.e., the same bundle + sample size combination) in different contrasts (rows) of the same record ID should receive the same group ID regardless of their group assignment 
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
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% filter(intervention_prelim!=comparison_prelim) #This also removes any rows with <NA> values in columns intervention_prelim & comparison_prelim.
  tabyl(NMA_data_analysis_subset_grpID$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID$comparison_prelim)  
  NMA_data_analysis_subset_grpID %>% count()
  tabyl(NMA_data_analysis_subset_grpID$domain)
  NMA_data_analysis_subset_grpID$intervention_prelim <- as.factor(NMA_data_analysis_subset_grpID$intervention_prelim)
  NMA_data_analysis_subset_grpID$comparison_prelim <- as.factor(NMA_data_analysis_subset_grpID$comparison_prelim)
  class(NMA_data_analysis_subset_grpID$intervention_prelim)
  class(NMA_data_analysis_subset_grpID$comparison_prelim)  
  
  ## Drop rows with missing values in the intervention and comparison columns (i.e., <NA>).
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
    write_csv(NMA_study_contrast_list, 'NMA_study_contrast_list_all_nodes.csv')
    
    ### Number of studies
    NMA_data_analysis_subset_grpID_s <- NMA_data_analysis_subset_grpID %>% distinct(record_id, .keep_all = TRUE)
    NMA_data_analysis_subset_grpID_s %>% count()
    tabyl(NMA_data_analysis_subset_grpID_s$record_id)
    
# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Whole Numbers (W)"
      
  ## Subset analysis data frame further to just the Whole Numbers (W) intervention content (icW)
  tabyl(NMA_data_analysis_subset_grpID$intervention_content)
  NMA_data_analysis_subset_grpID_icW <- NMA_data_analysis_subset_grpID %>% filter(intervention_content == "W")
  tabyl(NMA_data_analysis_subset_grpID_icW$intervention_content)
  NMA_data_analysis_subset_grpID_icW_c <- NMA_data_analysis_subset_grpID_icW %>% distinct(contrast_id, .keep_all = TRUE)
  NMA_data_analysis_subset_grpID_icW_c %>% count()
  
  ## Calculate the number of unique contrasts in which each intervention bundle is included
  tabyl(NMA_data_analysis_subset_grpID_icW$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID_icW$comparison_prelim)
  num_contrasts_icW <- NMA_data_analysis_subset_grpID_icW %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim)
  print(num_contrasts_icW)
  num_contrasts_icW_long <- num_contrasts_icW %>% pivot_longer(c(intervention_prelim, comparison_prelim ), names_to= "group_IC", values_to="group_intervention")
  print(num_contrasts_icW_long)
  num_contrasts_icW_long2 <- num_contrasts_icW_long %>% distinct(record_id, contrast_id, group_intervention, .keep_all = TRUE)
  print(num_contrasts_icW_long2)
  tabyl(num_contrasts_icW_long2$contrast_id) #Should be n=2 for each contrast if reshape and distinct steps done correctly: 1 intervention & 1 comparison per unique contrast. 
  num_contrasts_icW_long3 <- tabyl(num_contrasts_icW_long2$group_intervention)
  num_contrasts_icW_long3 <- num_contrasts_icW_long3 %>% dplyr::select(intervention= 'num_contrasts_icW_long2$group_intervention', num_contrasts= 'n')
  str(num_contrasts_icW_long3)
  num_contrasts_icW_long3$intervention <- as.character(num_contrasts_icW_long3$intervention)
  num_contrasts_icW_long3$intervention <- gsub("\\+", ".", num_contrasts_icW_long3$intervention)
  str(num_contrasts_icW_long3)
  print(num_contrasts_icW_long3) 
 
  # ## Remove one-contrast bundles
  # print(num_contrasts_icW_long3) ##NL+RS bundle is in only one contrast
  # tabyl(NMA_data_analysis_subset_grpID_icW$intervention_prelim)
  # tabyl(NMA_data_analysis_subset_grpID_icW$comparison_prelim)
  # NMA_data_analysis_subset_grpID_icW <- NMA_data_analysis_subset_grpID_icW %>% filter(intervention_prelim!="NL+RS")
  # #NMA_data_analysis_subset_grpID_icW <- NMA_data_analysis_subset_grpID_icW %>% filter(comparison_prelim!="NL+RS")
  # tabyl(NMA_data_analysis_subset_grpID_icW$intervention_prelim)
  # tabyl(NMA_data_analysis_subset_grpID_icW$comparison_prelim)  
   
  ## Calculate the number of students within each intervention bundle across all unique study-contrasts
  num_students_icW <- NMA_data_analysis_subset_grpID_icW %>% dplyr::select(record_id, contrast_id, domain, measure_name, intervention_prelim, intervention_n, comparison_prelim, comparison_n, full_sample_size)
  print(num_students_icW)
  num_students_icW2 <- num_students_icW %>% distinct(record_id, contrast_id, .keep_all = TRUE) #Keep only unique entries of each unique study-contrast so that each group of students is not summed more than once (because of multiple measures within some contrasts).
  print(num_students_icW2) 
  num_students_icW_long <- num_students_icW2 %>% pivot_longer(c(intervention_prelim, comparison_prelim ), names_to= "group_IC", values_to="intervention_comparison") #Put intervention and comparison groups in same row for summing students receiving the same intervention bundle regardless of whether it was received in the intervention or comparison group assignment.
  print(num_students_icW_long, n=20)
  num_students_icW_long2 <- num_students_icW_long %>% mutate(num_students_bundle= ifelse(group_IC == "intervention_prelim", intervention_n, comparison_n)) # Put number of students by assignment group in same column condition on the intervention/comparison group assignment.
  print(num_students_icW_long2)
  num_students_icW_long3 <- num_students_icW_long2 %>% group_by(intervention_comparison) %>% summarize(sum_num_students_bundle= sum(num_students_bundle)) # Sum students by intervention bundle.
  str(num_students_icW_long3)
  print(num_students_icW_long3)
  target_icW <- c("BAU","FF","FF+RS","NL+FF+RS","NL+RS","RS","VF+FF+RS","VF+RS")
  num_students_icW_long3 <- num_students_icW_long3[match(target_icW, num_students_icW_long3$intervention_comparison),]
  num_students_icW_long3$intervention_comparison <- as.character(num_students_icW_long3$intervention_comparison)
  str(num_students_icW_long3)  
  print(num_students_icW_long3)    
        
  ## Add contrast matrix to dataset
  NMA_data_analysis_subset_grpID_icW <- contrmat(NMA_data_analysis_subset_grpID_icW, grp1="intervention_prelim", grp2="comparison_prelim")
  str(NMA_data_analysis_subset_grpID_icW)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_icW)
  V_list    
  V_list_icW <- data.frame(V_list)
  write_csv(V_list_icW, 'V_list_icW.csv')
        
  ##Run standard NMA with the unique interventions bundles as moderators  
  tabyl(NMA_data_analysis_subset_grpID_icW$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID_icW$comparison_prelim)
  check_icW <- NMA_data_analysis_subset_grpID_icW %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim)
  print(check_icW)

    ### Fit model assuming consistency (tau^2_omega=0)
    res_mod_icW <- rma.mv(effect_size, V_list, 
                            mods = ~ FF + FF.RS + NL.FF.RS + NL.RS + RS + VF.FF.RS + VF.RS - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons
                            random = ~ 1 | record_id/es_id, 
                            rho=0.60, 
                            data=NMA_data_analysis_subset_grpID_icW)
    summary(res_mod_icW) 

    ### Fit Jackson's model to test for inconsistency 
    res_mod_icW_J <- rma.mv(effect_size, V_list,
                              mods = ~ FF + FF.RS + NL.FF.RS + NL.RS + RS + VF.FF.RS + VF.RS - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons 
                              random = list(~ 1 | record_id/es_id, ~ domain | record_id, ~ contrast_id | record_id),
                              rho=0.60, phi=1/2,
                              data=NMA_data_analysis_subset_grpID_icW)
    summary(res_mod_icW_J)

    ### Estimate all pairwise differences between treatments
    contr <- data.frame(t(combn(names(coef(res_mod_icW)), 2)))
    contr <- contrmat(contr, "X1", "X2")
    rownames(contr) <- paste(contr$X1, "-", contr$X2)
    contr <- as.matrix(contr[-c(1:2)])
    sav <- predict(res_mod_icW, newmods=contr)
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
    lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis")
    lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
    print(lt_info_df3)
    write_csv(lt_info_df3, 'nma_league_table_icW_allnodes.csv')
    #write_xlsx(lt_info_df3, 'nma_league_table_icW_allnodes.xlsx')
    
    ### Compute p-values
    contr <- data.frame(t(combn(c(names(coef(res_mod_icW)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
    contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
    b <- c(coef(res_mod_icW),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    vb <- bldiag(vcov(res_mod_icW),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
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
    res_mod_icW_df <- tidy(res_mod_icW, conf.int = TRUE)
    pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
    res_mod_icW_pscore <- res_mod_icW_df %>% left_join(pscores_df, by = c("term"))
    res_mod_icW_pscore <- res_mod_icW_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
    res_mod_icW_pscore
    
    ### Create forest plot using metafor's built-in function
    # forest(coef(res_mod_icW), diag(vcov(res_mod_icW)), slab=sub(".", " ", names(coef(res_mod_icW)), fixed=TRUE),
    #        #xlim=c(-5,5), alim=c(-3,3), psize=6, header="Intervention", top=2,
    #        header="Intervention",
    #        xlab="Difference in Standardized Mean Change (compared to Control)")
        
    ### Create forest plot using ggplot
    
      #### First create plot of estimates and confidence intervals
      res_mod_icW_pscore <- res_mod_icW_pscore %>% arrange(desc(Pscore))
      str(res_mod_icW_pscore)
      print(res_mod_icW_pscore)
      print(num_contrasts_icW_long3)
      res_mod_icW_pscore <- res_mod_icW_pscore %>% left_join(num_contrasts_icW_long3, by = "intervention") # Merge on number of unique contrasts in which each intervention bundle is included
      res_mod_icW_pscore$colour <- rep(c("yellow","burlywood1","royalblue1","green","azure1","red","pink"))
      str(res_mod_icW_pscore)
      print(res_mod_icW_pscore)

      res_mod_icW_pscore_forest <- ggplot(res_mod_icW_pscore, aes(x= estimate, y= intervention, xmin= ci.lb, xmax= ci.ub)) +  
        geom_hline(aes(yintercept = intervention, colour = colour), size=15) +
        geom_pointrange(shape = 22, fill = "black", size = res_mod_icW_pscore$num_contrasts/5) + 
        geom_text(label = res_mod_icW_pscore$num_contrasts, hjust = 0.5, vjust = 2.25, colour = "black", size =7, family= "Times New Roman") +        
        geom_vline(xintercept = 0, linetype = 3) +
        xlab("Difference in Standardized Mean Change (compared to Control)") +
        labs(caption = "*Values under points indicate number of contrasts                                                        ") +
        ylab("Intervention Bundle") +
        theme_classic() +
        scale_colour_identity() +
        scale_y_discrete(limits = rev(res_mod_icW_pscore$intervention)) +
        theme(axis.title.x = element_text(face = "bold", size=25, family= "Times New Roman"), #x-axis title
              plot.caption = element_text(size = 18, family= "Times New Roman"), #plot caption (x-axis)
              axis.title.y = element_text(face = "bold", size=25, hjust= 0.44, family= "Times New Roman"), #y-axis title
              axis.text.y = element_text(size=25, hjust=0.5), #the intervention bundles
              axis.text.x = element_text(size = 20, family= "Times New Roman")) #x-axis tick values
      res_mod_icW_pscore_forest
          
      #### Next create data table for merging with above plot with estimates and confidence intervals combined in one column
      res_mod_icW_pscore2 <- res_mod_icW_pscore
      round_digits <- function(x) {
        round(x, digits = 2)
      }
      convert_to_character <- function(x) {
        as.character(x)
      }
      res_mod_icW_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_icW_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], round_digits)
      res_mod_icW_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_icW_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], as.character)
      res_mod_icW_pscore2$ci.lb <- paste("(", res_mod_icW_pscore2$ci.lb, " -", sep= "")
      res_mod_icW_pscore2$ci.ub <- paste(res_mod_icW_pscore2$ci.ub, ")", sep= "")
      res_mod_icW_pscore2 <- res_mod_icW_pscore2 %>% unite(estimate_cis, estimate, ci.lb, ci.ub, sep= " ", remove = FALSE )
      print(res_mod_icW_pscore2)
          
      LfLabels1<-data.frame(x=c(1), 
                            y=c(7.5),
                            lab=c("Estimate (95% CI)"))
      LfLabels1      
      data_table1 <- ggplot(data = res_mod_icW_pscore2, aes(x, y = intervention)) +
        geom_hline(aes(yintercept = intervention, colour = colour), size = 15) + 
        geom_text(aes(x = 1, label = estimate_cis), size= 9.25, family= "Times New Roman") + 
        geom_text(data=LfLabels1,aes(x,y,label=lab, fontface="bold"), size=8, family= "Times New Roman") + 
        scale_colour_identity() +
        theme_void() + 
        theme(text= element_text(family = "Times New Roman")) +
        scale_y_discrete(limits = rev(res_mod_icW_pscore$intervention)) 
      data_table1
      
      LfLabels2<-data.frame(x=c(1), 
                            y=c(7.5),
                            lab=c("P-score"))
      LfLabels2      
      data_table2 <- ggplot(data = res_mod_icW_pscore2, aes(x, y = intervention)) +
        geom_hline(aes(yintercept = intervention, colour = colour), size = 15) + 
        geom_text(aes(x = 1, label = Pscore), size= 9.25, family= "Times New Roman") +
        geom_text(data=LfLabels2,aes(x,y,label=lab, fontface="bold"), size=8, family= "Times New Roman") +
        scale_colour_identity() +
        theme_void() + 
        theme(text= element_text(family = "Times New Roman")) +
        scale_y_discrete(limits = rev(res_mod_icW_pscore$intervention)) 
      data_table2
      
      #### Finally, merge plot and datatable for final forest plot
      aligned_plots <- align_plots(res_mod_icW_pscore_forest, data_table1, data_table2, align = "h")
      final_fp_nma_icW <- grid.arrange(grobs = aligned_plots, nrow= 1, widths= c(2.5,0.75,0.5))
      final_fp_nma_icW
        
    ### Create network graph
      
    ### Note: The data are currently in a contrast-based, "wide" format in which each contrast is an observation 
    ###       (i.e., intervention and comparison assignments of each contrast are in separate columns of the same row.)
    ###       Need to reshape the data "long" to an arm-based format in which the two assignment groups (intervention/comparison)
    ###       are in separate rows of the same column grouped by contrast ID across those rows. This facilitates the pairing of intervention/comparison 
    ###       groups for the creation of the (weighted) edges of the network graph.
      
      #### Review data in contrast-based wide format before reshape for comparison with data after reshape (as a desk check)
      NMA_data_analysis_subset2 <- NMA_data_analysis_subset_grpID_icW %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset2) #Example rows of the contrast-based wide format. Compare to the long format printed below.
      
      #### Each unique contrast within each unique study should only be counted once when weighting the network connections between each unique contrast combination of intervention versus comparison in the network graph.
      #### Because there can be multiple measures within multiple domains within each unique contrast within each unique study, we need reduce the data set to one observation per unique contrast within each unique study 
      #### so that a contrast with more domains/measures than another contrast is not overweighted in the visualization of the network connections (re the relative thicknesses of the "edges" between the I/C nodes in the network graph).
      NMA_data_analysis_subset_grpID_icW %>% count(record_id, contrast_id)
      NMA_data_analysis_subset3 <- NMA_data_analysis_subset_grpID_icW %>% distinct(record_id, contrast_id, .keep_all = TRUE)
      NMA_data_analysis_subset3 %>% count(record_id, contrast_id)
      NMA_data_analysis_subset3 %>% count()
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset3 %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size)
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset4 %>% unite("int_comp_prelim" , c(intervention_prelim,comparison_prelim), remove = FALSE)
      tabyl(NMA_data_analysis_subset4$int_comp_prelim) #The relative number of observations (n) for the intervention_BAU contrasts in this table should match the relative thicknesses of the lines (called "edges") in the network graph.
      
      #### Reshape data to arm-based long format
      NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset3, c(intervention_prelim, comparison_prelim), names_to = "assignment_I_C", values_to = "intervention_comparison")
      
      #### Review data in arm-based long format after reshape for comparison with data before reshape (as a desk check)
      NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% dplyr::select(record_id, contrast_id, assignment_I_C, intervention_comparison, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset_long2) #Example rows of the arm-based long format. Compare to the wide format printed above.
      
      #### Create the table of intervention/comparison pairs for creating the network graph with igraph
      dat_igraph <- NMA_data_analysis_subset_long
      dat_igraph$intervention_comparison <- as.character(dat_igraph$intervention_comparison)
      pairs <- data.frame(do.call(rbind, lapply(split(dat_igraph$intervention_comparison, dat_igraph$contrast_id), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
      print(pairs)
      pairs$X1 <- factor(pairs$X1, levels=sort(unique(dat_igraph$intervention_comparison)))
      pairs$X2 <- factor(pairs$X2, levels=sort(unique(dat_igraph$intervention_comparison)))
      tab <- table(pairs[,1], pairs[,2])
      tab
      
      #### Creating the network graph with igraph
      set.seed(3524)
      g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
      
      num_students_icW_long3
      num_students_icW_long4 <- num_students_icW_long3 %>% mutate(sum_num_students_bundle2= if_else((intervention_comparison=="NL+FF+RS" | intervention_comparison=="VF+RS"),sum_num_students_bundle*2,sum_num_students_bundle))
      num_students_icW_long4 <- num_students_icW_long4 %>% mutate(sum_num_students_bundle2= if_else((intervention_comparison=="FF" | intervention_comparison=="VF+FF+RS"),sum_num_students_bundle2*1.5,sum_num_students_bundle2))
      #num_students_icW_long4 <- num_students_icW_long3 %>% mutate(sum_num_students_bundle2= sum_num_students_bundle)
      num_students_icW_long4 <- num_students_icW_long4 %>% mutate(dist=c(0,1.75,0,1.5,2.5,0,-2,-2))
      num_students_icW_long4 <- num_students_icW_long4 %>% mutate(color=c("lightgray","royalblue1","burlywood1","red","green","azure1","yellow","pink"))
      num_students_icW_long4 <- num_students_icW_long4 %>% mutate(intervention_comparison= ifelse(intervention_comparison == "BAU", "Control", intervention_comparison))
      num_students_icW_long4
      
      plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
           layout=layout_in_circle(g, order=c("BAU","FF","FF+RS","NL+FF+RS","NL+RS","RS","VF+FF+RS","VF+RS")),
           vertex.size=(num_students_icW_long4$sum_num_students_bundle2)/50, vertex.color=num_students_icW_long4$color, 
           vertex.label.color="black", vertex.label.font=2, vertex.label=num_students_icW_long4$intervention_comparison, vertex.label.dist=num_students_icW_long4$dist)  

      # num_students_icW_long4
      # num_students_icW_long5 <- num_students_icW_long4 %>% mutate(sum_num_students_bundle3= if_else(intervention_comparison=="BAU",sum_num_students_bundle*5000,sum_num_students_bundle))
      # num_students_icW_long5
      # 
      # plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
      #      layout=layout_in_circle(g, order=c("BAU", "RS","NL+RS","FF","FF+RS")),
      #      vertex.size=log((num_students_icW_long5$sum_num_students_bundle3))*2.5, vertex.color=num_students_icW_long4$color,
      #      vertex.label.color="black", vertex.label.font=2, vertex.label=num_students_icW_long5$intervention_comparison, vertex.label.dist=num_students_icW_long4$dist)      
      
# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Rational Numbers (R)"
      
  ## Subset analysis data frame further to just the Rational Numbers (R) intervention content (icR)
  tabyl(NMA_data_analysis_subset_grpID$intervention_content)
  NMA_data_analysis_subset_grpID_icR <- NMA_data_analysis_subset_grpID %>% filter(intervention_content == "R")
  tabyl(NMA_data_analysis_subset_grpID_icR$intervention_content)
  NMA_data_analysis_subset_grpID_icR_c <- NMA_data_analysis_subset_grpID_icR %>% distinct(contrast_id, .keep_all = TRUE)
  NMA_data_analysis_subset_grpID_icR_c %>% count()
  
  ## Calculate the number of unique contrasts in which each intervention bundle is included
  tabyl(NMA_data_analysis_subset_grpID_icR$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID_icR$comparison_prelim)
  num_contrasts_icR <- NMA_data_analysis_subset_grpID_icR %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim)
  print(num_contrasts_icR)
  num_contrasts_icR_long <- num_contrasts_icR %>% pivot_longer(c(intervention_prelim, comparison_prelim ),names_to= "group_IC", values_to="group_intervention")
  print(num_contrasts_icR_long)
  num_contrasts_icR_long2 <- num_contrasts_icR_long %>% distinct(record_id, contrast_id, group_intervention, .keep_all = TRUE)
  print(num_contrasts_icR_long2)
  tabyl(num_contrasts_icR_long2$contrast_id) #Should be n=2 for each contrast if reshape and distinct steps done correctly: 1 intervention & 1 comparison per unique contrast. 
  num_contrasts_icR_long3 <- tabyl(num_contrasts_icR_long2$group_intervention)
  num_contrasts_icR_long3 <- num_contrasts_icR_long3 %>% dplyr::select(intervention= 'num_contrasts_icR_long2$group_intervention', num_contrasts= 'n')
  str(num_contrasts_icR_long3)
  num_contrasts_icR_long3$intervention <- as.character(num_contrasts_icR_long3$intervention)
  num_contrasts_icR_long3$intervention <- gsub("\\+", ".", num_contrasts_icR_long3$intervention)
  str(num_contrasts_icR_long3)
  print(num_contrasts_icR_long3)
  
  ## Remove one-contrast bundles
  # print(num_contrasts_icR_long3)
  # tabyl(NMA_data_analysis_subset_grpID_icR$intervention_prelim)
  # tabyl(NMA_data_analysis_subset_grpID_icR$comparison_prelim)
  # NMA_data_analysis_subset_grpID_icR <- NMA_data_analysis_subset_grpID_icR %>% filter(intervention_prelim!="NL+SE+VF+RS" & intervention_prelim!="SE+VF+RS")
  # #NMA_data_analysis_subset_grpID_icR <- NMA_data_analysis_subset_grpID_icR %>% filter(comparison_prelim!="NL+SE+VF+RS" & comparison_prelim!="SE+VF+RS")
  # tabyl(NMA_data_analysis_subset_grpID_icR$intervention_prelim)
  # tabyl(NMA_data_analysis_subset_grpID_icR$comparison_prelim)  

  ## Calculate the number of students within each intervention bundle across all unique study-contrasts
  num_students_icR <- NMA_data_analysis_subset_grpID_icR %>% dplyr::select(record_id, contrast_id, domain, measure_name, intervention_prelim, intervention_n, comparison_prelim, comparison_n, full_sample_size)
  print(num_students_icR)
  num_students_icR2 <- num_students_icR %>% distinct(record_id, contrast_id, .keep_all = TRUE) #Keep only unique entries of each unique study-contrast so that each group of students is not summed more than once (because of multiple measures within some contrasts).
  print(num_students_icR2) 
  num_students_icR_long <- num_students_icR2 %>% pivot_longer(c(intervention_prelim, comparison_prelim ), names_to= "group_IC", values_to="intervention_comparison") #Put intervention and comparison groups in same row for summing students receiving the same intervention bundle regardless of whether it was received in the intervention or comparison group assignment.
  print(num_students_icR_long, n=20)
  num_students_icR_long2 <- num_students_icR_long %>% mutate(num_students_bundle= ifelse(group_IC == "intervention_prelim", intervention_n, comparison_n)) # Put number of students by assignment group in same column condition on the intervention/comparison group assignment.
  print(num_students_icR_long2)
  num_students_icR_long3 <- num_students_icR_long2 %>% group_by(intervention_comparison) %>% summarize(sum_num_students_bundle= sum(num_students_bundle)) # Sum students by intervention bundle.
  str(num_students_icR_long3)
  print(num_students_icR_long3)
  target_icR <- c("BAU","NL+FF+RS","NL+RS","NL+SE+FF+RS","NL+SE+RS","NL+SE+VF+RS","RS","SE+RS")
  num_students_icR_long3 <- num_students_icR_long3[match(target_icR, num_students_icR_long3$intervention_comparison),]
  num_students_icR_long3$intervention_comparison <- as.character(num_students_icR_long3$intervention_comparison)
  str(num_students_icR_long3)  
  print(num_students_icR_long3)  
  
  ## Add contrast matrix to dataset
  NMA_data_analysis_subset_grpID_icR <- contrmat(NMA_data_analysis_subset_grpID_icR, grp1="intervention_prelim", grp2="comparison_prelim")
  str(NMA_data_analysis_subset_grpID_icR)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_icR)
  V_list
  V_list_icR_allnodes <- data.frame(V_list)
  write_csv(V_list_icR_allnodes, 'V_list_icR_allnodes.csv')
      
  ##Run standard NMA with the unique interventions bundles as moderators: exclude one-contrast bundles  
  tabyl(NMA_data_analysis_subset_grpID_icR$intervention_prelim)
  tabyl(NMA_data_analysis_subset_grpID_icR$comparison_prelim)
  check_icR <- NMA_data_analysis_subset_grpID_icR %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim)
  print(check_icR)
  
    ### Fit model assuming consistency (tau^2_omega=0)
    res_mod_icR <- rma.mv(effect_size, V_list, 
                           mods = ~ NL.FF.RS + NL.RS + NL.SE.FF.RS + NL.SE.RS + NL.SE.VF.RS + RS + SE.RS - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons
                           random = ~ 1 | record_id/es_id, 
                           rho=0.60, 
                           data=NMA_data_analysis_subset_grpID_icR)
    summary(res_mod_icR)
    
    ### Fit Jackson's model to test for inconsistency 
    res_mod_icR_J <- rma.mv(effect_size, V_list, 
                           mods = ~ NL.FF.RS + NL.RS + NL.SE.FF.RS + NL.SE.RS + NL.SE.VF.RS + RS + SE.RS - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons
                           random = list(~ 1 | record_id/es_id, ~ domain | record_id, ~ contrast_id | record_id),
                           rho=0.60, phi=1/2,
                           data=NMA_data_analysis_subset_grpID_icR)
    summary(res_mod_icR_J)
      
    ### Estimate all pairwise differences between treatments
    contr <- data.frame(t(combn(names(coef(res_mod_icR)), 2)))
    contr <- contrmat(contr, "X1", "X2")
    rownames(contr) <- paste(contr$X1, "-", contr$X2)
    contr <- as.matrix(contr[-c(1:2)])
    sav <- predict(res_mod_icR, newmods=contr)
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
    lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #To-do: possible to format ci below? + color code by sig
    lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
    print(lt_info_df3)
    write_csv(lt_info_df3, 'nma_league_table_icR_allnodes.csv')
    #write_xlsx(lt_info_df3, 'nma_league_table_icR_allnodes.xlsx')
      
    ### Compute p-values
    contr <- data.frame(t(combn(c(names(coef(res_mod_icR)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
    contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
    b <- c(coef(res_mod_icR),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    vb <- bldiag(vcov(res_mod_icR),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
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
    res_mod_icR_df <- tidy(res_mod_icR, conf.int = TRUE)
    pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
    res_mod_icR_pscore <- res_mod_icR_df %>% left_join(pscores_df, by = c("term"))
    res_mod_icR_pscore <- res_mod_icR_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
    res_mod_icR_pscore
      
    ### Create forest plot using metafor's built-in function
    # forest(coef(res_mod_icR), diag(vcov(res_mod_icR)), slab=sub(".", " ", names(coef(res_mod_icR)), fixed=TRUE),
    #        #xlim=c(-5,5), alim=c(-3,3), psize=6, header="Intervention", top=2,
    #        header="Intervention",
    #        xlab="Difference in Standardized Mean Change (compared to Control)")
      
    ### Create forest plot using ggplot
      
      #### First create plot of estimates and confidence intervals
      res_mod_icR_pscore <- res_mod_icR_pscore %>% arrange(desc(Pscore))  
      str(res_mod_icR_pscore)
      print(res_mod_icR_pscore)
      print(num_contrasts_icR_long3)
      res_mod_icR_pscore <- res_mod_icR_pscore %>% left_join(num_contrasts_icR_long3, by = "intervention") # Merge on number of unique contrasts in which each intervention bundle is included
      #res_mod_icR_pscore$colour <- rep(c("turquoise1","red","mediumpurple1","darkorange","green","yellow","azure1")) 
      #res_mod_icR_pscore$colour <- rep(c("lightyellow","red","mediumpurple1","darkorange","green","maroon1","azure1")) 
      res_mod_icR_pscore$colour <- rep(c("lavenderblush2","red","mediumpurple1","darkorange","maroon1","green","azure1"))
      str(res_mod_icR_pscore)   
      print(res_mod_icR_pscore)
      
      res_mod_icR_pscore_forest <- ggplot(res_mod_icR_pscore, aes(x= estimate, y= intervention, xmin= ci.lb, xmax= ci.ub)) + 
        geom_hline(aes(yintercept = intervention, colour = colour), size=15) +
        geom_pointrange(shape = 22, fill = "black", size = res_mod_icR_pscore$num_contrasts/5) + 
        geom_text(label = res_mod_icR_pscore$num_contrasts, hjust = 0.5, vjust = 2.25, colour = "black", size =7, family= "Times New Roman") +        
        geom_vline(xintercept = 0, linetype = 3) +
        xlab("Difference in Standardized Mean Change (compared to Control)") +
        labs(caption = "*Values under points indicate number of contrasts                                               ") +
        ylab("Intervention Bundle") +
        theme_classic() +
        scale_colour_identity() +
        scale_y_discrete(limits = rev(res_mod_icR_pscore$intervention)) +
        theme(axis.title.x = element_text(face = "bold", size=25, family= "Times New Roman"), #x-axis title
              plot.caption = element_text(size = 18, family= "Times New Roman"), #plot caption (x-axis)
              axis.title.y = element_text(face = "bold", size=25, hjust= 0.44, family= "Times New Roman"), #y-axis title
              axis.text.y = element_text(size=25, hjust=0.5), #the intervention bundles
              axis.text.x = element_text(size = 20, family= "Times New Roman")) #x-axis tick values      
      res_mod_icR_pscore_forest
      
      #### Next create data table for merging with above plot with estimates and confidence intervals combined in one column
      res_mod_icR_pscore2 <- res_mod_icR_pscore
      round_digits <- function(x) {
        round(x, digits = 2)
      }
      convert_to_character <- function(x) {
        as.character(x)
      }
      res_mod_icR_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_icR_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], round_digits)
      res_mod_icR_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_icR_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], as.character)
      res_mod_icR_pscore2$ci.lb <- paste("(", res_mod_icR_pscore2$ci.lb, " -", sep= "")
      res_mod_icR_pscore2$ci.ub <- paste(res_mod_icR_pscore2$ci.ub, ")", sep= "")
      res_mod_icR_pscore2 <- res_mod_icR_pscore2 %>% unite(estimate_cis, estimate, ci.lb, ci.ub, sep= " ", remove = FALSE )
      print(res_mod_icR_pscore2)

      LfLabels1<-data.frame(x=c(1), 
                           y=c(7.5),
                           lab=c("Estimate (95% CI)"))
      LfLabels1      
      data_table1 <- ggplot(data = res_mod_icR_pscore2, aes(x, y = intervention)) +
        geom_hline(aes(yintercept = intervention, colour = colour), size = 15) +
        geom_text(aes(x = 1, label = estimate_cis), size=9.25, family= "Times New Roman") +
        geom_text(data=LfLabels1,aes(x,y,label=lab, fontface="bold"), size=8, family= "Times New Roman") +
        scale_colour_identity() +
        theme_void() + 
        theme(text= element_text(family = "Times New Roman")) +
        scale_y_discrete(limits = rev(res_mod_icR_pscore$intervention)) 
      data_table1
      
      LfLabels2<-data.frame(x=c(1), 
                            y=c(7.5),
                            lab=c("P-score"))
      LfLabels2      
      data_table2 <- ggplot(data = res_mod_icR_pscore2, aes(x, y = intervention)) +
        geom_hline(aes(yintercept = intervention, colour = colour), size = 15) +
        geom_text(aes(x = 1, label = Pscore), size= 9.25, family= "Times New Roman") +
        geom_text(data=LfLabels2,aes(x,y,label=lab, fontface="bold"), size=8, family= "Times New Roman") +
        scale_colour_identity() +
        theme_void() + 
        theme(text= element_text(family = "Times New Roman")) +
        scale_y_discrete(limits = rev(res_mod_icR_pscore$intervention)) 
      data_table2
      
      #### Finally, merge plot and datatable for final forest plot
      aligned_plots <- align_plots(res_mod_icR_pscore_forest, data_table1, data_table2, align = "h")
      final_fp_nma_icR <- grid.arrange(grobs = aligned_plots, nrow= 1, widths= c(2.5,0.75,0.5))
      final_fp_nma_icR
      
    ### Create network graph
      
    ### Note: The data are currently in a contrast-based, "wide" format in which each contrast is an observation 
    ###       (i.e., intervention and comparison assignments of each contrast are in separate columns of the same row.)
    ###       Need to reshape the data "long" to an arm-based format in which the two assignment groups (intervention/comparison)
    ###       are in separate rows of the same column grouped by contrast ID across those rows. This facilitates the pairing of intervention/comparison 
    ###       groups for the creation of the (weighted) edges of the network graph.
      
      #### Review data in contrast-based wide format before reshape for comparison with data after reshape (as a desk check)
      NMA_data_analysis_subset2 <- NMA_data_analysis_subset_grpID_icR %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset2) #Example rows of the contrast-based wide format. Compare to the long format printed below.
      
      #### Each unique contrast within each unique study should only be counted once when weighting the network connections between each unique contrast combination of intervention and comparison in the network graph.
      #### Because there can be multiple measures within multiple domains within each unique contrast within each unique study, we need reduce the data set to one observation per unique contrast within each unique study 
      #### so that a contrast with more domains/measures than another contrast is not overweighted in the visualization of the network connections (re the relative thicknesses of the "edges" between the I/C nodes in the network graph).
      NMA_data_analysis_subset_grpID_icR %>% count(record_id, contrast_id)      
      NMA_data_analysis_subset3 <- NMA_data_analysis_subset_grpID_icR %>% distinct(record_id, contrast_id, .keep_all = TRUE)
      NMA_data_analysis_subset3 %>% count(record_id, contrast_id)
      NMA_data_analysis_subset3 %>% count()
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset3 %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size)
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset4 %>% unite("int_comp_prelim" , c(intervention_prelim,comparison_prelim), remove = FALSE)
      tabyl(NMA_data_analysis_subset4$int_comp_prelim) #The relative number of observations (n) for the intervention_BAU contrasts in this table should match the relative thicknesses of the lines (called "edges") in the network graph.
      
      #### Reshape data to arm-based long format
      NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset3, c(intervention_prelim, comparison_prelim), names_to = "assignment_I_C", values_to = "intervention_comparison")
      
      #### Review data in arm-based long format after reshape for comparison with data before reshape (as a desk check)
      NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% dplyr::select(record_id, contrast_id, assignment_I_C, intervention_comparison, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset_long2) #Example rows of the arm-based long format. Compare to the wide format printed above.
      
      #### Create the table of intervention/comparison pairs for creating the network graph with igraph
      dat_igraph <- NMA_data_analysis_subset_long
      dat_igraph$intervention_comparison <- as.character(dat_igraph$intervention_comparison)
      pairs <- data.frame(do.call(rbind, lapply(split(dat_igraph$intervention_comparison, dat_igraph$contrast_id), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
      print(pairs)
      pairs$X1 <- factor(pairs$X1, levels=sort(unique(dat_igraph$intervention_comparison)))
      pairs$X2 <- factor(pairs$X2, levels=sort(unique(dat_igraph$intervention_comparison)))
      tab <- table(pairs[,1], pairs[,2])
      tab
      
      #### Creating the network graph with igraph
      set.seed(3524)
      g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
      
      num_students_icR_long3
      #num_students_icR_long4 <- num_students_icR_long3 %>% mutate(sum_num_students_bundle2= if_else((intervention_comparison=="SE+VF+RS" | intervention_comparison=="NL+SE+RS" | intervention_comparison=="NL+SE+VF+RS" | intervention_comparison=="SE+VF+RS"),sum_num_students_bundle*1.5,sum_num_students_bundle))
      num_students_icR_long4 <- num_students_icR_long3 %>% mutate(sum_num_students_bundle2= if_else((intervention_comparison=="NL+SE+RS" | intervention_comparison=="NL+SE+VF+RS"),sum_num_students_bundle*1.5,sum_num_students_bundle))
      num_students_icR_long4 <- num_students_icR_long4 %>% mutate(sum_num_students_bundle2= if_else((intervention_comparison=="RS" | intervention_comparison=="SE+RS"),sum_num_students_bundle2*3.5,sum_num_students_bundle2))
      num_students_icR_long4 <- num_students_icR_long4 %>% mutate(dist=c(0,3.65,3.25,4.9,3.55,-2.5,1.5,-2))
      #num_students_icR_long4 <- num_students_icR_long4 %>% mutate(color=c("lightgray","red","green","mediumpurple1","darkorange","maroon1","lightyellow"))
      num_students_icR_long4 <- num_students_icR_long4 %>% mutate(color=c("lightgray","red","green","mediumpurple1","darkorange","maroon1","azure1","lavenderblush2"))
      num_students_icR_long4 <- num_students_icR_long4 %>% mutate(intervention_comparison= ifelse(intervention_comparison == "BAU", "Control", intervention_comparison))
      num_students_icR_long4
      
      plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
           layout=layout_in_circle(g, order=c("BAU","NL+FF+RS","NL+SE+FF+RS","NL+SE+RS","NL+RS","NL+SE+VF+RS","RS","SE+RS")),
           vertex.size=(num_students_icR_long4$sum_num_students_bundle2)/15, vertex.color=num_students_icR_long4$color, 
           vertex.label.color="black", vertex.label.font=2, vertex.label=num_students_icR_long4$intervention_comparison, vertex.label.dist=num_students_icR_long4$dist)  

      # num_students_icR_long4
      # num_students_icR_long5 <- num_students_icR_long4 %>% mutate(sum_num_students_bundle3= if_else(intervention_comparison=="BAU",sum_num_students_bundle*5000,sum_num_students_bundle))
      # num_students_icR_long5
      # 
      # plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
      #      layout=layout_in_circle(g, order=c("BAU","NL+FF+RS","NL+SE+FF+RS","NL+SE+RS","NL+RS")),
      #      vertex.size=log((num_students_icR_long5$sum_num_students_bundle3))*2.5, vertex.color=num_students_icR_long4$color,
      #      vertex.label.color="black", vertex.label.font=2, vertex.label=num_students_icR_long5$intervention_comparison, vertex.label.dist=num_students_icR_long4$dist)


#===================================== ANALYSIS SAMPLE SIZES =====================================#        

# Combine final analysis files by domain
      NMA_data_analysis_subset_grpID_final <- bind_rows(NMA_data_analysis_subset_grpID_icW, NMA_data_analysis_subset_grpID_icR)
      NMA_data_analysis_subset_grpID_final <- NMA_data_analysis_subset_grpID_final %>% rename(contrast_name= contrast_name...15)
      #NMA_data_analysis_subset_grpID_final <- NMA_data_analysis_subset_grpID_final %>% rename(intervention_content= intervention_content...36)
      tabyl(NMA_data_analysis_subset_grpID_final$domain)
      NMA_data_analysis_subset_grpID_final$simple_number <- as.character(NMA_data_analysis_subset_grpID_final$simple_number)
      NMA_data_analysis_subset_grpID_final$simple_number <- as.numeric(NMA_data_analysis_subset_grpID_final$simple_number)
      tabyl(NMA_data_analysis_subset_grpID_final$simple_number)
      
# Check counts of final NMA analysis file
      
      ### Number of effect sizes
      NMA_data_analysis_subset_grpID_final %>% count()
      tabyl(NMA_data_analysis_subset_grpID_final$es_id)
      NMA_study_contrast_list_es_final <- NMA_data_analysis_subset_grpID_final %>% dplyr::select(record_id, "Abbreviated Citation", contrast_id, simple_number, contrast_name, domain, measure_name, es_id) #Create list of study-contrasts included in NMA.
      NMA_study_contrast_list_es_final <- NMA_study_contrast_list_es_final %>% arrange(record_id, contrast_id, es_id)
      print(NMA_study_contrast_list_es_final)
      write_csv(NMA_study_contrast_list_es_final, 'NMA_study_contrast_list_es_final_allnodes.csv')
      
      ### Number of contrasts
      NMA_data_analysis_subset_grpID_final_c <- NMA_data_analysis_subset_grpID_final %>% distinct(contrast_id, .keep_all = TRUE)
      NMA_data_analysis_subset_grpID_final_c %>% count()
      tabyl(NMA_data_analysis_subset_grpID_final_c$contrast_id)
      NMA_study_contrast_list_final <- NMA_data_analysis_subset_grpID_final_c %>% dplyr::select(record_id, "Abbreviated Citation", contrast_id, contrast_name) #Create list of study-contrasts included in NMA.
      NMA_study_contrast_list_final <- NMA_study_contrast_list_final %>% arrange(record_id, contrast_id)
      print(NMA_study_contrast_list_final)
      write_csv(NMA_study_contrast_list_final, 'NMA_study_contrast_list_final_allnodes.csv')
      
      ### Number of studies
      NMA_data_analysis_subset_grpID_final_s <- NMA_data_analysis_subset_grpID_final %>% distinct(record_id, .keep_all = TRUE)
      NMA_data_analysis_subset_grpID_final_s %>% count()
      tabyl(NMA_data_analysis_subset_grpID_final_s$record_id)     
      
      ## Calculate the number of students
      NMA_data_analysis_subset_grpID_final_ord <- NMA_data_analysis_subset_grpID_final %>% arrange(record_id, contrast_id, desc(full_sample_size)) #Sort data based on study id -> contrast id within study id -> largest outcome measure sample size within contrast id. That way when we reduce the dataset to the contrast level below, we take the outcome measure within each contrast with the largest sample size for summing sample sizes across contrasts (We want the max sum).
      NMA_data_analysis_subset_grpID_final_ord2 <- NMA_data_analysis_subset_grpID_final_ord %>% dplyr::select(record_id, contrast_id, es_id, full_sample_size, TvsT)
      num_students_final <- NMA_data_analysis_subset_grpID_final_ord2 %>% distinct(record_id, contrast_id, .keep_all = TRUE) #Keep only unique entries of each unique study-contrast so that each group of students is not summed more than once (because of multiple measures within some contrasts).
      
      num_students_final_ALL <- num_students_final %>% summarize(sum_num_students_bundle= sum(full_sample_size)) # Sum students
      str(num_students_final_ALL)
      print(num_students_final_ALL)
      
      tabyl(NMA_data_analysis_subset_grpID_final$TvsT)
      num_students_final_TvT <- num_students_final %>% filter(TvsT==1)
      num_students_final_TvT <- num_students_final_TvT %>% summarize(sum_num_students_bundle= sum(full_sample_size)) # Sum students
      str(num_students_final_TvT)
      print(num_students_final_TvT)
      
      tabyl(NMA_data_analysis_subset_grpID_final$TvsT)
      num_students_final_TvBAU <- num_students_final %>% filter(TvsT==0)
      num_students_final_TvBAU <- num_students_final_TvBAU %>% summarize(sum_num_students_bundle= sum(full_sample_size)) # Sum students
      str(num_students_final_TvBAU)
      print(num_students_final_TvBAU)      
      
      ## Export data for verifying counts
      NMA_data_analysis_subset_grpID_final2 <- NMA_data_analysis_subset_grpID_final %>% dplyr::select(domain, record_id, contrast_id, es_id, intervention_prelim, comparison_prelim, measure_name, measure_type)
      NMA_data_analysis_subset_grpID_final2 <- NMA_data_analysis_subset_grpID_final2 %>% arrange(domain, record_id, contrast_id, es_id, intervention_prelim, comparison_prelim, measure_name, measure_type)
      write_csv(NMA_data_analysis_subset_grpID_final2, 'NMA_counts_by_domain_all_nodes.csv')
     
