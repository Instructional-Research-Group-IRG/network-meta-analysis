pacman::p_load(dplyr, tidyr, readxl, janitor, psych, readr)

# Load the cNMA data from Excel file in working directory

  #cNMA_data <- read_xlsx("CNMA Master Database 4-22-25.xlsx", sheet = "Master Database", range="C1:BZ410")
  cNMA_data <- read_xlsx("CNMA Master Database 4-22-25_BZfixed.xlsx", sheet = "Master Database") # cNMA master database file with fixed BZ column title truncated to prevent read-in error
  str(cNMA_data)
  cNMA_data %>% count()
  
  class(cNMA_data$study_id)
  class(cNMA_data$contrast_id)
  class(cNMA_data$es_id)
  
  options(max.print = 1000000)
  tabyl(cNMA_data$study_id)
  tabyl(cNMA_data$contrast_id)
  tabyl(cNMA_data$es_id)

# Load effect sizes (ESs) and their standard errors (SEs) updated to WWC standards, version 4.1
  es_se_41_data <- read_xlsx("DATABASE Converting ESs and SEs from 4.0 to 4.1 COMPLETED.xlsx")
  es_se_41_data %>% count()
  
  class(es_se_41_data$study_id)
  class(es_se_41_data$contrast_id)
  class(es_se_41_data$es_id)
  class(es_se_41_data$es_converted_41)
  class(es_se_41_data$se_converted_41)
  
  options(max.print = 1000000)
  tabyl(es_se_41_data$study_id)
  tabyl(es_se_41_data$contrast_id)
  es_se_41_data$contrast_id <- gsub("_disagg", "", es_se_41_data$contrast_id)
  es_se_41_data$contrast_id <- tolower(es_se_41_data$contrast_id)
  tabyl(es_se_41_data$contrast_id)
  tabyl(es_se_41_data$es_id)
  describe(es_se_41_data$es_converted_41) # Should be n=376, mean=0.54, range= [-1.44, 4.08]
  describe(es_se_41_data$se_converted_41) # Should be n=376, mean=0.54, range= [0.01, 0.94]
  
  es_se_41_data_merge <- es_se_41_data %>%
    dplyr::select(study_id, contrast_id, es_id, contrast, domain, outcome, outcome_type,	level_of_assignment, analytic_method, es_converted_41, se_converted_41) 
  write_csv(es_se_41_data_merge, 'es_se_41_data_merge.csv')
  es_se_41_data_merge %>% count()
  
# Merge the cNMA data with the updated ESs and SEs
  
  cNMA_data_4.1_merge <- cNMA_data %>%
    dplyr::select(study_id, contrast_id, es_id, simple_number, contrast_name, measure_name, domain, effect_size, standard_error) 
  write_csv(cNMA_data_4.1_merge, 'cNMA_data_4.1_merge.csv')
  cNMA_data_4.1_merge %>% count()
  
  cNMA_data_4.1_merge <- cNMA_data_4.1_merge %>% mutate(orig_row = row_number())
  cNMA_data_4.1_merge_postL <- cNMA_data_4.1_merge %>%
    left_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"))
  
  matched_count <- cNMA_data_4.1_merge_postL %>% filter(!is.na(es_converted_41)) %>% nrow()
  matched_count

  # cNMA_data_4.1 <- cNMA_data_4.1_merge %>%
  #   inner_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"), suffix = c("_cnma", "_esse"))  
  
  # cNMA_data_4.1 <- cNMA_data_4.1_merge %>%
  #   full_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"), suffix = c("_cnma", "_esse"))    
  # 
  # cNMA_data_4.1_ord <- cNMA_data_4.1 %>% arrange(study_id, contrast_id, es_id)
  #   
  # class(cNMA_data_4.1$es_converted_41)
  # class(cNMA_data_4.1$se_converted_41)
  # 
  # describe(cNMA_data_4.1$es_converted_41)
  # describe(cNMA_data_4.1$se_converted_41)
  # 
  # class(cNMA_data_4.1$study_id)
  # class(cNMA_data_4.1$contrast_id)
  # class(cNMA_data_4.1$es_id)
  # 
  # options(max.print = 1000000)
  # tabyl(cNMA_data_4.1$study_id)
  # tabyl(cNMA_data_4.1$contrast_id)
  # tabyl(cNMA_data_4.1$es_id)
  # 
  # cNMA_data_4.1_filtered <- cNMA_data_4.1 %>%
  #   #filter(!is.na(es_converted_41) & !is.na(se_converted_41)) %>%
  #   dplyr::select(study_id, contrast_id, es_id, es_converted_41, se_converted_41)
  
# Merge the updated ESs and SEs with the NNMA database as a check
  
  NMA_data_analysis_subset_grpID %>% count() # Dataframe object NMA_data_analysis_subset_grpID created in NMA_metafor_allnodes_ic.R
  NMA_data_analysis_subset_grpID_merge <- NMA_data_analysis_subset_grpID %>% dplyr::select(record_id, contrast_id, es_id, simple_number, contrast_name, measure_name, domain, effect_size, standard_error) 
  NMA_data_analysis_subset_grpID_merge <- NMA_data_analysis_subset_grpID_merge %>% rename(study_id = record_id)
  write_csv(NMA_data_analysis_subset_grpID_merge, 'NMA_data_analysis_subset_grpID_merge.csv')
  NMA_data_analysis_subset_grpID_merge %>% count()
  
  class(NMA_data_analysis_subset_grpID_merge$study_id)
  class(NMA_data_analysis_subset_grpID_merge$contrast_id)
  class(NMA_data_analysis_subset_grpID_merge$es_id)
  
  options(max.print = 1000000)
  tabyl(NMA_data_analysis_subset_grpID_merge$study_id)
  tabyl(NMA_data_analysis_subset_grpID_merge$contrast_id)
  NMA_data_analysis_subset_grpID_merge$contrast_id <- gsub("_disagg", "", NMA_data_analysis_subset_grpID_merge$contrast_id)
  NMA_data_analysis_subset_grpID_merge$contrast_id <- tolower(NMA_data_analysis_subset_grpID_merge$contrast_id)
  tabyl(es_se_41_data$contrast_id)
  tabyl(NMA_data_analysis_subset_grpID_merge$es_id)
  
  ##Left join
  NMA_data_analysis_subset_grpID_merge %>% count()
  es_se_41_data_merge %>% count()
  
  NMA_data_analysis_subset_grpID_merge <- NMA_data_analysis_subset_grpID_merge %>% mutate(orig_row = row_number())
  
  NMA_data_analysis_subset_grpID_merge_postL <- NMA_data_analysis_subset_grpID_merge %>%
    left_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"))
  NMA_data_analysis_subset_grpID_merge_postL %>% count()
  
  matched_count <- NMA_data_analysis_subset_grpID_merge_postL %>% filter(!is.na(es_converted_41)) %>% nrow()
  matched_count
  
  # NMA_data_analysis_subset_grpID_merge <- NMA_data_analysis_subset_grpID_merge %>%
  #   inner_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"), suffix = c("_cnma", "_esse"))  
  
  # NMA_data_analysis_subset_grpID_merge_post <- NMA_data_analysis_subset_grpID_merge %>%
  #   full_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"), suffix = c("_cnma", "_esse"))     
     