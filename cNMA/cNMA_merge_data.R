pacman::p_load(dplyr, tidyr, readxl, janitor, psych, readr)

# Load the cNMA data from Excel file in working directory

  cNMA_data <- read_xlsx("CNMA Master Database 4-22-25 v2 BZfixed.xlsx", sheet = "Master Database")
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
  es_se_41_data$contrast_id <- gsub("87213_t2", "87213_a", es_se_41_data$contrast_id)
  tabyl(es_se_41_data$contrast_id)
  tabyl(es_se_41_data$es_id)
  describe(es_se_41_data$es_converted_41) # Should be n=376, mean=0.54, range= [-1.44, 4.08]
  describe(es_se_41_data$se_converted_41) # Should be n=376, mean=0.54, range= [0.01, 0.94]
  
  es_se_41_data_merge <- es_se_41_data %>%
    dplyr::select(study_id, contrast_id, es_id, contrast, domain, outcome, outcome_type,	level_of_assignment, analytic_method, es_converted_41, se_converted_41) 
  es_se_41_data_merge <- es_se_41_data_merge %>% arrange(study_id, contrast_id, es_id)
  es_se_41_data_merge <- es_se_41_data_merge %>% mutate(esse_row_num = row_number()) #To help track which observations had a match in the merge
  es_se_41_data_merge %>% count() #376  
  write_csv(es_se_41_data_merge, 'es_se_41_data_merge.csv')
  
# Merge the cNMA data with the updated ESs and SEs
  
  cNMA_data_4.1_merge <- cNMA_data %>%
    dplyr::select(study_id, contrast_id, es_id, simple_number, contrast_name, measure_name, domain, effect_size, standard_error) 
  cNMA_data_4.1_merge <- cNMA_data_4.1_merge %>% arrange(study_id, contrast_id, es_id)
  cNMA_data_4.1_merge <- cNMA_data_4.1_merge %>% mutate(cnma_row_num = row_number()) #To help track which observations had a match in the merge
  cNMA_data_4.1_merge %>% count() #409
  write_csv(cNMA_data_4.1_merge, 'cNMA_data_4.1_merge.csv')
  
  cNMA_data_4.1_merge_postL <- cNMA_data_4.1_merge %>%
    left_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"))
  matched_count_postL <- cNMA_data_4.1_merge_postL %>% filter(!is.na(es_converted_41)) %>% nrow()
  matched_count_postL # 367/409 observations in cNMA database matched to conversion database. 367/376 observations in conversion database matched to cNMA. 

  cNMA_data_4.1_merge_postF <- cNMA_data_4.1_merge %>%
    full_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"))
  matched_count_postF <- cNMA_data_4.1_merge_postF %>% filter(!is.na(cnma_row_num) & !is.na(esse_row_num)) %>% nrow()
  matched_count_postF

# Explore observations that did not match across the two databases
  
  unmatched_postF <- cNMA_data_4.1_merge_postF %>% filter(is.na(cnma_row_num) | is.na(esse_row_num))
  unmatched_postF %>% count() #48 unmatched from cNMA databae + 15 unmatched from conversion database = 63 total unmatched
  unmatched_postF <- unmatched_postF %>% arrange(cnma_row_num, esse_row_num, study_id, contrast_id, es_id)
  unmatched_postF <- unmatched_postF %>% mutate(cnma_data= !is.na(cnma_row_num))
  unmatched_postF <- unmatched_postF %>% mutate(esse_data= !is.na(esse_row_num))
  unmatched_postF <- unmatched_postF %>% relocate(cnma_data, esse_data)
  unmatched_postF <- unmatched_postF %>% select(-cnma_row_num, -esse_row_num)
  View(unmatched_postF)
  write_csv(unmatched_postF, 'cnma_esse_unmatched_postmerge.csv')