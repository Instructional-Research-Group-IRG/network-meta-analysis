pacman::p_load(dplyr, tidyr, readxl, janitor, psych, readr)

# Load the cNMA data from Excel file in working directory

  #cNMA_data <- read_xlsx("CNMA Master Database 4-22-25_sbm.xlsx", sheet = "Master Database", range="C1:BZ410")
  cNMA_data <- read_xlsx("CNMA Master Database 4-22-25_sbm.xlsx", sheet = "Master Database")
  
  str(cNMA_data)
  
  class(cNMA_data$study_id)
  class(cNMA_data$contrast_id)
  class(cNMA_data$es_id)
  
  options(max.print = 1000000)
  tabyl(cNMA_data$study_id)
  tabyl(cNMA_data$contrast_id)
  tabyl(cNMA_data$es_id)


# Load effect sizes (ESs) and their standard errors (SEs) updated to WWC standards, version 4.1
  es_se_41_data <- read_xlsx("DATABASE Converting ESs and SEs from 4.0 to 4.1 COMPLETED.xlsx")
  
  class(es_se_41_data$study_id)
  class(es_se_41_data$contrast_id)
  class(es_se_41_data$es_id)
  class(es_se_41_data$es_converted_41)
  class(es_se_41_data$se_converted_41)
  
  options(max.print = 1000000)
  tabyl(es_se_41_data$study_id)
  tabyl(es_se_41_data$contrast_id)
  es_se_41_data$contrast_id <- gsub("_disagg", "", es_se_41_data$contrast_id)
  tabyl(es_se_41_data$contrast_id)
  tabyl(es_se_41_data$es_id)
  describe(es_se_41_data$es_converted_41) # Should be n=376, mean=0.54, range= [-1.44, 4.08]
  describe(es_se_41_data$se_converted_41) # Should be n=376, mean=0.54, range= [0.01, 0.94]
  
  es_se_41_data_merge <- es_se_41_data %>%
    select(study_id, contrast_id, es_id, contrast, domain, outcome, outcome_type,	level_of_assignment, analytic_method, es_converted_41, se_converted_41) 
  write_csv(es_se_41_data_merge, 'es_se_41_data_merge.csv')
  
# Merge the cNMA data with the updated ESs and SEs
  
  cNMA_data_4.1_merge <- cNMA_data %>%
    select(study_id, contrast_id, es_id, simple_number, contrast_name, measure_name, domain, effect_size, standard_error) 
  write_csv(cNMA_data_4.1_merge, 'cNMA_data_4.1_merge.csv')
  
  # cNMA_data_4.1 <- cNMA_data %>%
  #   left_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"))

  # cNMA_data_4.1 <- cNMA_data_4.1_merge %>%
  #   inner_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"), suffix = c("_cnma", "_esse"))  
  
  cNMA_data_4.1 <- cNMA_data_4.1_merge %>%
    full_join(es_se_41_data_merge, by = c("study_id", "contrast_id", "es_id"), suffix = c("_cnma", "_esse"))    
  
  cNMA_data_4.1_ord <- cNMA_data_4.1 %>% arrange(study_id, contrast_id, es_id)
    
  class(cNMA_data_4.1$es_converted_41)
  class(cNMA_data_4.1$se_converted_41)
  
  describe(cNMA_data_4.1$es_converted_41)
  describe(cNMA_data_4.1$se_converted_41)
  
  class(cNMA_data_4.1$study_id)
  class(cNMA_data_4.1$contrast_id)
  class(cNMA_data_4.1$es_id)
  
  options(max.print = 1000000)
  tabyl(cNMA_data_4.1$study_id)
  tabyl(cNMA_data_4.1$contrast_id)
  tabyl(cNMA_data_4.1$es_id)
  
  cNMA_data_4.1_filtered <- cNMA_data_4.1 %>%
    #filter(!is.na(es_converted_41) & !is.na(se_converted_41)) %>%
    select(study_id, contrast_id, es_id, es_converted_41, se_converted_41)