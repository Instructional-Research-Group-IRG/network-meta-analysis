# This script merges the CNMA Master Database with the updated WWC v4.1 effect sizes (ESs) and standard errors (SEs) from the conversion database.

# Load required packages
  pacman::p_load(dplyr, tidyr, readxl, janitor, psych, readr, googlesheets4, checkmate)
  options(max.print = 1000000)

# Load the cNMA data from Excel file in working directory
  
  ## Read the sheet
  cNMA_data <- read_sheet("https://docs.google.com/spreadsheets/d/1oCcRHU6OSc64OWVNLx1uksOu4QQlf2Xo7p4SZahPMio/edit?gid=931222966#gid=931222966", sheet = "Master Database")  
  str(cNMA_data)
  cNMA_data %>% count() # n=427
  
  ##Check column/variable classes
  class(cNMA_data$study_id)
  class(cNMA_data$contrast_id) #This is a list that needs to be converted to a character string.
  cNMA_data<- cNMA_data %>% mutate(contrast_id = as.character(contrast_id))
  class(cNMA_data$contrast_id)
  class(cNMA_data$es_id)
  
  ##Review column/variable values
  tabyl(cNMA_data$study_id)
  tabyl(cNMA_data$contrast_id)
  describe(cNMA_data$es_id)
  
  ##Prepare database for merging
  cNMA_data_4.1_merge <- cNMA_data
  cNMA_data_4.1_merge <- cNMA_data_4.1_merge %>% arrange(study_id, contrast_id, es_id)
  cNMA_data_4.1_merge <- cNMA_data_4.1_merge %>% mutate(cnma_row_num = row_number()) #To help track which observations had a match in the merge
  cNMA_data_4.1_merge %>% count() # n=427

# Load effect sizes (ESs) and their standard errors (SEs) updated to WWC standards, version 4.1
  es_se_41_data <- read_xlsx("DATABASE Converting ESs and SEs from 4.0 to 4.1 COMPLETED.xlsx")
  es_se_41_data %>% count() # n=376
  
  ##Check column/variable classes
  class(es_se_41_data$study_id)
  class(es_se_41_data$contrast_id)
  class(es_se_41_data$es_id)
  class(es_se_41_data$es_converted_41)
  class(es_se_41_data$se_converted_41)
  
  ##Review column/variable values
  tabyl(es_se_41_data$study_id)
  tabyl(es_se_41_data$contrast_id)
  tabyl(es_se_41_data$es_id)
  describe(es_se_41_data$es_converted_41) # Should be n=376, mean=0.54, range= [-1.44, 4.08]
  describe(es_se_41_data$se_converted_41) # Should be n=376, mean=0.2, range= [0.01, 0.94]
  
  ##Make necessary corrections to conversion database for merging with the CNMA Master Database
  es_se_41_data_merge <- es_se_41_data
  tabyl(es_se_41_data_merge$contrast_id)
  es_se_41_data_merge$contrast_id <- gsub("_disagg", "", es_se_41_data_merge$contrast_id) # "_disagg" removed from contrast IDs in the CNMA Master Database
  es_se_41_data_merge$contrast_id <- tolower(es_se_41_data_merge$contrast_id) # Convert contrast IDs to lowercase to match CNMA Master Database
  es_se_41_data_merge$contrast_id <- gsub("87213_t2", "87213_a", es_se_41_data_merge$contrast_id) # "87213_t2" changed to "87213_a" in the CNMA Master Database
  tabyl(es_se_41_data_merge$contrast_id)
  es_se_41_data_merge %>% count() # n=376
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20755" & contrast_id=="74207_a" & es_id==86)) #This (disaggregated) observation is not included in the CNMA Master Database. 
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20755" & contrast_id=="74207_a" & es_id==87)) #This (disaggregated) observation is not included in the CNMA Master Database.
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20755" & contrast_id=="74207_a" & es_id==88)) #This (disaggregated) observation is not included in the CNMA Master Database.
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20755" & contrast_id=="74207_b" & es_id==90)) #This (disaggregated) observation is not included in the CNMA Master Database.
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20755" & contrast_id=="74207_b" & es_id==91)) #This (disaggregated) observation is not included in the CNMA Master Database.
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20755" & contrast_id=="74207_b" & es_id==92)) #This (disaggregated) observation is not included in the CNMA Master Database.  
  es_se_41_data_merge %>% count() # n=370
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20756" & contrast_id=="74195" & es_id==94)) #This observation is not included in the CNMA Master Database.
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20756" & contrast_id=="74195" & es_id==95)) #This observation is not included in the CNMA Master Database.
  es_se_41_data_merge <- es_se_41_data_merge %>% filter(!(study_id=="MI20756" & contrast_id=="74195" & es_id==96)) #This observation is not included in the CNMA Master Database.
  es_se_41_data_merge %>% count() # n=367
  
  ##Prepare database for merging
  es_se_41_data_merge <- es_se_41_data_merge %>% arrange(study_id, contrast_id, es_id)
  es_se_41_data_merge <- es_se_41_data_merge %>% mutate(esse_row_num = row_number()) #To help track which observations had a match in the merge
  es_se_41_data_merge %>% count() # n=367
  es_se_41_data_merge_short <- es_se_41_data_merge %>%
    dplyr::select(study_id, contrast_id, es_id, es_converted_41, se_converted_41, esse_row_num, outcome, domain) #Reduce to just the variables needed for merging and updating the CNMA Master Database.
  es_se_41_data_merge_short <- es_se_41_data_merge_short %>% rename(measure_name_conversion=outcome) #To match variable names in the CNMA Master Database for merging. 
  es_se_41_data_merge_short <- es_se_41_data_merge_short %>% rename(domain_conversion=domain) #To match variable names in the CNMA Master Database for merging. 
  
# Merge the cNMA data with the updated ESs and SEs
  cNMA_data_4.1_merge %>% count() # n=427
  es_se_41_data_merge_short %>% count() # n=367
  cNMA_data_4.1_merge_postL <- cNMA_data_4.1_merge %>%
     left_join(es_se_41_data_merge_short, by = c("study_id", "contrast_id", "es_id")) #Use left join because we want to retain all observations in the CNMA Master Database but not those from the conversion database that do not make it into the final cNMA database.
  cNMA_data_4.1_merge_postL %>% count() # n=427
  matched_count_postL <- cNMA_data_4.1_merge_postL %>% filter(!is.na(es_converted_41)) %>% nrow()
  matched_count_postL # 367/427 observations in cNMA database matched to conversion database. 367/367 observations in conversion database matched to cNMA (n=all), as expected considering the observations from the conversion database known to not be in the CNMA database were dropped above.
  
  ##Explore the 427 - 367 = 60 observations of the cNMA database that did not match to the conversion database. They should all be observations that are exclusive to the CNMA Master Database.
  cNMA_data_4.1_merge_postF <- cNMA_data_4.1_merge %>%
    full_join(es_se_41_data_merge_short, by = c("study_id", "contrast_id", "es_id"))
  unmatched_postF <- cNMA_data_4.1_merge_postF %>% filter(is.na(cnma_row_num) | is.na(esse_row_num))
  unmatched_postF %>% count() # n=60 unmatched
  describe(unmatched_postF$cnma_row_num) # All 60 non-matches are from CNMA database, as expected. These are the 60 observations in the CNMA Master Database that are not in the conversion database.
  tabyl(unmatched_postF$cnma_row_num)
  describe(unmatched_postF$esse_row_num) # All conversion database observations have a match in the CNMA database, as expected, as indicated by all 60 non-matches having NAs for their conversion database row number.
  tabyl(unmatched_postF$esse_row_num)
  unmatched_postF <- unmatched_postF %>% arrange(cnma_row_num, esse_row_num, study_id, contrast_id, es_id)
  unmatched_postF <- unmatched_postF %>% mutate(cnma_data= !is.na(cnma_row_num))
  unmatched_postF <- unmatched_postF %>% mutate(esse_data= !is.na(esse_row_num))
  unmatched_postF <- unmatched_postF %>% relocate(cnma_data, esse_data)
  unmatched_postF <- unmatched_postF %>% dplyr::select(-cnma_row_num, -esse_row_num)
  #View(unmatched_postF)
  
  write_csv(unmatched_postF, 'cnma_esse_unmatched_postmerge.csv')
      
  tabyl(unmatched_postF$study_id) 
    #Studies with IDs that begin with "CN" (n=57) are new entries unique to the cNMA database. As such, we'd expect no matches to these observations in the conversion database.
    #The other 3 entries of the non-matching 60 observations are from study MI20755.
  unmatched_postF_MI20755 <- unmatched_postF %>% select(cnma_data, esse_data, study_id, contrast_id, es_id, measure_name, domain, effect_size, standard_error) %>% filter(study_id=="MI20755")
  unmatched_postF_MI20755
    #The three measures of contrast 74207 of study MI20755 in the CNMA database are the aggregated versions. The disaggregated versions were dropped above from the conversion database before merging. 
    #Because the effect sizes and standard errors did not change from 4.0 to 4.1 for the disaggregtaed versions, we can use the aggregated 4.0 effect sizes and standard errors as aggregated 4.1 effect sizes and standard errors.  
    #This resolves all final merging issues.
  
  ## Create final effect size column in which existing effect size is replaced with the updated 4.1 standard error  (es_converted_41) if not missing.
  cNMA_data_4.1 <- cNMA_data_4.1_merge_postL #From the left join above, this is our final merged data set (all merge issues resolved/verified above).
  cNMA_data_4.1 %>% count() # n=427
  cNMA_data_4.1 <- cNMA_data_4.1 %>% arrange(study_id, contrast_id, es_id)
  describe(cNMA_data_4.1$effect_size) 
  describe(cNMA_data_4.1$es_converted_41) 
  cNMA_data_4.1 <- cNMA_data_4.1 %>%
    mutate(effect_size_final = if_else(!is.na(es_converted_41), es_converted_41, effect_size)) 
  #assert_numeric(cNMA_data_4.1$effect_size_final, any.missing = FALSE) #Check no missing values. NOTE: Some additional studies without ESs and SEs have been added to the CNMA database which will eventually have 4.1 ESs and SEs in the conversion database for merging and therefore will no longer be missing. Finalize this check once both databases finalized.
  describe(cNMA_data_4.1$effect_size_final)
  
  ## Create final standard error column in which existing standard error is replaced with the updated 4.1 standard error  (se_converted_41) if not missing.
  describe(cNMA_data_4.1$standard_error) 
  describe(cNMA_data_4.1$se_converted_41) 
  cNMA_data_4.1 <- cNMA_data_4.1 %>%
    mutate(standard_error_final = if_else(!is.na(se_converted_41), se_converted_41, standard_error)) # Create final standard error column in which existing standard error  is replaced with the updated 4.1 standard error  (se_converted_41) if not missing.
  #assert_numeric(cNMA_data_4.1$standard_error_final, any.missing = FALSE) #Check no missing values. NOTE: Some additional studies without ESs and SEs have been added to the CNMA database which will eventually have 4.1 ESs and SEs in the conversion database for merging and therefore will no longer be missing. Finalize this check once both databases finalized.
  describe(cNMA_data_4.1$standard_error_final)
  
  ## Export and save final CNMA data set
  write_csv(cNMA_data_4.1, 'cNMA_data_4.1.csv')
  
  ## Export and save final CNMA data set with just key effect size and standard error variables as a check.
  cNMA_data_4.1_check= cNMA_data_4.1 %>% dplyr::select(study_id, contrast_id, es_id, effect_size, es_converted_41, effect_size_final, standard_error, se_converted_41, standard_error_final)
  #View(cNMA_data_4.1_check)
  write_csv(cNMA_data_4.1_check, 'cNMA_data_4.1_check.csv')