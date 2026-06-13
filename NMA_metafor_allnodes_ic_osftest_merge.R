# This script....

# Load required packages
  ## Install 'devel' version of metafor package
  ##install.packages("remotes") 
  ##remotes::install_github("wviechtb/metafor") 

  ## Install and load other required packages
  ##install.packages("pacman") 
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor, naniar, igraph, multcomp, broom, gridExtra, ggplot2, writexl, readr, grid, gridExtra, cowplot, extrafont, readxl)

# Write needed functions
  is_id <- function(data, vars) {
    nrow(data) == nrow(unique(data[vars])) #Returns true if list of columns uniquely identifies each observation
  }
  
# Prepare base NMA analysis database for merging with base OSF database
  
  ## Load full NNMA database
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1ayNoKwbxnUVa1XspqAWpS2YBFIwApvS5t3lld5Kx2VA/edit?gid=0#gid=0", sheet="Master Database") # <<Copy of NNMA Master Database - March 17, 11:18 AM>>; The master database changed after submission of publication in late March 2025. This recovered older version from 3/17/25 reproduces the results reported in the publication.
    
  ## Corrections needed to the abrreviated database to match the NNMA database
    # NNMA_Data <- NNMA_Data %>% mutate(intervention_content=  ifelse(record_id== "MI22399" & es_id %in% c(206, 207),"R", intervention_content))
    # NNMA_Data <- NNMA_Data %>% mutate(intervention_prelim=  ifelse(record_id== "MI20625" & es_id %in% c(50, 51, 52),"FF+RS", intervention_prelim))
    # NNMA_Data <- NNMA_Data %>% mutate(contrast_id=  ifelse(record_id== "MI10012" & intervention_prelim=="FF","7", contrast_id))
    
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
  
  ## Subset data following NMA analysis specifications
    
    ### Tabulate variables upon which to subset data
    tabyl(NNMA_Data$aggregated)
    tabyl(NNMA_Data$measure_type)
    tabyl(NNMA_Data$wwc_rating)  
    tabyl(NNMA_Data$intervention_prelim)    
    tabyl(NNMA_Data$comparison_prelim) 
    
    ### Subset data for analysis 
    NMA_data_analysis_subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") & aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") & (TvsT==1 | TvsT==0))
    NMA_data_analysis_subset %>% count() 
    #NOTE: We call the subset NMA analysis database the "base" NMA analysis database becasue it still includes all mirror contrasts, 
    #      (i.e., same bundle of components on each side of the contrast) and rows with missing values. n=373. Same as "base" OSF database loaded below.
    
    ## Check for full duplicates
    dups <- anyDuplicated(NMA_data_analysis_subset)
    assert("assert no duplicate entries", dups == 0) #No full duplicates. Data already in wide format.
    
    is_id(NMA_data_analysis_subset, c("record_id"))
    is_id(NMA_data_analysis_subset, c("record_id", "es_id"))
    is_id(NMA_data_analysis_subset, c("es_id"))
    is_id(NMA_data_analysis_subset, c("record_id", "effect_size"))
    
    NMA_data_analysis_subset_check <- NMA_data_analysis_subset %>% dplyr::select(record_id, contrast_id, es_id, effect_size)
    NMA_data_analysis_subset_check <- NMA_data_analysis_subset_check %>% arrange(record_id, contrast_id, es_id, effect_size)
    NMA_data_analysis_subset_check %>% print(n = Inf) 
    
# Prepare base OSF database for merging with base NMA database
    
    ## Load base OSF database
    OSF_Data <- read_csv("OSF/OSF_NMA Database.csv") #This is the abbreviated database to be uploaded to OSF.
    OSF_Data %>% count() 
    
    ## Check for full duplicates
    dups <- anyDuplicated(OSF_Data)
    assert("assert no duplicate entries", dups == 0) #No full duplicates. Data already in wide format.
    
    is_id(OSF_Data, c("record_id"))
    is_id(OSF_Data, c("record_id", "es_id"))
    is_id(OSF_Data, c("es_id"))
    is_id(OSF_Data, c("record_id", "effect_size"))
    
    OSF_Data_check <- OSF_Data %>% dplyr::select(record_id, contrast_id, es_id, effect_size)
    OSF_Data_check <- OSF_Data_check %>% arrange(record_id, contrast_id, es_id, effect_size)
    OSF_Data_check %>% print(n = Inf) 
    
    ## Load base OSF revised es_id database
    OSF_revise_es_ids <- read_csv("OSF/OSF_revise_es_ids.csv") #This is a crosswalk between the effect IDs (which identify unique observations) in the OSF & NMA databases, which are not the same. Both databases have the exact same 373 observations but the effect IDs in the OSF databse were created after the intial NNMA subsetting that results in the n=373 base NMA analysis database (of which the OSF database is the abbreviated version).
    OSF_revise_es_ids %>% count() 
    
    ## Merge effect size IDs that match base NMA analysis database to base OSF database
    OSF_Data <- OSF_Data %>% rename(es_id_old = es_id)
    OSF_Data_revise_es_ids <- OSF_Data %>% full_join(OSF_revise_es_ids, by = c("record_id", "es_id_old"), suffix = c("_osf_base", "_osf_rev"))
    OSF_Data_revise_es_ids %>% count() 
    
    OSF_Data_revise_es_ids_check <- OSF_Data_revise_es_ids %>% dplyr::select(record_id, contrast_id, es_id_old, es_id, effect_size)
    OSF_Data_revise_es_ids_check <- OSF_Data_revise_es_ids_check %>% arrange(record_id, contrast_id, es_id, effect_size)
    OSF_Data_revise_es_ids_check %>% print(n = Inf) 
    
#Merge contrast IDs of NMA analysis database to OSF database
    NMA_data_analysis_subset_revid <- NMA_data_analysis_subset %>% dplyr::select(record_id, contrast_id, es_id)
    NMA_data_analysis_subset_revid %>% print(n = Inf) 
    
    OSF_Data_revise_es_ids <- OSF_Data_revise_es_ids %>% rename(contrast_id_old = contrast_id)
    OSF_Data_revise_ids <- OSF_Data_revise_es_ids %>% full_join(NMA_data_analysis_subset_check, by = c("record_id", "es_id"), suffix = c("_osf_base", "_nma_base"))
    OSF_Data_revise_ids %>% count()
    
    skim(OSF_Data_revise_ids)
    
    ## Export analysis dataset with all domains for sharing
    write_csv(OSF_Data_revise_ids, "OSF/OSF_Data_revise_ids.csv")
  