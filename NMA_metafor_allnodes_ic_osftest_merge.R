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
    
    ## Load revised es_id crosswalk
    #NOTE: This is a crosswalk between the effect IDs (which identify unique observations) in the OSF & NMA databases, which are not the same. 
    #      Both databases have the exact same 373 observations but the effect IDs in the OSF databse were created after the intial NNMA subsetting
    #      that results in the n=373 base NMA analysis database (of which the OSF database is the abbreviated version). Those in the NMA database
    #      were created before subsetting, and those are the ones we want to use in the OSF database so that they match across databases.
    #      The crosswalk was created from merging the NMA and OSF print outs above and manually correcting the effect size IDs in Excel. This was done manually  
    #      because matching observations could be done easily by effect size ID but since that is the column we are updating, matching would have to be done using 
    #      character columns which is problematic given typos, trailing spaces, etc. As such matching was done visually by record ID (study ID), measure name,
    #      and effect size value, which was a relatively easy and accurate method for creating the crosswalk given that many effect sizes IDs were already
    #      the same for the same exact observations across both datasets, and those that did need updated were in batches within the same record ID
    #      so it was easy to match those observations across datasets using measure name and effect size value, then 
    OSF_revise_es_ids <- read_csv("OSF/OSF_revise_es_ids.csv")
    OSF_revise_es_ids %>% count() 
    OSF_revise_es_ids %>% print(n = Inf) 
    
    ## Merge effect size IDs that match base NMA analysis database to base OSF database
    OSF_Data <- OSF_Data %>% rename(es_id_old = es_id)
    OSF_Data_revise_es_ids <- OSF_Data %>% full_join(OSF_revise_es_ids, by = c("record_id", "es_id_old"), suffix = c("_osf_base", "_osf_rev"))
    OSF_Data_revise_es_ids %>% count() 
    
    OSF_Data_revise_es_ids_check <- OSF_Data_revise_es_ids %>% dplyr::select(record_id, contrast_id, es_id_old, es_id, effect_size)
    OSF_Data_revise_es_ids_check <- OSF_Data_revise_es_ids_check %>% arrange(record_id, contrast_id, es_id, effect_size)
    OSF_Data_revise_es_ids_check %>% print(n = Inf) 
    
# Merge contrast IDs of NMA analysis database to OSF database
    NMA_data_analysis_subset_revid <- NMA_data_analysis_subset %>% dplyr::select(record_id, contrast_id, es_id, simple_number, effect_size)
    NMA_data_analysis_subset_revid %>% print(n = Inf) 
    
    OSF_Data_revise_es_ids <- OSF_Data_revise_es_ids %>% rename(contrast_id_old = contrast_id)
    OSF_Data_revise_ids <- OSF_Data_revise_es_ids %>% full_join(NMA_data_analysis_subset_revid, by = c("record_id", "es_id"), suffix = c("_osf_base", "_nma_base"))
    OSF_Data_revise_ids %>% count()
    
    OSF_Data_revise_ids$simple_number <- as.character(OSF_Data_revise_ids$simple_number)
    OSF_Data_revise_ids_check <- OSF_Data_revise_ids %>% dplyr::select(record_id, simple_number, contrast_id_old, contrast_id, es_id_old, es_id, effect_size_osf_base, effect_size_nma_base)
    OSF_Data_revise_ids_check <- OSF_Data_revise_ids_check %>% arrange(record_id, contrast_id_old, contrast_id, es_id, es_id_old, effect_size_osf_base, effect_size_nma_base)
    OSF_Data_revise_ids_check %>% print(n = Inf) # Check that: i) simple number from NMA equals contrast_id_old (that's what the OSF database was using for contrast ID); and ii) the effect size IDS match indicating the same observations.
    
# Export OSF database with revised effect size IDs and contrast IDs
    
    ## Clean up OSF database with revised IDs to resemble original OSF database
    OSF_Data_revise_ids <- OSF_Data_revise_ids %>% dplyr::select(!c(es_id_old, contrast_id_old, simple_number, effect_size_nma_base))
    OSF_Data_revise_ids <- OSF_Data_revise_ids %>% rename(effect_size = effect_size_osf_base)
    OSF_Data_revise_ids <- OSF_Data_revise_ids %>% dplyr::select(record_id, contrast_id, es_id, everything())
    
    ## Export as .csv
    write_csv(OSF_Data_revise_ids, "OSF/OSF_NMA Database_revised ids.csv")
  