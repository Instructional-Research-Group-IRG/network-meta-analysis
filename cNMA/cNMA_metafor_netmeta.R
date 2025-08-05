# This script performs a component network meta-analysis (NMA) of mathematics education interventions using outcomes in the whole and ration numbers domains. 
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
  cNMA_data_4.1 <- read_csv('cNMA_data_4.1.csv')

# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: intervention_content == "Whole Numbers (W)"

  ## Subset analysis data frame further to just the Whole Numbers (W) intervention content (icW)
  tabyl(cNMA_data_4.1$intervention_content)
  tabyl(cNMA_data_4.1$domain)
  NMA_data_analysis_subset_grpID_icW <- NMA_data_analysis_subset_grpID %>% filter(intervention_content == "W")
  tabyl(NMA_data_analysis_subset_grpID_icW$intervention_content)
  NMA_data_analysis_subset_grpID_icW_c <- NMA_data_analysis_subset_grpID_icW %>% distinct(contrast_id, .keep_all = TRUE)
  NMA_data_analysis_subset_grpID_icW_c %>% count()