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
  pacman::p_load(metafor, googlesheets4, igraph, network, tidyr, dplyr, janitor, assertable)

# Load and subset data
  
  ##Load (read in) data
  NMA_data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set
  NMA_data %>% count()
  tabyl(NMA_data$record_id)
  ##NMA_data <- subset(NMA_data, !is.na(record_id)) #There are no true rows after row 608 (not counting the headers/column names as row 1). Those after row 608 are loaded in despite having no record IDs because column C is filled out with "FALSE" after row 608 (artifact of the Excel function in the cells of that column).
  ##NMA_data %>% count()
  ##assert_values(NMA_data, colnames= c("record_id"), test = "not_na", test_val = "NA")
  
  ## Subset data for analysis 
  NMA_data_analysis_subset <- subset(NMA_data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") &
                                       aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") &
                                       comparison_prelim=="BAU" & (NL_TX==1 | EX_TX==1 | VF_TX==1 | FF_TX==1 | RS_TX==1 )) 

# Create network graph
  
  ## Use the igraph package
  
  ### Note: The data are currently in a contrast-based, "wide" format in which each contrast is an observation 
  ###       (i.e., intervention and comparison assignments of each contrast are in separate columns of the same row.)
  ###       Need to reshape the data "long" to an arm-based format in which the two assignment groups (intervention/comparison)
  ###       are in separate rows of the same column grouped by contrast ID across those rows. This facilitates the pairing of intervention/comparison 
  ###       groups for the creation of the (weighted) edges of the network graph.
    
    ###Review data in contrast-based wide format before reshape for comparison with data after reshape (as a desk check)
    NMA_data_analysis_subset$contrast_id <- as.character(NMA_data_analysis_subset$contrast_id)
    NMA_data_analysis_subset2 <- NMA_data_analysis_subset %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size)
    print(NMA_data_analysis_subset2) #Example rows of the contrast-based wide format. Compare to the long format printed below.
    
    ###Each unique contrast within each unique study should only be counted once when weighting the network connections between each unique contrast combination of intervention and comparison in the network graph.
    ###Because there can be multiple measures within multiple domains within each unique contrast within each unique study, we need reduce the data set to one observation per unique contrast within each unique study 
    ### so that a contrast with more domains/measures than another contrast is not overweighted in the visualization of the network connections (re the relative thicknesses of the "edges" between the I/C nodes in the network graph).
    NMA_data_analysis_subset3 <- NMA_data_analysis_subset %>% distinct(record_id, contrast_id, .keep_all = TRUE)
    NMA_data_analysis_subset4 <- NMA_data_analysis_subset3 %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size)
    NMA_data_analysis_subset4 <- NMA_data_analysis_subset4 %>% unite("int_comp_prelim" , c(intervention_prelim,comparison_prelim), remove = FALSE)
    tabyl(NMA_data_analysis_subset4$int_comp_prelim) #The relative number of observations (n) for the intervention_BAU contrasts in this table should match the relative thicknesses of the lines (called "edges") in the network graph.
    
    ###Reshape data to arm-based long format
    NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset3, c(intervention_prelim, comparison_prelim), names_to = "assignment_I_C", values_to = "intervention_comparison")
    
    ###Review data in arm-based long format after reshape for comparison with data before reshape (as a desk check)
    NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% dplyr::select(record_id, contrast_id, assignment_I_C, intervention_comparison, domain, measure_name, es_id, effect_size)
    print(NMA_data_analysis_subset_long2) #Example rows of the arm-based long format. Compare to the wide format printed above.
    
    ###Create the table of intervention/comparison pairs for creating the network graph with igraph
    dat_igraph <- NMA_data_analysis_subset_long
    dat_igraph$contrast_id <- as.character(dat_igraph$contrast_id)
    pairs <- data.frame(do.call(rbind, lapply(split(dat_igraph$intervention_comparison, dat_igraph$contrast_id), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
    print(pairs)
    pairs$X1 <- factor(pairs$X1, levels=sort(unique(dat_igraph$intervention_comparison)))
    pairs$X2 <- factor(pairs$X2, levels=sort(unique(dat_igraph$intervention_comparison)))
    tab <- table(pairs[,1], pairs[,2])
    tab
    
    ###Creating the network graph with igraph
    set.seed(3524)
    g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
    plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
         layout=layout_in_circle(g),
         #layout=layout_nicely(g),
         #layout=layout_with_lgl(g),
         vertex.size=20, vertex.color="lightgray", vertex.label.color="black", vertex.label.font=2)  

  ## Use the network package
  
  ### Note: Creating the network graph with igraph seems to work just fine. No need currently to adapt using the network package.