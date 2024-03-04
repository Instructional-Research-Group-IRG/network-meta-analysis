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
  pacman::p_load(metafor, igraph, network, tidyr)

# Load (read) data (i.e., copy data to 'dat')
  #dat <- read_sheet("https://docs.google.com/spreadsheets/d/1bWugw06yFyetIVYlhAHHzM_d3KGhegxxLBm-5463j2Q/edit#gid=0") #Test data
  NMA_data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set
  NMA_data %>% count()
  tabyl(NMA_data$record_id)
  NMA_data <- subset(NMA_data, !is.na(record_id)) #There are no true rows after row 608 (not counting the headers/column names as row 1). Those after row 608 are loaded in despite having no record IDs because column C is filled out with "FALSE" after row 608 (artifact of the Excel function in the cells of that column).
  NMA_data %>% count()
  assert_values(NMA_data, colnames= c("record_id"), test = "not_na", test_val = "NA")
  
  ## Subset data for analysis 
  NMA_data_analysis_subset <- subset(NMA_data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") &
                                       aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") &
                                       comparison_prelim=="BAU" & (NL_TX==1 | SE_TX==1 | VF_TX==1 | F_TX==1 | BX_TX==1 | RS_TX==1 )) 
  
# Create network graph
  
  ## Use the igraph package
  NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset, c(intervention_prelim, comparison_prelim), names_to = "assignment_T_C", values_to = "treatment")
  
  NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% select(record_id, assignment_T_C, treatment)
  
  dat_igraph <- NMA_data_analysis_subset_long2
  pairs <- data.frame(do.call(rbind, sapply(split(dat_igraph$treatment, dat_igraph$record_id), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
  pairs$X1 <- factor(pairs$X1, levels=sort(unique(dat_igraph$treatment)))
  pairs$X2 <- factor(pairs$X2, levels=sort(unique(dat_igraph$treatment)))
  tab <- table(pairs[,1], pairs[,2])
  tab
  
  g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
  g <- graph_from_adjacency_matrix(tab, mode = "plus",                diag=FALSE)
  g <- graph_from_adjacency_matrix(tab,                               diag=FALSE)
  g <- graph_from_adjacency_matrix(tab, mode = "max")
  plot(g, edge.curved=FALSE, edge.width=E(g)$weight/2,
       layout=layout_in_circle(g),
       vertex.size=25, vertex.color="lightgray", vertex.label.color="black", vertex.label.font=2)  
  
  ## Use the network package
  
  