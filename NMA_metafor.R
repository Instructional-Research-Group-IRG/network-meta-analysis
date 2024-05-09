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
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor, naniar, igraph, multcomp, broom, gridExtra, ggplot2, writexl, readr)

# Load (read) data (i.e., copy data to 'dat')
  #dat <- read_sheet("https://docs.google.com/spreadsheets/d/1bWugw06yFyetIVYlhAHHzM_d3KGhegxxLBm-5463j2Q/edit#gid=0") #Test data
  NMA_data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set
  NMA_data %>% count()
  tabyl(NMA_data$record_id)
  ##NMA_data <- subset(NMA_data, !is.na(record_id)) #There are no true rows after row 608 (not counting the headers/column names as row 1). Those after row 608 are loaded in despite having no record IDs because column C is filled out with "FALSE" after row 608 (artifact of the Excel function in the cells of that column).
  ##NMA_data %>% count()
  ##assert_values(NMA_data, colnames= c("record_id"), test = "not_na", test_val = "NA")
  
# Explore data  
  head(NMA_data)
  skim(NMA_data)
  
  ## Check for full duplicates
  dups <- anyDuplicated(NMA_data)
  assert("assert no duplicate entries", dups == 0) #No full duplicates. Data already in wide format.

  ## Review outcome measures by domain
  NMA_data %>% count(domain, measure_name) %>% print(n= Inf)
    
  ## Check ratings
  NMA_data %>% group_by(wwc_rating) %>% count() %>% ungroup()
  NMA_data %>% group_by(domain, wwc_rating) %>% count() %>% ungroup() %>% print(n= Inf)
  
  ## Replace all NA values in the moderators with 0 to avoid the "Processing terminated since k <= 1" error when including as moderators in the rma.mv function below executing the NMA.
  #NMA_data <- NMA_data %>% replace_na(list(NL_TX = 0, EX_TX = 0, VF_TX = 0, FF_TX = 0, RS_TX = 0))
 
# Subset data following NMA analysis specifications
  
  ## Tabulate variables upon which to subset data
  tabyl(NMA_data$aggregated)
  tabyl(NMA_data$measure_type)
  tabyl(NMA_data$wwc_rating)  
  tabyl(NMA_data$intervention_prelim)    
  tabyl(NMA_data$comparison_prelim) 
  
  ## Subset data for analysis 
  NMA_data_analysis_subset <- subset(NMA_data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") & aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") & TvsT==0)
  
  ## Retabulate variables upon which to subset data to verify correct subset
  tabyl(NMA_data_analysis_subset$aggregated)
  tabyl(NMA_data_analysis_subset$measure_type)
  tabyl(NMA_data_analysis_subset$wwc_rating)  
  tabyl(NMA_data_analysis_subset$intervention_prelim)    
  tabyl(NMA_data_analysis_subset$comparison_prelim) 

# Create unique group ID for each independent group within a study (record ID)
  
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
  NMA_data_grpID <- NMA_data_grpID %>% unite("com_n_chr_bundle" , c(comparison,comparison_n_chr), remove = FALSE)
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
  
  ## Correct variable types
  convert_to_factor <- function(x) {
    as.factor(x)
  }    
  NMA_data_analysis_subset_grpID[c("intervention_prelim","comparison_prelim")] <- lapply(NMA_data_analysis_subset_grpID[c("intervention_prelim","comparison_prelim")], convert_to_factor)
  
  ## Add contrast matrix to dataset
  NMA_data_analysis_subset_grpID <- NMA_data_analysis_subset_grpID %>% drop_na(c(intervention_prelim, comparison_prelim)) #Drop rows in the intervention and comparison columns with missing values (i.e., <NA>).
  NMA_data_analysis_subset_grpID <- contrmat(NMA_data_analysis_subset_grpID, grp1="intervention_prelim", grp2="comparison_prelim")
  #contrast_matrix_check <- NMA_data_analysis_subset_grpID %>% dplyr::select(intervention_prelim,comparison_prelim,EX:BAU)
  contrast_matrix_check <- NMA_data_analysis_subset_grpID %>% dplyr::select(intervention_prelim,comparison_prelim,NL_TX:BAU) #To-do: Update once binary columns are finalized in sheet data.
  print(contrast_matrix_check)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID)
  V_list   
   
  ##Additional modifications to data to facilitate running NMA with metafor
  class(NMA_data_analysis_subset_grpID$contrast_id) 
  NMA_data_analysis_subset_grpID$contrast_id <- as.character(NMA_data_analysis_subset_grpID$contrast_id)
  tabyl(NMA_data_analysis_subset_grpID$contrast_id)
  class(NMA_data_analysis_subset_grpID$contrast_id)
  
  class(NMA_data_analysis_subset_grpID$dosage_weekly_freq)
  NMA_data_analysis_subset_grpID$dosage_weekly_freq <- as.character(NMA_data_analysis_subset_grpID$dosage_weekly_freq)
  tabyl(NMA_data_analysis_subset_grpID$dosage_weekly_freq)
  class(NMA_data_analysis_subset_grpID$dosage_weekly_freq) 
  
# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition 
  
  ## Model notes: setting rho=0.60; tau^2 reflects the amount of heterogeneity for all treatment comparisons
  
  ##Run preliminary standard NMA without moderators
  res <- rma.mv(effect_size, V_list,
         random = ~ 1 | record_id/es_id, 
         rho=0.60, 
         data=NMA_data_analysis_subset_grpID)
  summary(res)
  #weights.rma.mv(res)
  forest(res)

  ##Run standard NMA with the unique interventions bundles as moderators  
  tabyl(NMA_data_analysis_subset_grpID$intervention_prelim)
  res_mod <- rma.mv(effect_size, V_list, 
                     #mods = ~ intervention_prelim - 1,
                     #mods = ~ EX.FF.RS + EX.RS + EX.VF.FF.RS + EX.VF.RS + FF.RS + NL.EX.FF.RS + NL.EX.RS + NL.EX.VF.RS + NL.FF.RS + NL.RS + RS + VF.RS - 1, 
                     mods = ~ FF + FF.RS + NL.FF.RS + NL.RS + NL.TES.FF.RS + NL.TES.RS + NL.TES.VF.RS + RS + TES.VF.RS + VF.FF.RS + VF.RS - 1, 
                     random = ~ 1 | record_id/es_id, 
                     rho=0.60, 
                     data=NMA_data_analysis_subset_grpID)
  summary(res_mod)
  #weights.rma.mv(res_mod)
    
    ### Estimate all pairwise differences between treatments
    contr <- data.frame(t(combn(names(coef(res_mod)), 2)))
    contr <- contrmat(contr, "X1", "X2")
    rownames(contr) <- paste(contr$X1, "-", contr$X2)
    contr <- as.matrix(contr[-c(1:2)])
    sav <- predict(res_mod, newmods=contr)
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
    lt_info_df2$ci.lb <- paste("(", lt_info_df2$ci.lb, " -", sep= "")
    lt_info_df2$ci.ub <- paste(lt_info_df2$ci.ub, ")", sep= "")
    lt_info_df2 <- lt_info_df2 %>% unite(pred_cis, pred, ci.lb, ci.ub, sep= " ", remove = FALSE )
    print(lt_info_df2)
    lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis")
    lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
    print(lt_info_df3)
    write_csv(lt_info_df3, 'league_table.csv')
    #write_xlsx(lt_info_df3, 'league_table.xlsx')
    
    ### Compute p-values
    contr <- data.frame(t(combn(c(names(coef(res_mod)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
    contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
    b <- c(coef(res_mod),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    vb <- bldiag(vcov(res_mod),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
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
    res_mod_df <- tidy(res_mod, conf.int = TRUE)
    pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
    res_mod_pscore <- res_mod_df %>% left_join(pscores_df, by = c("term"))
    res_mod_pscore <- res_mod_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
    res_mod_pscore
    
    ### Create forest plot using metafor's built-in function
    forest(coef(res_mod), diag(vcov(res_mod)), slab=sub(".", " ", names(coef(res_mod)), fixed=TRUE),
           #xlim=c(-5,5), alim=c(-3,3), psize=6, header="Intervention", top=2,
           header="Intervention",
           xlab="Difference in Standardized Mean Change (compared to BAU)")
    
    ### Create forest plot using ggplot
      
      #### First create plot of estimates and confidence intervals
      #res_mod_pscore$colour <- rep(c("white", "gray95"), 5.5)
      res_mod_pscore
      res_mod_pscore_forest <- ggplot(res_mod_pscore, aes(x= estimate, y= intervention, xmin= ci.lb, xmax= ci.ub)) + 
        #geom_hline(aes(yintercept = intervention, colour = colour), size=7) +
        geom_hline(aes(yintercept = intervention), size=7) +
        geom_pointrange(shape = 22, fill = "black") +
        geom_vline(xintercept = 1, linetype = 3) +
        xlab("Difference in Standardized Mean Change (compared to BAU) with 95% Confidence Interval") +
        ylab("Intervention Bundle") +
        theme_classic() +
        scale_colour_identity() +
        scale_y_discrete(limits = rev(res_mod_pscore$intervention)) +
        #scale_x_log10(limits = c(-1.25, 2.25), 
                      #breaks = c(0.25, 0.5, 1, 2, 4), 
                      #labels = c("0.25", "0.5", "1", "2", "4"), expand = c(0,0)) +
        theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      res_mod_pscore_forest
      
      #### Next create data table for merging with above plot with estimates and confidence intervals combined in one column
      res_mod_pscore2 <- res_mod_pscore
      round_digits <- function(x) {
        round(x, digits = 2)
      }
      convert_to_character <- function(x) {
        as.character(x)
      }
      res_mod_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], round_digits)
      res_mod_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], as.character)
      res_mod_pscore2$ci.lb <- paste("(", res_mod_pscore2$ci.lb, " -", sep= "")
      res_mod_pscore2$ci.ub <- paste(res_mod_pscore2$ci.ub, ")", sep= "")
      res_mod_pscore2 <- res_mod_pscore2 %>% unite(estimate_cis, estimate, ci.lb, ci.ub, sep= " ", remove = FALSE )
      print(res_mod_pscore2)
    
      LfLabels<-data.frame(x=c(0,4.1,6.5),
                           y=c(rep(length(unique(res_mod_pscore2$estimate))-0.2,times=3)),
                           lab=c("Intervention","Estimate (95% CI)","P-score"))
      LfLabels
      
      data_table <- ggplot(data = res_mod_pscore2, aes(y = intervention)) +
        #geom_hline(aes(yintercept = intervention, colour = colour), size = 7) +
        geom_hline(aes(yintercept = intervention), size = 7) +
        geom_text(aes(x = 0, label = intervention), hjust = 0) +
        geom_text(aes(x = 5, label = estimate_cis)) +
        geom_text(aes(x = 7, label = Pscore), hjust = 1) +
        geom_text(data=LfLabels,aes(x,y,label=lab, fontface="bold"), vjust=-10, hjust=0, size=4, size=4) +
        scale_colour_identity() +
        theme_void() + 
        theme(plot.margin = margin(5, 0, 35, 0)) +
        scale_y_discrete(limits = rev(res_mod_pscore$intervention)) 
      data_table
      
      #### Finally, merge plot and datatable for final forest plot
      final_fp_nma <- grid.arrange(data_table, res_mod_pscore_forest, ncol=2)
      final_fp_nma
    
    ### Create network graph
      
    ### Note: The data are currently in a contrast-based, "wide" format in which each contrast is an observation 
    ###       (i.e., intervention and comparison assignments of each contrast are in separate columns of the same row.)
    ###       Need to reshape the data "long" to an arm-based format in which the two assignment groups (intervention/comparison)
    ###       are in separate rows of the same column grouped by contrast ID across those rows. This facilitates the pairing of intervention/comparison 
    ###       groups for the creation of the (weighted) edges of the network graph.
      
      ####Review data in contrast-based wide format before reshape for comparison with data after reshape (as a desk check)
      NMA_data_analysis_subset$contrast_id <- as.character(NMA_data_analysis_subset$contrast_id)
      NMA_data_analysis_subset2 <- NMA_data_analysis_subset %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset2) #Example rows of the contrast-based wide format. Compare to the long format printed below.
      
      ####Each unique contrast within each unique study should only be counted once when weighting the network connections between each unique contrast combination of intervention and comparison in the network graph.
      ####Because there can be multiple measures within multiple domains within each unique contrast within each unique study, we need reduce the data set to one observation per unique contrast within each unique study 
      #### so that a contrast with more domains/measures than another contrast is not overweighted in the visualization of the network connections (re the relative thicknesses of the "edges" between the I/C nodes in the network graph).
      NMA_data_analysis_subset3 <- NMA_data_analysis_subset %>% distinct(record_id, contrast_id, .keep_all = TRUE)
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset3 %>% dplyr::select(record_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size)
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset4 %>% unite("int_comp_prelim" , c(intervention_prelim,comparison_prelim), remove = FALSE)
      tabyl(NMA_data_analysis_subset4$int_comp_prelim) #The relative number of observations (n) for the intervention_BAU contrasts in this table should match the relative thicknesses of the lines (called "edges") in the network graph.
      
      ####Reshape data to arm-based long format
      NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset3, c(intervention_prelim, comparison_prelim), names_to = "assignment_I_C", values_to = "intervention_comparison")
      
      ####Review data in arm-based long format after reshape for comparison with data before reshape (as a desk check)
      NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% dplyr::select(record_id, contrast_id, assignment_I_C, intervention_comparison, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset_long2) #Example rows of the arm-based long format. Compare to the wide format printed above.
      
      ####Create the table of intervention/comparison pairs for creating the network graph with igraph
      dat_igraph <- NMA_data_analysis_subset_long
      dat_igraph$contrast_id <- as.character(dat_igraph$contrast_id)
      pairs <- data.frame(do.call(rbind, lapply(split(dat_igraph$intervention_comparison, dat_igraph$contrast_id), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
      print(pairs)
      pairs$X1 <- factor(pairs$X1, levels=sort(unique(dat_igraph$intervention_comparison)))
      pairs$X2 <- factor(pairs$X2, levels=sort(unique(dat_igraph$intervention_comparison)))
      tab <- table(pairs[,1], pairs[,2])
      tab
      
      ####Creating the network graph with igraph
      set.seed(3524)
      g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
      plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
           layout=layout_in_circle(g),
           #layout=layout_nicely(g),
           #layout=layout_with_lgl(g),
           vertex.size=20, vertex.color="lightgray", vertex.label.color="black", vertex.label.font=2)  
   
      