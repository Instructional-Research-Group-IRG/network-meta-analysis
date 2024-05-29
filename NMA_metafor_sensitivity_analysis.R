# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: domain == "General Mathematics Achievement"

  ## Subset analysis data frame further to just the General Mathematics Achievement domain (d1gmaSA)
  tabyl(NMA_data_analysis_subset_grpID_alldomains$domain)
  NMA_data_analysis_subset_grpID_d1gmaSA <- NMA_data_analysis_subset_grpID_alldomains %>% filter(domain == "General Mathematics Achievement")
  tabyl(NMA_data_analysis_subset_grpID_d1gmaSA$domain)
  
  ## Model notes: setting rho=0.60; tau^2 reflects the amount of heterogeneity for all treatment comparisons
  
  ## Add contrast matrix to dataset
  NMA_data_analysis_subset_grpID_d1gmaSA <- NMA_data_analysis_subset_grpID_d1gmaSA %>% drop_na(c(intervention_combo, comparison_combo)) #Drop rows in the intervention and comparison columns with missing values (i.e., <NA>).
  NMA_data_analysis_subset_grpID_d1gmaSA <- contrmat(NMA_data_analysis_subset_grpID_d1gmaSA, grp1="intervention_combo", grp2="comparison_combo")
  str(NMA_data_analysis_subset_grpID_d1gmaSA)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_d1gmaSA)
  V_list     
  
  ## Calculate the number of unique contrasts in which each intervention bundle is included
  tabyl(NMA_data_analysis_subset_grpID_d1gmaSA$intervention_combo)
  tabyl(NMA_data_analysis_subset_grpID_d1gmaSA$comparison_combo)
  num_contrasts_d1gmaSA <- NMA_data_analysis_subset_grpID_d1gmaSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo)
  print(num_contrasts_d1gmaSA)
  num_contrasts_d1gmaSA_long <- num_contrasts_d1gmaSA %>% pivot_longer(c(intervention_combo, comparison_combo ), names_to= "group_IC", values_to="group_intervention")
  print(num_contrasts_d1gmaSA_long)
  num_contrasts_d1gmaSA_long2 <- num_contrasts_d1gmaSA_long %>% distinct(record_id, contrast_id, group_intervention, .keep_all = TRUE)
  print(num_contrasts_d1gmaSA_long2)
  tabyl(num_contrasts_d1gmaSA_long2$contrast_id) #Should be n=2 for each contrast if reshape and distinct steps done correctly: 1 intervention & 1 comparison per unique contrast. 
  num_contrasts_d1gmaSA_long3 <- tabyl(num_contrasts_d1gmaSA_long2$group_intervention)
  num_contrasts_d1gmaSA_long3 <- num_contrasts_d1gmaSA_long3 %>% dplyr::select(intervention= 'num_contrasts_d1gmaSA_long2$group_intervention', num_contrasts= 'n')
  str(num_contrasts_d1gmaSA_long3)
  num_contrasts_d1gmaSA_long3$intervention <- as.character(num_contrasts_d1gmaSA_long3$intervention)
  num_contrasts_d1gmaSA_long3$intervention <- gsub("\\+", ".", num_contrasts_d1gmaSA_long3$intervention)
  str(num_contrasts_d1gmaSA_long3)
  print(num_contrasts_d1gmaSA_long3)  
  
  ##Run standard NMA with the unique interventions bundles as moderators  
  tabyl(NMA_data_analysis_subset_grpID_d1gmaSA$intervention_combo)
  tabyl(NMA_data_analysis_subset_grpID_d1gmaSA$comparison_combo)
  check_d1gmaSA <- NMA_data_analysis_subset_grpID_d1gmaSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo)
  print(check_d1gmaSA)
  res_mod_d1gmaSA <- rma.mv(effect_size, V_list, 
                          mods = ~ FF.N.RV + FF.RS.N + FF.RS.N.SEO + FF.RV + NL.RS + RS + RS.N + RS.N.FWOF + RS.N.SEO + RS.N.TV + RS.SEO + RS.TV.FWOF + VF.RS.N.SEO - 1, 
                          random = ~ 1 | record_id/es_id, 
                          rho=0.60, 
                          data=NMA_data_analysis_subset_grpID_d1gmaSA)
  summary(res_mod_d1gmaSA)
  #weights.rma.mv(res_mod_d1gmaSA)
  
    ### Estimate all pairwise differences between treatments
    contr <- data.frame(t(combn(names(coef(res_mod_d1gmaSA)), 2)))
    contr <- contrmat(contr, "X1", "X2")
    rownames(contr) <- paste(contr$X1, "-", contr$X2)
    contr <- as.matrix(contr[-c(1:2)])
    sav <- predict(res_mod_d1gmaSA, newmods=contr)
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
    lt_info_df2$ci.lb <- paste("(", lt_info_df2$ci.lb, " ,", sep= "")
    lt_info_df2$ci.ub <- paste(lt_info_df2$ci.ub, ")", sep= "")
    lt_info_df2 <- lt_info_df2 %>% unite(pred_cis, pred, ci.lb, ci.ub, sep= " ", remove = FALSE )
    print(lt_info_df2)
    lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #To-do: possible to format ci below? + color code by sig
    lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
    print(lt_info_df3)
    write_csv(lt_info_df3, 'nma_league_table_d1gmaSA.csv')
    #write_xlsx(lt_info_df3, 'nma_league_table_d1gmaSA.xlsx')
    
    ### Compute p-values
    contr <- data.frame(t(combn(c(names(coef(res_mod_d1gmaSA)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
    contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
    b <- c(coef(res_mod_d1gmaSA),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    vb <- bldiag(vcov(res_mod_d1gmaSA),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
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
    res_mod_d1gmaSA_df <- tidy(res_mod_d1gmaSA, conf.int = TRUE)
    pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
    res_mod_d1gmaSA_pscore <- res_mod_d1gmaSA_df %>% left_join(pscores_df, by = c("term"))
    res_mod_d1gmaSA_pscore <- res_mod_d1gmaSA_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
    res_mod_d1gmaSA_pscore
    
    ### Create forest plot using metafor's built-in function
    # forest(coef(res_mod_d1gmaSA), diag(vcov(res_mod_d1gmaSA)), slab=sub(".", " ", names(coef(res_mod_d1gmaSA)), fixed=TRUE),
    #        #xlim=c(-5,5), alim=c(-3,3), psize=6, header="Intervention", top=2,
    #        header="Intervention",
    #        xlab="Difference in Standardized Mean Change")
    
    ### Create forest plot using ggplot
    
      #### First create plot of estimates and confidence intervals
      res_mod_d1gmaSA_pscore <- res_mod_d1gmaSA_pscore %>% arrange(desc(Pscore))
      str(res_mod_d1gmaSA_pscore)
      print(res_mod_d1gmaSA_pscore)
      print(num_contrasts_d1gmaSA_long3)
      res_mod_d1gmaSA_pscore <- res_mod_d1gmaSA_pscore %>% left_join(num_contrasts_d1gmaSA_long3, by = "intervention") # Merge on number of unique contrasts in which each intervention bundle is included
      res_mod_d1gmaSA_pscore$colour <- rep(c("white", "gray95","white", "gray95","white","gray95","white", "gray95","white","gray95","white", "gray95","white"))
      str(res_mod_d1gmaSA_pscore)
      print(res_mod_d1gmaSA_pscore)
      
      res_mod_d1gmaSA_pscore_forest <- ggplot(res_mod_d1gmaSA_pscore, aes(x= estimate, y= intervention, xmin= ci.lb, xmax= ci.ub)) + 
        geom_hline(aes(yintercept = intervention, colour = colour), size=7) +
        geom_pointrange(shape = 22, fill = "black", size = res_mod_d1gmaSA_pscore$num_contrasts/7.5) + 
        geom_text(label = res_mod_d1gmaSA_pscore$num_contrasts, hjust = -1.25, vjust = 1, colour = "black", fontface="bold", size =3) +        
        geom_vline(xintercept = 0, linetype = 3) +
        xlab("Difference in Standardized Mean Change with 95% Confidence Interval") +
        labs(caption = "*Size of and values next to points indicate the number of contrasts in which intervention is included", hjust=0) +
        ylab("Intervention Bundle") +
        theme_classic() +
        scale_colour_identity() +
        scale_y_discrete(limits = rev(res_mod_d1gmaSA_pscore$intervention)) +
        #scale_x_log10(limits = c(-1.25, 2.25), 
        #breaks = c(0.25, 0.5, 1, 2, 4), 
        #labels = c("0.25", "0.5", "1", "2", "4"), expand = c(0,0)) +
        theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      res_mod_d1gmaSA_pscore_forest
      
      #### Next create data table for merging with above plot with estimates and confidence intervals combined in one column
      res_mod_d1gmaSA_pscore2 <- res_mod_d1gmaSA_pscore
      round_digits <- function(x) {
        round(x, digits = 2)
      }
      convert_to_character <- function(x) {
        as.character(x)
      }
      res_mod_d1gmaSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_d1gmaSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], round_digits)
      res_mod_d1gmaSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_d1gmaSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], as.character)
      res_mod_d1gmaSA_pscore2$ci.lb <- paste("(", res_mod_d1gmaSA_pscore2$ci.lb, " -", sep= "")
      res_mod_d1gmaSA_pscore2$ci.ub <- paste(res_mod_d1gmaSA_pscore2$ci.ub, ")", sep= "")
      res_mod_d1gmaSA_pscore2 <- res_mod_d1gmaSA_pscore2 %>% unite(estimate_cis, estimate, ci.lb, ci.ub, sep= " ", remove = FALSE )
      print(res_mod_d1gmaSA_pscore2)
      
      LfLabels<-data.frame(x=c(0,4.5,6.7), 
                           y=c(rep(length(unique(res_mod_d1gmaSA_pscore2$estimate))-0.2,times=3)),
                           lab=c("Intervention","Estimate (95% CI)","P-score"))
      LfLabels
      
      data_table <- ggplot(data = res_mod_d1gmaSA_pscore2, aes(y = intervention)) +
        geom_hline(aes(yintercept = intervention, colour = colour), size = 7) +
        geom_text(aes(x = 0, label = intervention), hjust = 0) +
        geom_text(aes(x = 5, label = estimate_cis)) +
        geom_text(aes(x = 7, label = Pscore), hjust = 1) +
        geom_text(data=LfLabels,aes(x,y,label=lab, fontface="bold"), vjust=-3.5, hjust=0, size=4, size=4) +
        scale_colour_identity() +
        theme_void() + 
        theme(plot.margin = margin(5, 0, 35, 0)) +
        scale_y_discrete(limits = rev(res_mod_d1gmaSA_pscore$intervention)) 
      data_table
      
      #### Finally, merge plot and datatable for final forest plot
      final_fp_nma_d1gmaSA <- grid.arrange(data_table, res_mod_d1gmaSA_pscore_forest, ncol=2)
      final_fp_nma_d1gmaSA
    
    ### Create network graph
      
    ### Note: The data are currently in a contrast-based, "wide" format in which each contrast is an observation 
    ###       (i.e., intervention and comparison assignments of each contrast are in separate columns of the same row.)
    ###       Need to reshape the data "long" to an arm-based format in which the two assignment groups (intervention/comparison)
    ###       are in separate rows of the same column grouped by contrast ID across those rows. This facilitates the pairing of intervention/comparison 
    ###       groups for the creation of the (weighted) edges of the network graph.
    
      #### Review data in contrast-based wide format before reshape for comparison with data after reshape (as a desk check)
      NMA_data_analysis_subset2 <- NMA_data_analysis_subset_grpID_d1gmaSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset2) #Example rows of the contrast-based wide format. Compare to the long format printed below.
      
      #### Each unique contrast within each unique study should only be counted once when weighting the network connections between each unique contrast combination of intervention versus comparison in the network graph.
      #### Because there can be multiple measures within multiple domains within each unique contrast within each unique study, we need reduce the data set to one observation per unique contrast within each unique study 
      #### so that a contrast with more domains/measures than another contrast is not overweighted in the visualization of the network connections (re the relative thicknesses of the "edges" between the I/C nodes in the network graph).
      NMA_data_analysis_subset_grpID_d1gmaSA %>% count(record_id, contrast_id)
      NMA_data_analysis_subset3 <- NMA_data_analysis_subset_grpID_d1gmaSA %>% distinct(record_id, contrast_id, .keep_all = TRUE)
      NMA_data_analysis_subset3 %>% count(record_id, contrast_id)
      NMA_data_analysis_subset3 %>% count()
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset3 %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo, domain, measure_name, es_id, effect_size)
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset4 %>% unite("int_comp_prelim" , c(intervention_combo,comparison_combo), remove = FALSE)
      tabyl(NMA_data_analysis_subset4$int_comp_prelim) #The relative number of observations (n) for the intervention_BAU contrasts in this table should match the relative thicknesses of the lines (called "edges") in the network graph.
      
      #### Reshape data to arm-based long format
      NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset3, c(intervention_combo, comparison_combo), names_to = "assignment_I_C", values_to = "intervention_comparison")
      
      #### Review data in arm-based long format after reshape for comparison with data before reshape (as a desk check)
      NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% dplyr::select(record_id, contrast_id, assignment_I_C, intervention_comparison, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset_long2) #Example rows of the arm-based long format. Compare to the wide format printed above.
      
      #### Create the table of intervention/comparison pairs for creating the network graph with igraph
      dat_igraph <- NMA_data_analysis_subset_long
      dat_igraph$intervention_comparison <- as.character(dat_igraph$intervention_comparison)
      pairs <- data.frame(do.call(rbind, lapply(split(dat_igraph$intervention_comparison, dat_igraph$contrast_id), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
      print(pairs)
      pairs$X1 <- factor(pairs$X1, levels=sort(unique(dat_igraph$intervention_comparison)))
      pairs$X2 <- factor(pairs$X2, levels=sort(unique(dat_igraph$intervention_comparison)))
      tab <- table(pairs[,1], pairs[,2])
      tab
      
      #### Creating the network graph with igraph
      set.seed(3524)
      g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
      plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
           layout=layout_in_circle(g),
           #layout=layout_nicely(g),
           #layout=layout_with_lgl(g),
           vertex.size=20, vertex.color="lightgray", vertex.label.color="black", vertex.label.font=2)  

# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: domain == "Rational Numbers"

  ## Subset analysis data frame further to just the Rational Numbers domain (d2rnSA)
  tabyl(NMA_data_analysis_subset_grpID_alldomains$domain)
  NMA_data_analysis_subset_grpID_d2rnSA <- NMA_data_analysis_subset_grpID_alldomains %>% filter(domain == "Rational Numbers")
  tabyl(NMA_data_analysis_subset_grpID_d2rnSA$domain)
  
  ## Model notes: setting rho=0.60; tau^2 reflects the amount of heterogeneity for all treatment comparisons
  
  ## Add contrast matrix to dataset
  NMA_data_analysis_subset_grpID_d2rnSA <- NMA_data_analysis_subset_grpID_d2rnSA %>% drop_na(c(intervention_combo, comparison_combo)) #Drop rows in the intervention and comparison columns with missing values (i.e., <NA>).
  NMA_data_analysis_subset_grpID_d2rnSA <- contrmat(NMA_data_analysis_subset_grpID_d2rnSA, grp1="intervention_combo", grp2="comparison_combo")
  str(NMA_data_analysis_subset_grpID_d2rnSA)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_d2rnSA)
  V_list
  
  ## Calculate the number of unique contrasts in which each intervention bundle is included
  tabyl(NMA_data_analysis_subset_grpID_d2rnSA$intervention_combo)
  tabyl(NMA_data_analysis_subset_grpID_d2rnSA$comparison_combo)
  num_contrasts_d2rnSA <- NMA_data_analysis_subset_grpID_d2rnSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo)
  print(num_contrasts_d2rnSA)
  num_contrasts_d2rnSA_long <- num_contrasts_d2rnSA %>% pivot_longer(c(intervention_combo, comparison_combo ),names_to= "group_IC", values_to="group_intervention")
  print(num_contrasts_d2rnSA_long)
  num_contrasts_d2rnSA_long2 <- num_contrasts_d2rnSA_long %>% distinct(record_id, contrast_id, group_intervention, .keep_all = TRUE)
  print(num_contrasts_d2rnSA_long2)
  tabyl(num_contrasts_d2rnSA_long2$contrast_id) #Should be n=2 for each contrast if reshape and distinct steps done correctly: 1 intervention & 1 comparison per unique contrast. 
  num_contrasts_d2rnSA_long3 <- tabyl(num_contrasts_d2rnSA_long2$group_intervention)
  num_contrasts_d2rnSA_long3 <- num_contrasts_d2rnSA_long3 %>% dplyr::select(intervention= 'num_contrasts_d2rnSA_long2$group_intervention', num_contrasts= 'n')
  str(num_contrasts_d2rnSA_long3)
  num_contrasts_d2rnSA_long3$intervention <- as.character(num_contrasts_d2rnSA_long3$intervention)
  num_contrasts_d2rnSA_long3$intervention <- gsub("\\+", ".", num_contrasts_d2rnSA_long3$intervention)
  str(num_contrasts_d2rnSA_long3)
  print(num_contrasts_d2rnSA_long3)
  
  ##Run standard NMA with the unique interventions bundles as moderators  
  tabyl(NMA_data_analysis_subset_grpID_d2rnSA$intervention_combo)
  tabyl(NMA_data_analysis_subset_grpID_d2rnSA$comparison_combo)
  check_d2rnSA <- NMA_data_analysis_subset_grpID_d2rnSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo)
  print(check_d2rnSA)
  res_mod_d2rnSA <- rma.mv(effect_size, V_list, 
                         mods = ~ NL.FF.RS + NL.FF.RS.SEO + NL.FF.RS.SEO.TV + NL.RS.FWOF + NL.RS.SEO.TV.FWOF + NL.TES.FF.RS.TV + NL.TES.RS.TV + NL.TES.VF.RS + RS.TV + TES.VF.RS.N - 1, 
                         random = ~ 1 | record_id/es_id, 
                         rho=0.60, 
                         data=NMA_data_analysis_subset_grpID_d2rnSA)
  summary(res_mod_d2rnSA)
  #weights.rma.mv(res_mod_d2rnSA)

    ### Estimate all pairwise differences between treatments
    contr <- data.frame(t(combn(names(coef(res_mod_d2rnSA)), 2)))
    contr <- contrmat(contr, "X1", "X2")
    rownames(contr) <- paste(contr$X1, "-", contr$X2)
    contr <- as.matrix(contr[-c(1:2)])
    sav <- predict(res_mod_d2rnSA, newmods=contr)
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
    lt_info_df2$ci.lb <- paste("(", lt_info_df2$ci.lb, " ,", sep= "")
    lt_info_df2$ci.ub <- paste(lt_info_df2$ci.ub, ")", sep= "")
    lt_info_df2 <- lt_info_df2 %>% unite(pred_cis, pred, ci.lb, ci.ub, sep= " ", remove = FALSE )
    print(lt_info_df2)
    lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #To-do: possible to format ci below? + color code by sig
    lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
    print(lt_info_df3)
    write_csv(lt_info_df3, 'nma_league_table_d2rnSA.csv')
    #write_xlsx(lt_info_df3, 'nma_league_table_d2rnSA.xlsx')
    
    ### Compute p-values
    contr <- data.frame(t(combn(c(names(coef(res_mod_d2rnSA)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
    contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
    b <- c(coef(res_mod_d2rnSA),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    vb <- bldiag(vcov(res_mod_d2rnSA),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
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
    res_mod_d2rnSA_df <- tidy(res_mod_d2rnSA, conf.int = TRUE)
    pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
    res_mod_d2rnSA_pscore <- res_mod_d2rnSA_df %>% left_join(pscores_df, by = c("term"))
    res_mod_d2rnSA_pscore <- res_mod_d2rnSA_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
    res_mod_d2rnSA_pscore
    
    ### Create forest plot using metafor's built-in function
    # forest(coef(res_mod_d2rnSA), diag(vcov(res_mod_d2rnSA)), slab=sub(".", " ", names(coef(res_mod_d2rnSA)), fixed=TRUE),
    #        #xlim=c(-5,5), alim=c(-3,3), psize=6, header="Intervention", top=2,
    #        header="Intervention",
    #        xlab="Difference in Standardized Mean Change")
    
    ### Create forest plot using ggplot

      #### First create plot of estimates and confidence intervals
      res_mod_d2rnSA_pscore <- res_mod_d2rnSA_pscore %>% arrange(desc(Pscore))
      str(res_mod_d2rnSA_pscore)
      print(res_mod_d2rnSA_pscore)
      print(num_contrasts_d2rnSA_long3)
      res_mod_d2rnSA_pscore <- res_mod_d2rnSA_pscore %>% left_join(num_contrasts_d2rnSA_long3, by = "intervention") # Merge on number of unique contrasts in which each intervention bundle is included
      res_mod_d2rnSA_pscore$colour <- rep(c("white", "gray95","white", "gray95","white","gray95","white", "gray95","white", "gray95"))
      str(res_mod_d2rnSA_pscore)      
      print(res_mod_d2rnSA_pscore)
      
      res_mod_d2rnSA_pscore_forest <- ggplot(res_mod_d2rnSA_pscore, aes(x= estimate, y= intervention, xmin= ci.lb, xmax= ci.ub)) + 
        geom_hline(aes(yintercept = intervention, colour = colour), size=7) +
        geom_pointrange(shape = 22, fill = "black", size = res_mod_d2rnSA_pscore$num_contrasts/7.5) + 
        geom_text(label = res_mod_d2rnSA_pscore$num_contrasts, hjust = -1.25, vjust = 1, colour = "black", fontface="bold", size =3) +        
        geom_vline(xintercept = 0, linetype = 3) +
        xlab("Difference in Standardized Mean Change with 95% Confidence Interval") +
        labs(caption = "*Size of and values next to points indicate the number of contrasts in which intervention is included", hjust=0) +
        ylab("Intervention Bundle") +
        theme_classic() +
        scale_colour_identity() +
        scale_y_discrete(limits = rev(res_mod_d2rnSA_pscore$intervention)) +
        #scale_x_log10(limits = c(-1.25, 2.25), 
        #breaks = c(0.25, 0.5, 1, 2, 4), 
        #labels = c("0.25", "0.5", "1", "2", "4"), expand = c(0,0)) +
        theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      res_mod_d2rnSA_pscore_forest
      
      #### Next create data table for merging with above plot with estimates and confidence intervals combined in one column
      res_mod_d2rnSA_pscore2 <- res_mod_d2rnSA_pscore
      round_digits <- function(x) {
        round(x, digits = 2)
      }
      convert_to_character <- function(x) {
        as.character(x)
      }
      res_mod_d2rnSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_d2rnSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], round_digits)
      res_mod_d2rnSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_d2rnSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], as.character)
      res_mod_d2rnSA_pscore2$ci.lb <- paste("(", res_mod_d2rnSA_pscore2$ci.lb, " -", sep= "")
      res_mod_d2rnSA_pscore2$ci.ub <- paste(res_mod_d2rnSA_pscore2$ci.ub, ")", sep= "")
      res_mod_d2rnSA_pscore2 <- res_mod_d2rnSA_pscore2 %>% unite(estimate_cis, estimate, ci.lb, ci.ub, sep= " ", remove = FALSE )
      print(res_mod_d2rnSA_pscore2)
      
      LfLabels<-data.frame(x=c(0,4.5,6.7), 
                           y=c(rep(length(unique(res_mod_d2rnSA_pscore2$estimate))-0.2,times=3)),
                           lab=c("Intervention","Estimate (95% CI)","P-score"))
      LfLabels
      
      data_table <- ggplot(data = res_mod_d2rnSA_pscore2, aes(y = intervention)) +
        geom_hline(aes(yintercept = intervention, colour = colour), size = 7) +
        geom_text(aes(x = 0, label = intervention), hjust = 0) +
        geom_text(aes(x = 5, label = estimate_cis)) +
        geom_text(aes(x = 7, label = Pscore), hjust = 1) +
        geom_text(data=LfLabels,aes(x,y,label=lab, fontface="bold"), vjust=-3.5, hjust=0, size=4, size=4) +
        scale_colour_identity() +
        theme_void() + 
        theme(plot.margin = margin(5, 0, 35, 0)) +
        scale_y_discrete(limits = rev(res_mod_d2rnSA_pscore$intervention)) 
      data_table
      
      #### Finally, merge plot and datatable for final forest plot
      final_fp_nma_d2rnSA <- grid.arrange(data_table, res_mod_d2rnSA_pscore_forest, ncol=2)
      final_fp_nma_d2rnSA

    ### Create network graph
    
    ### Note: The data are currently in a contrast-based, "wide" format in which each contrast is an observation 
    ###       (i.e., intervention and comparison assignments of each contrast are in separate columns of the same row.)
    ###       Need to reshape the data "long" to an arm-based format in which the two assignment groups (intervention/comparison)
    ###       are in separate rows of the same column grouped by contrast ID across those rows. This facilitates the pairing of intervention/comparison 
    ###       groups for the creation of the (weighted) edges of the network graph.

      #### Review data in contrast-based wide format before reshape for comparison with data after reshape (as a desk check)
      NMA_data_analysis_subset2 <- NMA_data_analysis_subset_grpID_d2rnSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset2) #Example rows of the contrast-based wide format. Compare to the long format printed below.
      
      #### Each unique contrast within each unique study should only be counted once when weighting the network connections between each unique contrast combination of intervention and comparison in the network graph.
      #### Because there can be multiple measures within multiple domains within each unique contrast within each unique study, we need reduce the data set to one observation per unique contrast within each unique study 
      #### so that a contrast with more domains/measures than another contrast is not overweighted in the visualization of the network connections (re the relative thicknesses of the "edges" between the I/C nodes in the network graph).
      NMA_data_analysis_subset_grpID_d2rnSA %>% count(record_id, contrast_id)      
      NMA_data_analysis_subset3 <- NMA_data_analysis_subset_grpID_d2rnSA %>% distinct(record_id, contrast_id, .keep_all = TRUE)
      NMA_data_analysis_subset3 %>% count(record_id, contrast_id)
      NMA_data_analysis_subset3 %>% count()
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset3 %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo, domain, measure_name, es_id, effect_size)
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset4 %>% unite("int_comp_prelim" , c(intervention_combo,comparison_combo), remove = FALSE)
      tabyl(NMA_data_analysis_subset4$int_comp_prelim) #The relative number of observations (n) for the intervention_BAU contrasts in this table should match the relative thicknesses of the lines (called "edges") in the network graph.
      
      #### Reshape data to arm-based long format
      NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset3, c(intervention_combo, comparison_combo), names_to = "assignment_I_C", values_to = "intervention_comparison")
      
      #### Review data in arm-based long format after reshape for comparison with data before reshape (as a desk check)
      NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% dplyr::select(record_id, contrast_id, assignment_I_C, intervention_comparison, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset_long2) #Example rows of the arm-based long format. Compare to the wide format printed above.
      
      #### Create the table of intervention/comparison pairs for creating the network graph with igraph
      dat_igraph <- NMA_data_analysis_subset_long
      dat_igraph$intervention_comparison <- as.character(dat_igraph$intervention_comparison)
      pairs <- data.frame(do.call(rbind, lapply(split(dat_igraph$intervention_comparison, dat_igraph$contrast_id), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
      print(pairs)
      pairs$X1 <- factor(pairs$X1, levels=sort(unique(dat_igraph$intervention_comparison)))
      pairs$X2 <- factor(pairs$X2, levels=sort(unique(dat_igraph$intervention_comparison)))
      tab <- table(pairs[,1], pairs[,2])
      tab
      
      #### Creating the network graph with igraph
      set.seed(3524)
      g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
      plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
           layout=layout_in_circle(g),
           #layout=layout_nicely(g),
           #layout=layout_with_lgl(g),
           vertex.size=20, vertex.color="lightgray", vertex.label.color="black", vertex.label.font=2)   

# Execute network meta-analysis using a contrast-based random-effects model using BAU as the reference condition: domain == "Whole Numbers"

  ## Subset analysis data frame further to just the Whole Numbers domain (d3wnSA)
  tabyl(NMA_data_analysis_subset_grpID_alldomains$domain)
  NMA_data_analysis_subset_grpID_d3wnSA <- NMA_data_analysis_subset_grpID_alldomains %>% filter(domain == "Whole Numbers")
  tabyl(NMA_data_analysis_subset_grpID_d3wnSA$domain)
  
  ## Model notes: setting rho=0.60; tau^2 reflects the amount of heterogeneity for all treatment comparisons
  
  ## Add contrast matrix to dataset
  NMA_data_analysis_subset_grpID_d3wnSA <- NMA_data_analysis_subset_grpID_d3wnSA %>% drop_na(c(intervention_combo, comparison_combo)) #Drop rows in the intervention and comparison columns with missing values (i.e., <NA>).
  NMA_data_analysis_subset_grpID_d3wnSA <- contrmat(NMA_data_analysis_subset_grpID_d3wnSA, grp1="intervention_combo", grp2="comparison_combo")
  str(NMA_data_analysis_subset_grpID_d3wnSA)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies
  V_list <- vcalc(variance, cluster= record_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=group1_id, grp2=group2_id, w1=intervention_n, w2=comparison_n, data=NMA_data_analysis_subset_grpID_d3wnSA)
  V_list    
  
  ## Calculate the number of unique contrasts in which each intervention bundle is included
  tabyl(NMA_data_analysis_subset_grpID_d3wnSA$intervention_combo)
  tabyl(NMA_data_analysis_subset_grpID_d3wnSA$comparison_combo)
  num_contrasts_d3wnSA <- NMA_data_analysis_subset_grpID_d3wnSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo)
  print(num_contrasts_d3wnSA)
  num_contrasts_d3wnSA_long <- num_contrasts_d3wnSA %>% pivot_longer(c(intervention_combo, comparison_combo ),names_to= "group_IC", values_to="group_intervention")
  print(num_contrasts_d3wnSA_long)
  num_contrasts_d3wnSA_long2 <- num_contrasts_d3wnSA_long %>% distinct(record_id, contrast_id, group_intervention, .keep_all = TRUE)
  print(num_contrasts_d3wnSA_long2)
  tabyl(num_contrasts_d3wnSA_long2$contrast_id) #Should be n=2 for each contrast if reshape and distinct steps done correctly: 1 intervention & 1 comparison per unique contrast. 
  num_contrasts_d3wnSA_long3 <- tabyl(num_contrasts_d3wnSA_long2$group_intervention)
  num_contrasts_d3wnSA_long3 <- num_contrasts_d3wnSA_long3 %>% dplyr::select(intervention= 'num_contrasts_d3wnSA_long2$group_intervention', num_contrasts= 'n')
  str(num_contrasts_d3wnSA_long3)
  num_contrasts_d3wnSA_long3$intervention <- as.character(num_contrasts_d3wnSA_long3$intervention)
  num_contrasts_d3wnSA_long3$intervention <- gsub("\\+", ".", num_contrasts_d3wnSA_long3$intervention)
  str(num_contrasts_d3wnSA_long3)
  print(num_contrasts_d3wnSA_long3)  
  
  ##Run standard NMA with the unique interventions bundles as moderators  
  tabyl(NMA_data_analysis_subset_grpID_d3wnSA$intervention_combo)
  tabyl(NMA_data_analysis_subset_grpID_d3wnSA$comparison_combo)
  check_d3wnSA <- NMA_data_analysis_subset_grpID_d3wnSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo)
  print(check_d3wnSA)
  res_mod_d3wnSA <- rma.mv(effect_size, V_list, 
                         mods = ~ FF + FF.N.RV + FF.RS + FF.RS.N + FF.RS.N.SEO + FF.RS.N.TV + FF.RS.SEO +  FF.RS.SEO.TV + FF.RS.TV + FF.RV + NL.FF.RS.SEO + NL.FF.RS.TV + RS + RS.N + RS.N.FWOF + RS.N.SEO + RS.N.SEO.FWOF + RS.N.TV + RS.SEO + RS.TV.FWOF + VF.FF.RS + VF.FF.RS.N.SEO + VF.FF.RS.SEO + VF.FF.RS.TV + VF.RS.N.SEO - 1, 
                         random = ~ 1 | record_id/es_id, 
                         rho=0.60, 
                         data=NMA_data_analysis_subset_grpID_d3wnSA)
  summary(res_mod_d3wnSA)
  #weights.rma.mv(res_mod_d3wnSA)

    ### Estimate all pairwise differences between treatments
    contr <- data.frame(t(combn(names(coef(res_mod_d3wnSA)), 2)))
    contr <- contrmat(contr, "X1", "X2")
    rownames(contr) <- paste(contr$X1, "-", contr$X2)
    contr <- as.matrix(contr[-c(1:2)])
    sav <- predict(res_mod_d3wnSA, newmods=contr)
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
    lt_info_df2$ci.lb <- paste("(", lt_info_df2$ci.lb, " ,", sep= "")
    lt_info_df2$ci.ub <- paste(lt_info_df2$ci.ub, ")", sep= "")
    lt_info_df2 <- lt_info_df2 %>% unite(pred_cis, pred, ci.lb, ci.ub, sep= " ", remove = FALSE )
    print(lt_info_df2)
    lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #To-do: possible to format ci below? + color code by sig
    lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
    print(lt_info_df3)
    write_csv(lt_info_df3, 'nma_league_table_d3wnSA.csv')
    #write_xlsx(lt_info_df3, 'nma_league_table_d3wnSA.xlsx')
    
    ### Compute p-values
    contr <- data.frame(t(combn(c(names(coef(res_mod_d3wnSA)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
    contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
    b <- c(coef(res_mod_d3wnSA),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
    vb <- bldiag(vcov(res_mod_d3wnSA),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
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
    res_mod_d3wnSA_df <- tidy(res_mod_d3wnSA, conf.int = TRUE)
    pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
    res_mod_d3wnSA_pscore <- res_mod_d3wnSA_df %>% left_join(pscores_df, by = c("term"))
    res_mod_d3wnSA_pscore <- res_mod_d3wnSA_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
    print(res_mod_d3wnSA_pscore, n=Inf)
    
    ### Create forest plot using metafor's built-in function
    # forest(coef(res_mod_d3wnSA), diag(vcov(res_mod_d3wnSA)), slab=sub(".", " ", names(coef(res_mod_d3wnSA)), fixed=TRUE),
    #       #xlim=c(-5,5), alim=c(-3,3), psize=6, header="Intervention", top=2,
    #       header="Intervention",
    #       xlab="Difference in Standardized Mean Change")
    
    ### Create forest plot using ggplot

      #### First create plot of estimates and confidence intervals
      res_mod_d3wnSA_pscore <- res_mod_d3wnSA_pscore %>% arrange(desc(Pscore))
      str(res_mod_d3wnSA_pscore)
      print(res_mod_d3wnSA_pscore)
      print(num_contrasts_d3wnSA_long3)
      res_mod_d3wnSA_pscore <- res_mod_d3wnSA_pscore %>% left_join(num_contrasts_d3wnSA_long3, by = "intervention") # Merge on number of unique contrasts in which each intervention bundle is included
      res_mod_d3wnSA_pscore$colour <- rep(c("white", "gray95","white", "gray95","white","gray95", "gray95","white", "gray95","white","gray95", "gray95","white", "gray95","white","gray95", "gray95","white", "gray95","white","gray95", "gray95","white", "gray95","white"))
      str(res_mod_d3wnSA_pscore)     
      print(res_mod_d3wnSA_pscore)
      
      res_mod_d3wnSA_pscore_forest <- ggplot(res_mod_d3wnSA_pscore, aes(x= estimate, y= intervention, xmin= ci.lb, xmax= ci.ub)) + 
        geom_hline(aes(yintercept = intervention, colour = colour), size=7) +
        geom_pointrange(shape = 22, fill = "black", size = res_mod_d3wnSA_pscore$num_contrasts/7.5) + 
        geom_text(label = res_mod_d3wnSA_pscore$num_contrasts, hjust = -1.25, vjust = 1, colour = "black", fontface="bold", size =3) +        
        geom_vline(xintercept = 0, linetype = 3) +
        xlab("Difference in Standardized Mean Change with 95% Confidence Interval") +
        labs(caption = "*Size of and values next to points indicate the number of contrasts in which intervention is included", hjust=0) +
        ylab("Intervention Bundle") +
        theme_classic() +
        scale_colour_identity() +
        scale_y_discrete(limits = rev(res_mod_d3wnSA_pscore$intervention)) +
        #scale_x_log10(limits = c(-1.25, 2.25), 
        #breaks = c(0.25, 0.5, 1, 2, 4), 
        #labels = c("0.25", "0.5", "1", "2", "4"), expand = c(0,0)) +
        theme(axis.text.y = element_blank(), axis.title.y = element_blank())
      res_mod_d3wnSA_pscore_forest
      
      #### Next create data table for merging with above plot with estimates and confidence intervals combined in one column
      res_mod_d3wnSA_pscore2 <- res_mod_d3wnSA_pscore
      round_digits <- function(x) {
        round(x, digits = 2)
      }
      convert_to_character <- function(x) {
        as.character(x)
      }
      res_mod_d3wnSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_d3wnSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], round_digits)
      res_mod_d3wnSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(res_mod_d3wnSA_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], as.character)
      res_mod_d3wnSA_pscore2$ci.lb <- paste("(", res_mod_d3wnSA_pscore2$ci.lb, " -", sep= "")
      res_mod_d3wnSA_pscore2$ci.ub <- paste(res_mod_d3wnSA_pscore2$ci.ub, ")", sep= "")
      res_mod_d3wnSA_pscore2 <- res_mod_d3wnSA_pscore2 %>% unite(estimate_cis, estimate, ci.lb, ci.ub, sep= " ", remove = FALSE )
      print(res_mod_d3wnSA_pscore2)
      
      LfLabels<-data.frame(x=c(0,4.5,6.7), 
                           y=c(rep(length(unique(res_mod_d3wnSA_pscore2$estimate))-0.2,times=3)),
                           lab=c("Intervention","Estimate (95% CI)","P-score"))
      LfLabels
      
      data_table <- ggplot(data = res_mod_d3wnSA_pscore2, aes(y = intervention)) +
        geom_hline(aes(yintercept = intervention, colour = colour), size = 7) +
        geom_text(aes(x = 0, label = intervention), hjust = 0) +
        geom_text(aes(x = 5, label = estimate_cis)) +
        geom_text(aes(x = 7, label = Pscore), hjust = 1) +
        geom_text(data=LfLabels,aes(x,y,label=lab, fontface="bold"), vjust=-6.5, hjust=0, size=4, size=4) +
        scale_colour_identity() +
        theme_void() + 
        theme(plot.margin = margin(5, 0, 35, 0)) +
        scale_y_discrete(limits = rev(res_mod_d3wnSA_pscore$intervention)) 
      data_table
       
      #### Finally, merge plot and datatable for final forest plot
      final_fp_nma_d3wnSA <- grid.arrange(data_table, res_mod_d3wnSA_pscore_forest, ncol=2)
      final_fp_nma_d3wnSA

    ### Create network graph
    
    ### Note: The data are currently in a contrast-based, "wide" format in which each contrast is an observation 
    ###       (i.e., intervention and comparison assignments of each contrast are in separate columns of the same row.)
    ###       Need to reshape the data "long" to an arm-based format in which the two assignment groups (intervention/comparison)
    ###       are in separate rows of the same column grouped by contrast ID across those rows. This facilitates the pairing of intervention/comparison 
    ###       groups for the creation of the (weighted) edges of the network graph.

      #### Review data in contrast-based wide format before reshape for comparison with data after reshape (as a desk check)
      NMA_data_analysis_subset2 <- NMA_data_analysis_subset_grpID_d3wnSA %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset2) #Example rows of the contrast-based wide format. Compare to the long format printed below.
      
      #### Each unique contrast within each unique study should only be counted once when weighting the network connections between each unique contrast combination of intervention and comparison in the network graph.
      #### Because there can be multiple measures within multiple domains within each unique contrast within each unique study, we need reduce the data set to one observation per unique contrast within each unique study 
      #### so that a contrast with more domains/measures than another contrast is not overweighted in the visualization of the network connections (re the relative thicknesses of the "edges" between the I/C nodes in the network graph).
      NMA_data_analysis_subset_grpID_d3wnSA %>% count(record_id, contrast_id)      
      NMA_data_analysis_subset3 <- NMA_data_analysis_subset_grpID_d3wnSA %>% distinct(record_id, contrast_id, .keep_all = TRUE)
      NMA_data_analysis_subset3 %>% count(record_id, contrast_id)
      NMA_data_analysis_subset3 %>% count()
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset3 %>% dplyr::select(record_id, contrast_id, intervention_combo, comparison_combo, domain, measure_name, es_id, effect_size)
      NMA_data_analysis_subset4 <- NMA_data_analysis_subset4 %>% unite("int_comp_prelim" , c(intervention_combo,comparison_combo), remove = FALSE)
      tabyl(NMA_data_analysis_subset4$int_comp_prelim) #The relative number of observations (n) for the intervention_BAU contrasts in this table should match the relative thicknesses of the lines (called "edges") in the network graph.
      
      #### Reshape data to arm-based long format
      NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset3, c(intervention_combo, comparison_combo), names_to = "assignment_I_C", values_to = "intervention_comparison")
      
      #### Review data in arm-based long format after reshape for comparison with data before reshape (as a desk check)
      NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% dplyr::select(record_id, contrast_id, assignment_I_C, intervention_comparison, domain, measure_name, es_id, effect_size)
      print(NMA_data_analysis_subset_long2) #Example rows of the arm-based long format. Compare to the wide format printed above.
      
      #### Create the table of intervention/comparison pairs for creating the network graph with igraph
      dat_igraph <- NMA_data_analysis_subset_long
      dat_igraph$intervention_comparison <- as.character(dat_igraph$intervention_comparison)
      pairs <- data.frame(do.call(rbind, lapply(split(dat_igraph$intervention_comparison, dat_igraph$contrast_id), function(x) t(combn(x,2)))), stringsAsFactors=FALSE)
      print(pairs)
      pairs$X1 <- factor(pairs$X1, levels=sort(unique(dat_igraph$intervention_comparison)))
      pairs$X2 <- factor(pairs$X2, levels=sort(unique(dat_igraph$intervention_comparison)))
      tab <- table(pairs[,1], pairs[,2])
      tab
      
      #### Creating the network graph with igraph
      set.seed(3524)
      g <- graph_from_adjacency_matrix(tab, mode = "plus", weighted=TRUE, diag=FALSE)
      plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
           layout=layout_in_circle(g),
           #layout=layout_nicely(g),
           #layout=layout_with_lgl(g),
           vertex.size=20, vertex.color="lightgray", vertex.label.color="black", vertex.label.font=2)       