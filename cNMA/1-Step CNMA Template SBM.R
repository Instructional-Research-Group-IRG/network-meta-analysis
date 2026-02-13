# Load required packages
  pacman::p_load(metafor, googlesheets4, dplyr, tidyr, skimr, testit, assertable, meta, netmeta, stringr, janitor, naniar, igraph, multcomp, broom, gridExtra, ggplot2, writexl, readr, grid, gridExtra, cowplot, extrafont)
  options(max.print = 1000000)
  
# Load final CNMA dataset with effect sizes and standard errors converted to 4.1
  cNMA_data_4.1 <- read_csv("cNMA_data_4.1.csv")
  cNMA_data_4.1 <- cNMA_data_4.1 %>% mutate(variance_final = ((standard_error_final^2) * (intervention_n + comparison_n))) # Calculate variance from standard error and sample size
  describe (cNMA_data_4.1$standard_error_final)
  describe (cNMA_data_4.1$variance_final)
  
# Merge intervention_prelim & comparison_prelim from NNMA database to cNMA_data_4.1 until intervention and comparison columns are added to the CNMA database 
  
  ## Load in observations used for NMA
  NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0") #Full data set
  NMA_data_analysis_subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") & aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") & (TvsT==1 | TvsT==0))
  
  ## Modify data to match CNMA for merging
  NMA_data_analysis_subset <- NMA_data_analysis_subset %>% rename(study_id=record_id)  
  class(NMA_data_analysis_subset$contrast_id)
  NMA_data_analysis_subset <- NMA_data_analysis_subset %>% mutate(contrast_id = as.character(contrast_id))
  class(NMA_data_analysis_subset$contrast_id)
  tabyl(NMA_data_analysis_subset$contrast_id)
  NMA_data_analysis_subset$contrast_id <- gsub("_disagg", "", NMA_data_analysis_subset$contrast_id) # "_disagg" removed from contrast IDs in the CNMA Master Database
  NMA_data_analysis_subset$contrast_id <- tolower(NMA_data_analysis_subset$contrast_id) # Convert contrast IDs to lowercase to match CNMA Master Database
  NMA_data_analysis_subset$contrast_id <- gsub("87213_t2", "87213_a", NMA_data_analysis_subset$contrast_id) # "87213_t2" changed to "87213_a" in the CNMA Master Database
  tabyl(NMA_data_analysis_subset$contrast_id)
  NMA_data_analysis_subset_short <- NMA_data_analysis_subset %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim)
  
  ## Merge on intervention_prelim & comparison_prelim to cNMA_data_4.1
  cNMA_data_4.1 %>% count()
  cNMA_data_4.1_NMAmerge <- cNMA_data_4.1 %>%
    inner_join(NMA_data_analysis_subset_short, by = c("study_id", "contrast_id", "es_id")) #Use inner join because we only want to retain observations of the CNMA database that have matches in the NNMA database.
  tabyl(cNMA_data_4.1_NMAmerge$intervention_prelim)
  tabyl(cNMA_data_4.1_NMAmerge$comparison_prelim)
  cNMA_data_4.1_NMAmerge %>% count()
  cNMA_data_4.1_NMAmerge <- cNMA_data_4.1_NMAmerge %>% filter(!is.na(intervention_prelim) & !is.na(comparison_prelim)) # Drop rows where intervention_prelim or comparison_prelim is NA  
  cNMA_data_4.1_NMAmerge %>% count()
  tabyl(cNMA_data_4.1_NMAmerge$intervention_prelim)
  tabyl(cNMA_data_4.1_NMAmerge$comparison_prelim)
  assert_character(cNMA_data_4.1_NMAmerge$intervention_content, any.missing = FALSE) 
  cNMA_data_4.1_NMAmerge <- cNMA_data_4.1_NMAmerge %>% filter(intervention_prelim!=comparison_prelim) #Drop observations with the exact same intervention bundle.
  cNMA_data_4.1_NMAmerge %>% count()
  
# Execute CNMA for Whole Numbers domain
  
  ## Filter dataset to Whole Numbers domain
  tabyl(cNMA_data_4.1_NMAmerge$intervention_content)
  cNMA_data_4.1_NMAmerge_icW <- cNMA_data_4.1_NMAmerge %>% filter(intervention_content == "W")
  tabyl(cNMA_data_4.1_NMAmerge_icW$intervention_content)
  
  ## Prepare component binaries for cNMA
  cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim, measure_name, domain, effect_size_final, variance_final, intervention_n, comparison_n)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$intervention_prelim)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$comparison_prelim)
  
  cNMA_data_4.1_NMAmerge_icW_short$FF <- 0
  cNMA_data_4.1_NMAmerge_icW_short$RS <- 0
  cNMA_data_4.1_NMAmerge_icW_short$NL <- 0
  cNMA_data_4.1_NMAmerge_icW_short$TES <- 0
  cNMA_data_4.1_NMAmerge_icW_short$VF <- 0
  cNMA_data_4.1_NMAmerge_icW_short$BAU <- 0
  
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$FF)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$RS)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$NL)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$TES)  
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$VF)
  tabyl(cNMA_data_4.1_NMAmerge_icW_short$BAU)
   
    ### FF
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="F",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="FF",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="NL+TES+FF+RS",1, FF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(intervention_prelim=="VF+FF+RS",1, FF))
    
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(comparison_prelim=="FF" & FF==0,-1, ifelse(comparison_prelim=="FF" & FF==1, 0, FF)))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(comparison_prelim=="FF+RS" & FF==0,-1, ifelse(comparison_prelim=="FF+RS" & FF==1, 0, FF)))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(FF = ifelse(comparison_prelim=="NL+FF+RS" & FF==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & FF==1, 0, FF)))
    
    cNMA_data_4.1_NMAmerge_icW_short_FF <- cNMA_data_4.1_NMAmerge_icW_short %>% dplyr::select(intervention_prelim, comparison_prelim, FF)
    print(cNMA_data_4.1_NMAmerge_icW_short_FF, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$FF)
  
    ### RS
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+FF+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+VF+RS",1, RS))  
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="TES+RS",1, RS))     
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="VF+FF+RS",1, RS))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(intervention_prelim=="VF+RS",1, RS))
    
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(comparison_prelim=="FF+RS" & RS==0,-1, ifelse(comparison_prelim=="FF+RS" & RS==1, 0, RS)))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(RS = ifelse(comparison_prelim=="NL+FF+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & RS==1, 0, RS)))
    
    cNMA_data_4.1_NMAmerge_icW_short_RS <- cNMA_data_4.1_NMAmerge_icW_short %>% dplyr::select(intervention_prelim, comparison_prelim, RS)
    print(cNMA_data_4.1_NMAmerge_icW_short_RS, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$RS)
    
    ### NL
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+RS",1, NL))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+FF+RS",1, NL))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+VF+RS",1, NL))  
    
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(NL = ifelse(comparison_prelim=="NL+FF+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & NL==1, 0, NL)))
    
    cNMA_data_4.1_NMAmerge_icW_short_NL <- cNMA_data_4.1_NMAmerge_icW_short %>% dplyr::select(intervention_prelim, comparison_prelim, NL)
    print(cNMA_data_4.1_NMAmerge_icW_short_NL, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$NL)
    
    ### TES
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+FF+RS",1, TES))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+VF+RS",1, TES))  
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(TES = ifelse(intervention_prelim=="TES+RS",1, TES))  
 
    cNMA_data_4.1_NMAmerge_icW_short_TES <- cNMA_data_4.1_NMAmerge_icW_short %>% dplyr::select(intervention_prelim, comparison_prelim, TES)
    print(cNMA_data_4.1_NMAmerge_icW_short_TES, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$TES)
    
    ### VF
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(VF = ifelse(intervention_prelim=="NL+TES+VF+RS",1, VF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(VF = ifelse(intervention_prelim=="VF+FF+RS",1, VF))
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(VF = ifelse(intervention_prelim=="VF+RS",1, VF))
    
    cNMA_data_4.1_NMAmerge_icW_short_VF <- cNMA_data_4.1_NMAmerge_icW_short %>% dplyr::select(intervention_prelim, comparison_prelim, VF)
    print(cNMA_data_4.1_NMAmerge_icW_short_VF, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$VF)
    
    ### BAU
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$BAU)
    cNMA_data_4.1_NMAmerge_icW_short <- cNMA_data_4.1_NMAmerge_icW_short %>% mutate(BAU = ifelse(comparison_prelim=="BAU",-1, BAU))
    
    cNMA_data_4.1_NMAmerge_icW_short_BAU <- cNMA_data_4.1_NMAmerge_icW_short %>% dplyr::select(intervention_prelim, comparison_prelim, BAU)
    print(cNMA_data_4.1_NMAmerge_icW_short_BAU, n=Inf)
    tabyl(cNMA_data_4.1_NMAmerge_icW_short$BAU) 

  str(cNMA_data_4.1_NMAmerge_icW_short)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies- Whole Numbers domain
  V_list <- vcalc(variance_final, cluster= study_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=intervention_prelim, grp2=comparison_prelim, w1=intervention_n, w2=comparison_n, data=cNMA_data_4.1_NMAmerge_icW_short)
  
  ## Fit 1-step additive CNMA- Whole Numbers domain
  mod_addCNMA_icW<- rma.mv(effect_size_final, V_list, 
                                     mods = ~ FF + RS + NL + TES + VF - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons
                                     random = ~ 1 | study_id/es_id, 
                                     rho=0.60, 
                                     data=cNMA_data_4.1_NMAmerge_icW_short)
  summary(mod_addCNMA_icW)

  ### Estimate all pairwise differences between treatments
  contr <- data.frame(t(combn(names(coef(mod_addCNMA_icW)), 2)))
  contr <- contrmat(contr, "X1", "X2")
  rownames(contr) <- paste(contr$X1, "-", contr$X2)
  contr <- as.matrix(contr[-c(1:2)])
  sav <- predict(mod_addCNMA_icW, newmods=contr)
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
  lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #This creates the league table formatted as "left vs top".
  lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
  print(lt_info_df3)
  write_csv(lt_info_df3, 'nma_league_table_icW_cnma.csv')
  #write_xlsx(lt_info_df3, 'nma_league_table_icW_cnma.xlsx')
  
  ### Compute p-values
  contr <- data.frame(t(combn(c(names(coef(mod_addCNMA_icW)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
  contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
  b <- c(coef(mod_addCNMA_icW),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
  vb <- bldiag(vcov(mod_addCNMA_icW),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
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
  mod_addCNMA_icW_df <- tidy(mod_addCNMA_icW, conf.int = TRUE)
  pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
  mod_addCNMA_icW_pscore <- mod_addCNMA_icW_df %>% left_join(pscores_df, by = c("term"))
  mod_addCNMA_icW_pscore <- mod_addCNMA_icW_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
  mod_addCNMA_icW_pscore
  
  ### Create forest plot using metafor's built-in function
  # forest(coef(res_mod_icW), diag(vcov(res_mod_icW)), slab=sub(".", " ", names(coef(res_mod_icW)), fixed=TRUE),
  #        #xlim=c(-5,5), alim=c(-3,3), psize=6, header="Intervention", top=2,
  #        header="Intervention",
  #        xlab="Difference in Standardized Mean Change (compared to Control)")
  
  ### Create forest plot using ggplot
  
  #### First create plot of estimates and confidence intervals
  mod_addCNMA_icW_pscore <- mod_addCNMA_icW_pscore %>% arrange(desc(Pscore))
  str(mod_addCNMA_icW_pscore)
  print(mod_addCNMA_icW_pscore)
  # print(num_contrasts_icW_long3)
  # mod_addCNMA_icW_pscore <- mod_addCNMA_icW_pscore %>% left_join(num_contrasts_icW_long3, by = "intervention") # Merge on number of unique contrasts in which each intervention bundle is included
  # print(num_students_icW_long3)
  # tabyl(num_students_icW_long3$intervention_comparison)
  # num_students_icW_long3$intervention_comparison <- gsub("\\+", ".", num_students_icW_long3$intervention_comparison)
  # num_students_icW_long3 <- num_students_icW_long3 %>% rename(num_students = sum_num_students_bundle)
  # mod_addCNMA_icW_pscore <- mod_addCNMA_icW_pscore %>% left_join(num_students_icW_long3, by = c("intervention"= "intervention_comparison")) # Merge on number of unique students in which each intervention bundle is included
  mod_addCNMA_icW_pscore$colour <- rep(c("yellow","burlywood1","royalblue1","green","azure1"))
  str(mod_addCNMA_icW_pscore)
  print(mod_addCNMA_icW_pscore)
  
  mod_addCNMA_icW_pscore_forest <- ggplot(mod_addCNMA_icW_pscore, aes(x= estimate, y= intervention, xmin= ci.lb, xmax= ci.ub)) +  
    geom_hline(aes(yintercept = intervention, colour = colour), size=15) +
    #geom_pointrange(shape = 22, fill = "black", size = mod_addCNMA_icW_pscore$num_contrasts/5) + 
    geom_pointrange(shape = 22, fill = "black") + 
    #geom_text(label = paste0("(",mod_addCNMA_icW_pscore$num_contrasts, " contrast/s, ", mod_addCNMA_icW_pscore$num_students, " students",")"), hjust = 0.5, vjust = 2.95, colour = "black", size =6.5, family= "Times New Roman") +        
    geom_vline(xintercept = 0, linetype = 3) +
    xlab("Difference in Standardized Mean Change (compared to Control)") +
    ylab("Intervention Component") +
    theme_classic() +
    scale_colour_identity() +
    scale_y_discrete(limits = rev(mod_addCNMA_icW_pscore$intervention)) +
    theme(axis.title.x = element_text(face = "bold", size=25, family= "Times New Roman"), #x-axis title
          plot.caption = element_text(size = 18, family= "Times New Roman"), #plot caption (x-axis)
          axis.title.y = element_text(face = "bold", size=25, hjust= 0.44, family= "Times New Roman"), #y-axis title
          axis.text.y = element_text(size=25, hjust=0.5), #the intervention bundles
          axis.text.x = element_text(size = 20, family= "Times New Roman")) #x-axis tick values
  mod_addCNMA_icW_pscore_forest
  
  #### Next create data table for merging with above plot with estimates and confidence intervals combined in one column
  mod_addCNMA_icW_pscore2 <- mod_addCNMA_icW_pscore
  round_digits <- function(x) {
    round(x, digits = 2)
  }
  convert_to_character <- function(x) {
    as.character(x)
  }
  mod_addCNMA_icW_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(mod_addCNMA_icW_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], round_digits)
  mod_addCNMA_icW_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(mod_addCNMA_icW_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], as.character)
  mod_addCNMA_icW_pscore2$ci.lb <- paste("(", mod_addCNMA_icW_pscore2$ci.lb, " -", sep= "")
  mod_addCNMA_icW_pscore2$ci.ub <- paste(mod_addCNMA_icW_pscore2$ci.ub, ")", sep= "")
  mod_addCNMA_icW_pscore2 <- mod_addCNMA_icW_pscore2 %>% unite(estimate_cis, estimate, ci.lb, ci.ub, sep= " ", remove = FALSE )
  print(mod_addCNMA_icW_pscore2)
  
  LfLabels1<-data.frame(x=c(1), 
                        y=c(5.5),
                        lab=c("Estimate (95% CI)"))
  LfLabels1      
  data_table1 <- ggplot(data = mod_addCNMA_icW_pscore2, aes(x, y = intervention)) +
    geom_hline(aes(yintercept = intervention, colour = colour), size = 15) + 
    geom_text(aes(x = 1, label = estimate_cis), size= 9.25, family= "Times New Roman") + 
    geom_text(data=LfLabels1,aes(x,y,label=lab, fontface="bold"), size=8, family= "Times New Roman") + 
    scale_colour_identity() +
    theme_void() + 
    theme(text= element_text(family = "Times New Roman")) +
    scale_y_discrete(limits = rev(mod_addCNMA_icW_pscore$intervention)) 
  data_table1
  
  LfLabels2<-data.frame(x=c(1), 
                        y=c(5.5),
                        lab=c("P-score"))
  LfLabels2      
  data_table2 <- ggplot(data = mod_addCNMA_icW_pscore2, aes(x, y = intervention)) +
    geom_hline(aes(yintercept = intervention, colour = colour), size = 15) + 
    geom_text(aes(x = 1, label = Pscore), size= 9.25, family= "Times New Roman") +
    geom_text(data=LfLabels2,aes(x,y,label=lab, fontface="bold"), size=8, family= "Times New Roman") +
    scale_colour_identity() +
    theme_void() + 
    theme(text= element_text(family = "Times New Roman")) +
    scale_y_discrete(limits = rev(mod_addCNMA_icW_pscore$intervention)) 
  data_table2
  
  #### Finally, merge plot and datatable for final forest plot
  aligned_plots <- align_plots(mod_addCNMA_icW_pscore_forest, data_table1, data_table2, align = "h")
  final_fp_nma_icW <- grid.arrange(grobs = aligned_plots, nrow= 1, widths= c(2.5,0.75,0.5))
  final_fp_nma_icW

  ### Create network graph

  ### Note: The data are currently in a contrast-based, "wide" format in which each contrast is an observation
  ###       (i.e., intervention and comparison assignments of each contrast are in separate columns of the same row.)
  ###       Need to reshape the data "long" to an arm-based format in which the two assignment groups (intervention/comparison)
  ###       are in separate rows of the same column grouped by contrast ID across those rows. This facilitates the pairing of intervention/comparison
  ###       groups for the creation of the (weighted) edges of the network graph.

  #### Review data in contrast-based wide format before reshape for comparison with data after reshape (as a desk check)
  cNMA_data_analysis_subset2 <- cNMA_data_4.1_NMAmerge_icW %>% dplyr::select(study_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size_final)
  print(cNMA_data_analysis_subset2) #Example rows of the contrast-based wide format. Compare to the long format printed below.

  #### Each unique contrast within each unique study should only be counted once when weighting the network connections between each unique contrast combination of intervention versus comparison in the network graph.
  #### Because there can be multiple measures within multiple domains within each unique contrast within each unique study, we need reduce the data set to one observation per unique contrast within each unique study
  #### so that a contrast with more domains/measures than another contrast is not overweighted in the visualization of the network connections (re the relative thicknesses of the "edges" between the I/C nodes in the network graph).
  cNMA_data_4.1_NMAmerge_icW %>% count(study_id, contrast_id)
  NMA_data_analysis_subset3 <- cNMA_data_4.1_NMAmerge_icW %>% distinct(study_id, contrast_id, .keep_all = TRUE)
  NMA_data_analysis_subset3 %>% count(study_id, contrast_id)
  NMA_data_analysis_subset3 %>% count()
  NMA_data_analysis_subset4 <- NMA_data_analysis_subset3 %>% dplyr::select(study_id, contrast_id, intervention_prelim, comparison_prelim, domain, measure_name, es_id, effect_size_final)
  NMA_data_analysis_subset4 <- NMA_data_analysis_subset4 %>% unite("int_comp_prelim" , c(intervention_prelim,comparison_prelim), remove = FALSE)
  tabyl(NMA_data_analysis_subset4$int_comp_prelim) #The relative number of observations (n) for the intervention_BAU contrasts in this table should match the relative thicknesses of the lines (called "edges") in the network graph.

  #### Reshape data to arm-based long format
  NMA_data_analysis_subset_long <- pivot_longer(NMA_data_analysis_subset3, c(intervention_prelim, comparison_prelim), names_to = "assignment_I_C", values_to = "intervention_comparison")

  #### Review data in arm-based long format after reshape for comparison with data before reshape (as a desk check)
  NMA_data_analysis_subset_long2 <- NMA_data_analysis_subset_long %>% dplyr::select(study_id, contrast_id, assignment_I_C, intervention_comparison, domain, measure_name, es_id, effect_size_final)
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

  # num_students_icW_long3
  # num_students_icW_long4 <- num_students_icW_long3 %>% mutate(sum_num_students_bundle2= if_else((intervention_comparison=="NL.FF.RS" | intervention_comparison=="VF.RS"),num_students*2,num_students))
  # num_students_icW_long4 <- num_students_icW_long4 %>% mutate(sum_num_students_bundle2= if_else((intervention_comparison=="FF" | intervention_comparison=="VF.FF.RS"),sum_num_students_bundle2*1.5,sum_num_students_bundle2))
  # #num_students_icW_long4 <- num_students_icW_long3 %>% mutate(sum_num_students_bundle2= sum_num_students_bundle)
  # num_students_icW_long4 <- num_students_icW_long4 %>% mutate(dist=c(0,1.75,0,1.5,2.5,0,-2,-2))
  # num_students_icW_long4 <- num_students_icW_long4 %>% mutate(color=c("lightgray","royalblue1","burlywood1","red","green","azure1","yellow","pink"))
  # num_students_icW_long4 <- num_students_icW_long4 %>% mutate(intervention_comparison= ifelse(intervention_comparison == "BAU", "Control", intervention_comparison))
  # num_students_icW_long4

  plot(g, edge.curved=FALSE, edge.width=E(g)$weight,
       layout=layout_in_circle(g))
       #layout=layout_in_circle(g, order=c("BAU","FF","FF+RS","NL+FF+RS","NL+RS","RS","VF+FF+RS","VF+RS")),
       #vertex.size=(num_students_icW_long4$sum_num_students_bundle2)/50, vertex.color=num_students_icW_long4$color,
       #vertex.label.color="black", vertex.label.font=2, vertex.label=num_students_icW_long4$intervention_comparison, vertex.label.dist=num_students_icW_long4$dist)


# Execute CNMA for Rational Numbers domain
  
  ## Filter dataset to Whole Numbers domain
  tabyl(cNMA_data_4.1_NMAmerge$intervention_content)
  cNMA_data_4.1_NMAmerge_icR <- cNMA_data_4.1_NMAmerge %>% filter(intervention_content == "R")
  tabyl(cNMA_data_4.1_NMAmerge_icR$intervention_content)
  
  ## Prepare component binaries for cNMA
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR %>% dplyr::select(study_id, contrast_id, es_id, intervention_prelim, comparison_prelim, measure_name, domain, effect_size_final, variance_final, intervention_n, comparison_n)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$intervention_prelim)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$comparison_prelim)
  
  cNMA_data_4.1_NMAmerge_icR_short$FF <- 0
  cNMA_data_4.1_NMAmerge_icR_short$RS <- 0
  cNMA_data_4.1_NMAmerge_icR_short$NL <- 0
  cNMA_data_4.1_NMAmerge_icR_short$TES <- 0
  cNMA_data_4.1_NMAmerge_icR_short$VF <- 0
  cNMA_data_4.1_NMAmerge_icR_short$BAU <- 0
  
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$FF)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$RS)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$NL)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$TES)  
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$VF)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$BAU)
  
  ### FF
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="FF",1, FF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="FF+RS",1, FF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="NL+FF+RS",1, FF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="NL+TES+FF+RS",1, FF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(intervention_prelim=="VF+FF+RS",1, FF))
  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(comparison_prelim=="FF+RS" & FF==0,-1, ifelse(comparison_prelim=="FF+RS" & FF==1, 0, FF)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(FF = ifelse(comparison_prelim=="NL+FF+RS" & FF==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & FF==1, 0, FF)))

  cNMA_data_4.1_NMAmerge_icR_short_FF <- cNMA_data_4.1_NMAmerge_icR_short %>% dplyr::select(intervention_prelim, comparison_prelim, FF)
  print(cNMA_data_4.1_NMAmerge_icR_short_FF, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$FF)
  
  ### RS
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="FF+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+FF+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+FF+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="NL+TES+VF+RS",1, RS))  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="RS",1, RS))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(intervention_prelim=="VF+FF+RS",1, RS))
  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(comparison_prelim=="FF+RS" & RS==0,-1, ifelse(comparison_prelim=="FF+RS" & RS==1, 0, RS)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(comparison_prelim=="NL+FF+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & RS==1, 0, RS)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(comparison_prelim=="NL+TES+RS" & RS==0,-1, ifelse(comparison_prelim=="NL+TES+RS" & RS==1, 0, RS)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(RS = ifelse(comparison_prelim=="RS" & RS==0,-1, ifelse(comparison_prelim=="RS" & RS==1, 0, RS)))
  
  cNMA_data_4.1_NMAmerge_icR_short_RS <- cNMA_data_4.1_NMAmerge_icR_short %>% dplyr::select(intervention_prelim, comparison_prelim, RS)
  print(cNMA_data_4.1_NMAmerge_icR_short_RS, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$RS)
  
  ### NL
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+FF+RS",1, NL))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+RS",1, NL))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+FF+RS",1, NL))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+RS",1, NL))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(intervention_prelim=="NL+TES+VF+RS",1, NL))  
  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(comparison_prelim=="NL+FF+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+FF+RS" & NL==1, 0, NL)))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(NL = ifelse(comparison_prelim=="NL+TES+RS" & NL==0,-1, ifelse(comparison_prelim=="NL+TES+RS" & NL==1, 0, NL)))
  
  cNMA_data_4.1_NMAmerge_icR_short_NL <- cNMA_data_4.1_NMAmerge_icR_short %>% dplyr::select(intervention_prelim, comparison_prelim, NL)
  print(cNMA_data_4.1_NMAmerge_icR_short_NL, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$NL)
  
  ### TES
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+FF+RS",1, TES))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+RS",1, TES)) 
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(TES = ifelse(intervention_prelim=="NL+TES+VF+RS",1, TES))  
  
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(TES = ifelse(comparison_prelim=="NL+TES+RS" & TES==0,-1, ifelse(comparison_prelim=="NL+TES+RS" & TES==1, 0, TES)))
  
  cNMA_data_4.1_NMAmerge_icR_short_TES <- cNMA_data_4.1_NMAmerge_icR_short %>% dplyr::select(intervention_prelim, comparison_prelim, TES)
  print(cNMA_data_4.1_NMAmerge_icR_short_TES, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$TES)
  
  ### VF
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(VF = ifelse(intervention_prelim=="NL+TES+VF+RS",1, VF))
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(VF = ifelse(intervention_prelim=="VF+FF+RS",1, VF))
  
  cNMA_data_4.1_NMAmerge_icR_short_VF <- cNMA_data_4.1_NMAmerge_icR_short %>% dplyr::select(intervention_prelim, comparison_prelim, VF)
  print(cNMA_data_4.1_NMAmerge_icR_short_VF, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$VF)
  
  ### BAU
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$BAU)
  cNMA_data_4.1_NMAmerge_icR_short <- cNMA_data_4.1_NMAmerge_icR_short %>% mutate(BAU = ifelse(comparison_prelim=="BAU",-1, BAU))
  
  cNMA_data_4.1_NMAmerge_icR_short_BAU <- cNMA_data_4.1_NMAmerge_icR_short %>% dplyr::select(intervention_prelim, comparison_prelim, BAU)
  print(cNMA_data_4.1_NMAmerge_icR_short_BAU, n=Inf)
  tabyl(cNMA_data_4.1_NMAmerge_icR_short$BAU) 
  
  str(cNMA_data_4.1_NMAmerge_icR_short)
  
  ## Calculate the variance-covariance matrix for multi-treatment studies- Whole Numbers domain
  V_list <- vcalc(variance_final, cluster= study_id, obs= measure_name, type= domain, rho=c(0.6, 0.6), grp1=intervention_prelim, grp2=comparison_prelim, w1=intervention_n, w2=comparison_n, data=cNMA_data_4.1_NMAmerge_icR_short)
  
  
  ## Fit 1-step additive CNMA- Whole Numbers domain
  mod_addCNMA_icR<- rma.mv(effect_size_final, V_list, 
                           mods = ~ FF + RS + NL + TES + VF - 1, # The "treatment" left out (BAU) becomes the reference level for the comparisons
                           random = ~ 1 | study_id/es_id, 
                           rho=0.60, 
                           data=cNMA_data_4.1_NMAmerge_icR_short)
  summary(mod_addCNMA_icR)  

  ### Estimate all pairwise differences between treatments
  contr <- data.frame(t(combn(names(coef(mod_addCNMA_icR)), 2)))
  contr <- contrmat(contr, "X1", "X2")
  rownames(contr) <- paste(contr$X1, "-", contr$X2)
  contr <- as.matrix(contr[-c(1:2)])
  sav <- predict(mod_addCNMA_icR, newmods=contr)
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
  lt_info_df3 <- lt_info_df2 %>% pivot_wider(id_cols= "comp1", names_from= "comp2", values_from = "pred_cis") #This creates the league table formatted as "left vs top".
  lt_info_df3 <- rename(lt_info_df3, Intervention = comp1)
  print(lt_info_df3)
  write_csv(lt_info_df3, 'nma_league_table_icR_cnma.csv')
  #write_xlsx(lt_info_df3, 'nma_league_table_icR_cnma.xlsx')
  
  ### Compute p-values
  contr <- data.frame(t(combn(c(names(coef(mod_addCNMA_icR)),"BAU"), 2))) # add "BAU" to contrast matrix / Likely to remove this from output/forest plot
  contr <- contrmat(contr, "X1", "X2", last="BAU", append=FALSE)
  b <- c(coef(mod_addCNMA_icR),0) # add 0 for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
  vb <- bldiag(vcov(mod_addCNMA_icR),0) # add 0 row/column for 'BAU' (the "reference treatment" excluded from the mods argument of the rma.mv function executing the NMA above)
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
  mod_addCNMA_icR_df <- tidy(mod_addCNMA_icR, conf.int = TRUE)
  pscores_df <- cbind(term = rownames(pscores), as.data.frame(pscores))
  mod_addCNMA_icR_pscore <- mod_addCNMA_icR_df %>% left_join(pscores_df, by = c("term"))
  mod_addCNMA_icR_pscore <- mod_addCNMA_icR_pscore %>% rename(intervention = term, se = std.error, zval = statistic, pval = p.value, ci.lb = conf.low, ci.ub = conf.high,  Pscore = V1)
  mod_addCNMA_icR_pscore
  
  ### Create forest plot using metafor's built-in function
  # forest(coef(res_mod_icR), diag(vcov(res_mod_icR)), slab=sub(".", " ", names(coef(res_mod_icR)), fixed=TRUE),
  #        #xlim=c(-5,5), alim=c(-3,3), psize=6, header="Intervention", top=2,
  #        header="Intervention",
  #        xlab="Difference in Standardized Mean Change (compared to Control)")
  
  ### Create forest plot using ggplot
  
  #### First create plot of estimates and confidence intervals
  mod_addCNMA_icR_pscore <- mod_addCNMA_icR_pscore %>% arrange(desc(Pscore))
  str(mod_addCNMA_icR_pscore)
  print(mod_addCNMA_icR_pscore)
  # print(num_contrasts_icR_long3)
  # mod_addCNMA_icR_pscore <- mod_addCNMA_icR_pscore %>% left_join(num_contrasts_icR_long3, by = "intervention") # Merge on number of unique contrasts in which each intervention bundle is included
  # print(num_students_icR_long3)
  # tabyl(num_students_icR_long3$intervention_comparison)
  # num_students_icR_long3$intervention_comparison <- gsub("\\+", ".", num_students_icR_long3$intervention_comparison)
  # num_students_icR_long3 <- num_students_icR_long3 %>% rename(num_students = sum_num_students_bundle)
  # mod_addCNMA_icR_pscore <- mod_addCNMA_icR_pscore %>% left_join(num_students_icR_long3, by = c("intervention"= "intervention_comparison")) # Merge on number of unique students in which each intervention bundle is included
  mod_addCNMA_icR_pscore$colour <- rep(c("yellow","burlywood1","royalblue1","green","azure1"))
  str(mod_addCNMA_icR_pscore)
  print(mod_addCNMA_icR_pscore)
  
  mod_addCNMA_icR_pscore_forest <- ggplot(mod_addCNMA_icR_pscore, aes(x= estimate, y= intervention, xmin= ci.lb, xmax= ci.ub)) +  
    geom_hline(aes(yintercept = intervention, colour = colour), size=15) +
    #geom_pointrange(shape = 22, fill = "black", size = mod_addCNMA_icR_pscore$num_contrasts/5) + 
    geom_pointrange(shape = 22, fill = "black") + 
    #geom_text(label = paste0("(",mod_addCNMA_icR_pscore$num_contrasts, " contrast/s, ", mod_addCNMA_icR_pscore$num_students, " students",")"), hjust = 0.5, vjust = 2.95, colour = "black", size =6.5, family= "Times New Roman") +        
    geom_vline(xintercept = 0, linetype = 3) +
    xlab("Difference in Standardized Mean Change (compared to Control)") +
    ylab("Intervention Component") +
    theme_classic() +
    scale_colour_identity() +
    scale_y_discrete(limits = rev(mod_addCNMA_icR_pscore$intervention)) +
    theme(axis.title.x = element_text(face = "bold", size=25, family= "Times New Roman"), #x-axis title
          plot.caption = element_text(size = 18, family= "Times New Roman"), #plot caption (x-axis)
          axis.title.y = element_text(face = "bold", size=25, hjust= 0.44, family= "Times New Roman"), #y-axis title
          axis.text.y = element_text(size=25, hjust=0.5), #the intervention bundles
          axis.text.x = element_text(size = 20, family= "Times New Roman")) #x-axis tick values
  mod_addCNMA_icR_pscore_forest
  
  #### Next create data table for merging with above plot with estimates and confidence intervals combined in one column
  mod_addCNMA_icR_pscore2 <- mod_addCNMA_icR_pscore
  round_digits <- function(x) {
    round(x, digits = 2)
  }
  convert_to_character <- function(x) {
    as.character(x)
  }
  mod_addCNMA_icR_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(mod_addCNMA_icR_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], round_digits)
  mod_addCNMA_icR_pscore2[c("estimate","Pscore","ci.lb","ci.ub")] <- lapply(mod_addCNMA_icR_pscore2[c("estimate","Pscore","ci.lb","ci.ub")], as.character)
  mod_addCNMA_icR_pscore2$ci.lb <- paste("(", mod_addCNMA_icR_pscore2$ci.lb, " -", sep= "")
  mod_addCNMA_icR_pscore2$ci.ub <- paste(mod_addCNMA_icR_pscore2$ci.ub, ")", sep= "")
  mod_addCNMA_icR_pscore2 <- mod_addCNMA_icR_pscore2 %>% unite(estimate_cis, estimate, ci.lb, ci.ub, sep= " ", remove = FALSE )
  print(mod_addCNMA_icR_pscore2)
  
  LfLabels1<-data.frame(x=c(1), 
                        y=c(5.5),
                        lab=c("Estimate (95% CI)"))
  LfLabels1      
  data_table1 <- ggplot(data = mod_addCNMA_icR_pscore2, aes(x, y = intervention)) +
    geom_hline(aes(yintercept = intervention, colour = colour), size = 15) + 
    geom_text(aes(x = 1, label = estimate_cis), size= 9.25, family= "Times New Roman") + 
    geom_text(data=LfLabels1,aes(x,y,label=lab, fontface="bold"), size=8, family= "Times New Roman") + 
    scale_colour_identity() +
    theme_void() + 
    theme(text= element_text(family = "Times New Roman")) +
    scale_y_discrete(limits = rev(mod_addCNMA_icR_pscore$intervention)) 
  data_table1
  
  LfLabels2<-data.frame(x=c(1), 
                        y=c(5.5),
                        lab=c("P-score"))
  LfLabels2      
  data_table2 <- ggplot(data = mod_addCNMA_icR_pscore2, aes(x, y = intervention)) +
    geom_hline(aes(yintercept = intervention, colour = colour), size = 15) + 
    geom_text(aes(x = 1, label = Pscore), size= 9.25, family= "Times New Roman") +
    geom_text(data=LfLabels2,aes(x,y,label=lab, fontface="bold"), size=8, family= "Times New Roman") +
    scale_colour_identity() +
    theme_void() + 
    theme(text= element_text(family = "Times New Roman")) +
    scale_y_discrete(limits = rev(mod_addCNMA_icR_pscore$intervention)) 
  data_table2
  
  #### Finally, merge plot and datatable for final forest plot
  aligned_plots <- align_plots(mod_addCNMA_icR_pscore_forest, data_table1, data_table2, align = "h")
  final_fp_nma_icR <- grid.arrange(grobs = aligned_plots, nrow= 1, widths= c(2.5,0.75,0.5))
  final_fp_nma_icR  
  
  
  
  