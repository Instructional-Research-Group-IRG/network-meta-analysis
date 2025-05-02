
rename (ContrastNumbersimple_number MRGlink ContrastNamecontrast_name DomainSourceNNMAdatabase Studyrecord_idSourceNNM StudyCitationfull_citation EffectsizeIDSourceNNMA) ///
	   (contrast_simple_number mrg_link contrast domain study_id citation es_id)
	   
rename (LevelofAssignmentSource OutcomeSourceNNMAdatabase Outcometypecontinuousdichoto Analysismethodeffectsizecalc) ///
       (level_of_assignment outcome outcome_type analytic_method)
	   
rename (Interventionindividualanalytic Comparisonindividualanalytics Totalindividualanalyticsample Interventionclusteranalyticsa Comparisonclusteranalyticsamp TotalclusteranalyticsampleN AveragenperclusterNavg) ///
	   (n_i_indiv n_c_indiv n_t_indiv n_i_cluster n_c_cluster n_t_cluster n_avg_cluster)
	   
rename (Interventionoutcomeadjustedme Comparisonoutcomeadjustedmean Interventionoutcomeunadjusted Comparisonoutcomeunadjustedme X Comparisonoutcomeunadjustedst Adjustedgroupmeandifference) ///
	   (mean_i_adj mean_c_adj mean_i_unadj mean_c_unadj mean_i_sd mean_c_sd mean_dif_adj)
	
rename (Interventionbaselinemeanypr Comparisonbaselinemeanypre Interventionbaselineunadjusted Comparisonbaselineunadjusteds) ///
	   (mean_i_bl mean_c_bl mean_i_blsd mean_c_blsd)
	
rename (Regessioncoefficientunstandar Didoriginalstudyneedtoclust Regressioncoefficientstandard AL ModelRsquaredR2Source DegreesoffreedomhSour tstatistictSourceMRG ANOVAFstatisticFSourc IntraclassCorrelationCoefficie) ///
	   (regression_coef study_clustered regression_coef_se_uc regression_coef_se_cc model_Rsqrd dof t_stat anova_f_stat icc)

rename (Outcomeeffectsizereportedas EffectsizestandarderrorSo VarianceSourceNNMAdatabas Outcomeeffectsizepvaluerepo Outcomeeffectsizestatistical Baselineeffectsizereported) ///
       (es_official_40 se_e14_40 variance p_value stat_sig es_baseline_40)
	   
rename (AZ BA) ///
	   (notes1 notes2)
	   
rename (Areoutcomeandbaselinethesam Baselineoutcomecorrelationp) ///
	   (pre_post_same_measure pre_post_correlation)	   
	   
rename (Smallsamplebiascorrection LogoddsratioLORcalcula PooledstandarddeviationS) ///
       (omega log_odds_ratio sd_pooled)
	   
drop AR // duplicate column for log odds ratio

label variable outcome_type "outcome type"
label variable analytic_method "analytic method"
label variable es_official_40 "official effect size, 4.0"
label variable se_e14_40 "standard error of effect size, 4.0"
label variable level_of_assignment "level of assignment"
label variable study_clustered "did the authors' regression model properly adjust for clustering?"
label variable notes1 "notes- 1"
label variable notes2 "notes- 2"