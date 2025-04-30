
rename (ContrastNumbersimple_number ContrastNamecontrast_name DomainSourceNNMAdatabase) ///
	   (contrast_simple_number contrast domain)
rename (Studyrecord_idSourceNNM EffectsizeIDSourceNNMA) ///
	   (study_id es_id)
rename MRGlink mrg_link
rename LevelofAssignmentSource level_of_assignment
rename (OutcomeSourceNNMAdatabase Outcometypecontinuousdichoto) ///
       (outcome outcome_type)
rename Analysismethodeffectsizecalc analytic_method
rename (Interventionindividualanalytic Comparisonindividualanalytics Totalindividualanalyticsample Interventionclusteranalyticsa Comparisonclusteranalyticsamp TotalclusteranalyticsampleN AveragenperclusterNavg) ///
	   (n_i_indiv n_c_indiv n_t_indiv n_i_cluster n_c_cluster n_t_cluster n_avg_cluster)
rename (Interventionoutcomeadjustedme Comparisonoutcomeadjustedmean Interventionoutcomeunadjusted Comparisonoutcomeunadjustedme X Comparisonoutcomeunadjustedst) ///
	   (mean_i_adj mean_c_adj mean_i_unadj mean_c_unadj mean_i_sd mean_c_sd)
rename (Didoriginalstudyneedtoclust Regessioncoefficientunstandar /*Didoriginalstudyneedtoclust*/ Regressioncoefficientstandard AL ModelRsquaredR2Source DegreesoffreedomhSour tstatistictSourceMRG ANOVAFstatisticFSourc IntraclassCorrelationCoefficie AR) ///
	   (study_clustered regression_coef /*study_need_to_cluster*/ regression_coef_se_uc regression_coef_se_cc model_Rsqrd dof t_stat anova_f_stat icc log_odds_ratio)
rename (Outcomeeffectsizereportedas EffectsizestandarderrorSo) ///
       (es_official_40 se_e14_40)
rename (Areoutcomeandbaselinethesam Baselineoutcomecorrelationp) ///
	   (pre_post_same_measure pre_post_correlation)
rename LogoddsratioLORcalcula log_odds_post	   