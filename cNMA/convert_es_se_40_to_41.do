
capture log close _all
log using "C:\Users\sethb\Documents\Career\freelance\IRG\assignments\network meta-analysis\network-meta-analysis\cNMA\convert_es_se_40_to_41.log", replace name(convert_es_se)


*************************************************************************************************
***                                                                                           ***
*** Program name: convert_es_se_40_to_41.do                                                   ***
*** Project: Component Network Meta-Analysis												  ***
*** Purpose: Convert effect sizes and their standard errors from 4.0 to 4.1 WWC Standards     ***    
***																							  ***
*** Contents:																				  ***
***    0) SET UP CODE																		  ***
***    I) PREPARE CNMA DATABASE																  ***
***    II) CONVERT EFFECT SIZES FROM 4.0 to 4.1												  ***
***    III) CONVERT STANDARD ERRORS OF EFFECT SIZES FROM 4.0 to 4.1     					  ***
***																							  ***
*** Authors: Seth B. Morgan																      ***
*** Start date: February 19, 2025															  ***
*** Last date modified: April 29, 2025												          ***
***																							  ***
*** Notes:																					  ***
***																							  ***
***																						      ***
*************************************************************************************************

clear all
version 19.5
set more off
set varabbrev off
pause off


*=========================================================================================
* 0) SET UP CODE
*=========================================================================================	

/* Set seed */
	set seed 7122024
	
/* Define globals */

	*Root
	global root "C:\Users\sethb\Documents\Career\freelance\IRG\assignments\network meta-analysis\network-meta-analysis\cNMA"
    adopath ++ "$root"
    

*=========================================================================================
* I) PREPARE CNMA DATABASE
*=========================================================================================

	/* Load in cNMA database */
	import excel "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 CLEAN.xlsx", sheet("Data inventory for 4.0 to 4.1") firstrow clear
	save "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 CLEAN.dta", replace

	/* Clean up variable names */
	include "$root\convert_es_se_40_to_41_revise_variable_names.do"
	
	/* Drop uneccesary columns */
	drop StudyCitationfull_citation
	order mrg_link, last
	
	/*Keep only valid rows */
	count
	drop if missing(contrast_simple_number)
	count
	
	/* Save cNMA database as .dta */
	save "$root\Data inventory for 4.0 to 4.dta", replace
	
	/* Explore key variables */
	tab1 level_of_assignment analytic_method outcome_type, missing
	tablist level_of_assignment analytic_method outcome_type, sort(v) ab(32)
	tablist contrast_id study_id if missing(outcome_type) & !missing(level_of_assignment), sort(v) ab(32)
	tablist contrast_id study_id if missing(outcome_type) & missing(level_of_assignment), sort(v) ab(32)
	
	/* Clean up string variables */
	foreach var of varlist _all {
		capture confirm string variable `var'
		if _rc==0 {
			display as input _n "String variable: `var'"
			replace `var'= strtrim(stritrim(`var'))
		}
	}
	
	/* Clean up numeric variables */
	replace regression_coef_se_uc="" if regression_coef_se_uc=="Not presented in report"
	replace regression_coef_se_cc="" if regression_coef_se_cc=="Not presented in report"
	replace regression_coef_se_cc="" if regression_coef_se_cc=="Not reported in main article."
	replace model_Rsqrd="" if model_Rsqrd=="Not reported"
	replace model_Rsqrd="" if model_Rsqrd=="Not presented in report"
	foreach var of varlist mean_?_*adj mean_?_sd regression_coef_se_?c model_Rsqrd {
		display as input _n "Numeric variable: `var'"
		destring `var', replace
	}
	
	/* Calculate necessary variables from study-reported data */
			
		*-> Total sample of individuals
			assert !missing(n_t_indiv) 
			tablist study_id contrast_id n_i_indiv n_c_indiv n_t_indiv if n_i_indiv + n_c_indiv != n_t_indiv, sort(v) ab(32)
			replace n_t_indiv= 934 if study_id=="MI20802" & contrast_id=="86426"
			assert n_i_indiv + n_c_indiv == n_t_indiv
			tablist n_i_indiv n_c_indiv n_t_indiv, sort(v)

		*-> Total sample of clusters
			assert missing(n_t_cluster) if level_of_assignment!="cluster"
			tablist level_of_assignment n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster", sort(v)
			replace n_t_cluster= n_i_cluster + n_c_cluster
			tablist level_of_assignment n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster", sort(v)
			tablist level_of_assignment contrast_id n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster" & missing(n_t_cluster), sort(v) ab(32) // (!) Send to team for correction in database- contrast IDs: 87196, 90508.
			
		*-> Average number of individuals per cluster
			tablist level_of_assignment level_of_assignment n_i_indiv n_c_indiv n_t_indiv n_i_cluster n_c_cluster n_t_cluster n_avg_cluster if level_of_assignment=="cluster", sort(v) ab(32)
			replace n_avg_cluster= n_t_indiv/n_t_cluster
			tablist level_of_assignment n_i_indiv n_c_indiv n_t_indiv n_i_cluster n_c_cluster n_t_cluster n_avg_cluster if level_of_assignment=="cluster", sort(v) ab(32)
			assert n_avg_cluster==. if level_of_assignment!="cluster"
			
		*-> Small-sample bias adjustment (ω)
			generate omega= .
			label variable omega "small-sample bias adjustment (ω)"
			replace omega= (1-(3/((4*(n_i_indiv + n_c_indiv))-9)))
			summarize omega
			assert !missing(omega)
			assert missing(Smallsamplebiascorrection)
			drop Smallsamplebiascorrection
			
		*-> Pooled standard deviation (S)
			replace mean_i_sd="10.4" if  mean_i_sd=="`10.4"
			destring mean_i_sd, replace
			generate S_pooled_sd = sqrt( ( ((n_i_indiv-1)*mean_i_sd^2) + ((n_c_indiv-1)*mean_c_sd^2) ) / (n_i_indiv + n_c_indiv - 2) )
			label variable S_pooled_sd "pooled standard deviation"
			tablist level_of_assignment S_pooled_sd n_i_indiv n_c_indiv mean_i_sd mean_c_sd, sort(v) ab(32)
			
		*-> Log odds ratio
			tablist level_of_assignment analytic_method outcome_type mean_?_unadj mean_?_adj log_odds_post if outcome_type=="dichotomous", sort(v) ab(32)
			foreach var of varlist mean_?_*adj {
				replace `var'= `var' / 100 if outcome_type=="dichotomous"
			}
			generate OR_adj= (mean_i_adj * (1-mean_c_adj))  /  (mean_c_adj * (1-mean_i_adj)) if outcome_type=="dichotomous"
			replace log_odds_post= ln(OR_adj) if outcome_type=="dichotomous"  
			generate OR_unadj= (mean_i_unadj * (1-mean_c_unadj))  /  (mean_c_unadj * (1-mean_i_unadj)) if outcome_type=="dichotomous"
			replace log_odds_post= ln(OR_unadj) if missing(log_odds_post) & outcome_type=="dichotomous" // (!) Use unadjusted if adjusted not available?
			tablist level_of_assignment analytic_method outcome_type mean_?_unadj OR_unadj mean_?_adj OR_adj log_odds_post if outcome_type=="dichotomous", sort(v) ab(32)
			
			
*=========================================================================================
* II) CONVERT EFFECT SIZES FROM 4.0 to 4.1
*=========================================================================================  

	/* Prepare data and variables for ES conversions */
	summarize es_official_40, detail
	list study_id contrast_id outcome outcome_type es_official_40 mrg_link if missing(es_official_40), ab(32)
	drop if missing(es_official_40) // (!)
	
	generate es_converted_41=.
	label variable es_converted_41 "effect size converted from 4.0 to 4.1"

	/* Individual-level Assignment */
	tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual", sort(v) ab(32)
		
		*-> Continuous Measures
			
			tablist level_of_assignment analytic_method outcome_type, sort(v) ab(32)
			tablist level_of_assignment analytic_method outcome_type if level_of_assignment=="individual" & outcome_type=="continuous", sort(v) ab(32)
			count if level_of_assignment=="individual" & outcome_type=="continuous"
			
			//Note: For continuous measures with individual-level assignment using any analytical method besides difference-in-differences, the v4.1 effect size calulations are the same as under v4.0.
			replace es_converted_41=es_official_40 if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method!="difference-in-differences"
			
			//Note: For continuous measures with individual-level assignment analyzed using difference-in-differences, the v4.1 effect size calulations are:
			//			• the same as under v4.0 if the pre- and posttests are not the same; but 
			//     		• different from v4.0 if the pre- and posttests are the same.
			//      However, there are no continuous measures with individual-level assignment analyzed using difference-in-differences in the NNMA database.
			assert !(level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="difference-in-differences")
			
			summarize es_official_40 es_converted_41 if level_of_assignment=="individual" & outcome_type=="continuous"
			
			/*(!) Think about these checks and recalcs a bit more.
			**-->> Recalculate effect sizes based on no adjustment as a check
			generate es_converted_41_check=.
			tablist level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj S_pooled_sd es_official_40 es_converted_41 if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment", sort(v) ab(32)
			replace es_converted_41_check= (omega * (mean_i_unadj - mean_c_unadj)) / S_pooled_sd if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment"
			tablist level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj S_pooled_sd es_official_40 es_converted_41 es_converted_41_check if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment" & !missing(es_converted_41_check), sort(v) ab(32)
			summarize es_converted_41 es_converted_41_check if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment" & !missing(es_converted_41_check)
			summarize es_converted_41 es_converted_41_check if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment" & !missing(es_converted_41_check) // (!) Some very different. Study-reported ES not WWC Hedges' g, for example?
			
			*-> Additional adjustment if officially reported effect size calculated with unadjusted means but adjusted means are available
			// Note: (!) Ask team about this. (!) Also about negative effect sizes if positive result is decrease in outcome.
			tablist es_nochange level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj mean_i_sd mean_c_sd es_official_40 es_converted_41 if level_of_assignment=="individual" & analytic_method=="no adjustment" & outcome_type=="continuous" & !missing(mean_i_sd) & !missing(mean_c_sd), sort(v) ab(32)
			replace es_nochange=2 if level_of_assignment=="individual" & analytic_method=="no adjustment" & outcome_type=="continuous" & !missing(mean_i_sd) & !missing(mean_c_sd)
			replace es_converted_41= (omega * (mean_i_adj - mean_c_adj)) / S_pooled_sd if es_nochange==2
			tablist es_nochange level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj mean_i_sd mean_c_sd es_official_40 es_converted_41 if es_nochange==2, sort(v) ab(32)
			*/
		
		*-> Dichotmous Measures
			
			tablist level_of_assignment analytic_method outcome_type, sort(v) ab(32)
			tablist level_of_assignment analytic_method outcome_type log_odds_post if level_of_assignment=="individual" & outcome_type=="dichotomous", sort(v) ab(32)			
			count if level_of_assignment=="individual" & outcome_type=="dichotomous"
			
			//Note: For dichotomous measures with individual-level assignment using analytical any method besides difference-in-differences:
			//			• the Cox effect size (d_Cox) was calculated with the small-sample bias adjustment omega under v4.0 (p. E-7, v4.0 procedures handbook);
			//          • however, under v4.1, this adjustment is not applied (equation E.4.3, v4.1 procedures handbook).
			replace es_converted_41= log_odds_post if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences" 
			tablist level_of_assignment analytic_method outcome_type outcome log_odds_post es_official_40 es_converted_41 omega if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences", sort(v) ab(32) // (!) These 4.1 recalculations are not similar to 4.0 values. 

			//Note: For dichotomous measures with individual-level assignment using difference-in-differences:
			//			• the Cox effect size (d_Cox) was calculated with the small-sample bias adjustment omega under v4.0 (p. E-7, v4.0 procedures handbook);
			//          • however, under v4.1, this adjustment is not applied (equation E.4.3, v4.1 procedures handbook);
			//          • in addition, v4.1 adds the population correlation to the diff-in-diff adjustment compared to v4.0
			//      However, there are no dichotomous measures with individual-level assignment analyzed using difference-in-differences in the NNMA database.
			assert !(level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method=="difference-in-differences")
			
			summarize es_official_40 es_converted_41 if level_of_assignment=="individual" & outcome_type=="dichotomous"
	
	/* Cluster-level Assignment */
	//Note:  All cluster-level assignment studies report student-level effect sizes (i.e., there are no cluster-level effect sizes reported in the cNMA database). (!) confirm with RTL
	tablist level_of_assignment analytic_method outcome_type if level_of_assignment=="cluster"
	
		*-> Continuous Measures
			
			tablist level_of_assignment analytic_method outcome_type, sort(v) ab(32)
			tablist level_of_assignment analytic_method outcome_type if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32)
			count if level_of_assignment=="cluster" & outcome_type=="continuous"
			
			//Note: For continuous measures with cluster-level assignment, recalculate the student-level ES using the unbiased formula in Table 1 of the Supplement, which imposes a bias correction, in place of [E.5.1] on page E-9 of v4.1 Procedures 
			//      which is the same equation as on page E-9 of v4.0 Procedures. Note that that the bias correction uses the intraclass correlation coefficient (ICC).
			
			tablist level_of_assignment analytic_method outcome_type icc if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32) 
			replace icc="." if inlist(icc,"Not in article","Not in article but see comment","Not presented in report","Not reported in main article.","Not reported in the main article","Not reported in this study") 
			destring icc, replace
			replace icc= 0.2 if icc==. & level_of_assignment=="cluster" & outcome_type=="continuous" // (!) Placeholder. Needs to be updated by team. "As defaults, the WWC uses the ICC values of .20 for achievement outcomes and .10 for all other outcomes, but will use study-reported ICC values when available. (Procedures handbook 4.1, p. 20)"
			tablist level_of_assignment analytic_method outcome_type icc if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32) // (!) Use 0.2 if ICC missing?
			
			*Calculate 4.1 effect size for outcome measures of cluster-level assignment studies: unbiased [E.5.1] formula of Table 1 of the Supplement
			replace es_converted_41 = es_official_40 * sqrt( 1 - ( (2*(n_avg_cluster-1)*icc) / n_t_indiv-2) ) if level_of_assignment=="cluster" & outcome_type=="continuous"
			tablist analytic_method es_official_40 es_converted_41 icc n_avg_cluster n_t_cluster n_t_indiv if level_of_assignment=="cluster"  & outcome_type=="continuous", sort(v) ab(32)
			replace es_converted_41 = es_official_40 if missing(es_converted_41) & level_of_assignment=="cluster" & outcome_type=="continuous"
			tablist analytic_method es_official_40 es_converted_41 icc n_avg_cluster n_t_cluster n_t_indiv if level_of_assignment=="cluster"  & outcome_type=="continuous", sort(v) ab(32)
			
			summarize es_official_40 es_converted_41 if level_of_assignment=="cluster" & outcome_type=="continuous"
			
		*-> Dichotmous Measures
			
			tablist level_of_assignment analytic_method outcome_type, sort(v) ab(32)
			capture tablist level_of_assignment analytic_method outcome_type log_odds_post if level_of_assignment=="cluster" & outcome_type=="dichotomous", sort(v) ab(32)			
			count if level_of_assignment=="cluster" & outcome_type=="dichotomous"			

			//Note: For continuous measures with cluster-level assignment, the calculation of ESs is not explicitly covered in either v4.0 or v4.1 Prociedures or v4.1 Supplement.
			//      However, there are no dichotomous measures with cluster-level assignment in the NNMA database.
			assert !(level_of_assignment=="cluster" & outcome_type=="dichotomous")
	
	
*=========================================================================================
* III) CONVERT STANDARD ERRORS OF EFFECT SIZES FROM 4.0 to 4.1
*=========================================================================================     

	//Note: Effect size standard errors (SEs) were not calculated under 4.0. The SEs in this database were calculated using equation [E.1.4] of Procedures Handbook Version 4.1 (page E.3).

	/* Prepare data and variables for SE conversions */
	summarize se_e14_40, detail
	generate se_converted_41=.
	label variable se_converted_41 "standard error of effect size converted from 4.0 to 4.1"
	
	/* Individual-level Assignment */
	tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual", sort(v) ab(32)	
		
		*-> Continuous measures
		tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & outcome_type=="continuous", sort(v) ab(32)
		
			**-> ANCOVA or regression: decision tree
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous", sort(v)	ab(32)
			tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc model_Rsqrd t_stat if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous", sort(v)	ab(32)
				
				***-> Indiv/ANCOVAregression/continuous; regression coefficient standard error reported: ([E.7.1] supp, Individual assignment column of Table 2 of Supplement)
				count if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & (!missing(regression_coef_se_uc) | !missing(regression_coef_se_cc) )
				replace se_converted_41= omega * sqrt( ((regression_coef_se_uc/S_pooled_sd)^2) + ((es_converted_41^2)/(2*n_t_indiv)) ) if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & !missing(regression_coef_se_uc)
				replace se_converted_41= omega * sqrt( ((regression_coef_se_cc/S_pooled_sd)^2) + ((es_converted_41^2)/(2*n_t_indiv)) ) if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & !missing(regression_coef_se_cc)
				
				***-> Indiv/ANCOVAregression/continuous; model R2 value is reported: ([E.2.2] supp/v4.1, Individual assignment column of Table 3 of Supplement; same as [E.2.2] in v4.1 Procedures, page E-5)
				// Note: None
				count if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & !missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc)
				assert r(N)==0
			
				***-> Indiv/ANCOVAregression/continuous; cannot use either of the two above AND/OR only t-statistic reported: (SE=b/t)
				// Note: None
				count if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc)
				replace se_converted_41 = (regression_coef / t_stat) if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc)
			
				***-> Indiv/ANCOVAregression/continuous; cannot use either of the three above: (no change/ [E.1.4] v4.1, page E-3, v4.1 procedures handbook)
				replace se_converted_41= se_e14_40 if missing(se_converted_41) & level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous"
				
			tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc model_Rsqrd t_stat se_converted_41 se_e14_40 if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous", sort(v)	ab(32) 
			
			**-> Gain Scores
			// Note: None
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & outcome_type=="continuous", sort(v) ab(32)
			assert analytic_method!="gain score" if level_of_assignment=="individual" & outcome_type=="continuous"
			
			**-> Difference-in-differences
			// Note: None
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & outcome_type=="continuous", sort(v) ab(32)
			assert analytic_method!="difference-in-differences" if level_of_assignment=="individual" & outcome_type=="continuous"
			
			**-> No adjustment
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual", sort(v) ab(32)
			replace se_converted_41= se_e14_40 if level_of_assignment=="individual" & analytic_method=="no adjustment" & outcome_type=="continuous"

		*-> Dichotomous measures
		tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual"  & outcome_type=="dichotomous", sort(v) ab(32)	
		
			**-> Indiv/DnD/dichotomous: ([E.4.4], Individual assignment column of Table 3 of Supplement)
			//Note: None
			assert analytic_method!="difference-in-differences" if level_of_assignment=="individual" & outcome_type=="dichotomous"
			
			**-> Indiv/AnyOtherAnalysis/dichotomous: ([E.4.4], Individual assignment column of Table 3 of Supplement)
**# Bookmark #1
			********replace se_converted_41= (1/1.65) * ( sqrt( ) )
			
	/* Cluster-level Assignment */
	//Note:  All cluster-level assignment studies report student-level effect sizes (i.e., there are no cluster-level effect sizes reported in the cNMA database). (!) confirm with RTL
	tablist level_of_assignment analytic_method outcome_type if level_of_assignment=="cluster", sort(v) ab(32)
	
	/* Save final ES/SE conversion database */
	quietly compress
	sort contrast_simple_number
	save "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 COMPLETED.dta", replace 
	
	
*=========================================================================================
* IV) DESCRIPTIVE STATISTICS
*=========================================================================================     
	
	/* */


log close convert_es_se
