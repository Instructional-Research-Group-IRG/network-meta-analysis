
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
*** Last date modified: May 1, 2025												              ***
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
	import excel "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 CLEAN.xlsx", sheet("DATABASE Converting ESs and SEs") firstrow clear
	save "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 CLEAN.dta", replace

	/* Revise variable names */
	include "$root\convert_es_se_40_to_41_revise_variable_names.do"
	
	/* Drop uneccesary columns */
	order citation mrg_link, last
	
	/*Keep only valid rows */
	count
	drop if missing(contrast_simple_number) // (!) Asking Sarah K. about the contrast IDs added to lines 400-508 of the database with no other info (no study ID, ES ID, effect size, se, etc.)
	count
	
	/* Save cNMA database as .dta */
	save "$root\Data inventory for converting ESs & SEs from 4.0 to 4.1.dta", replace
	
	/* Explore key variables */
	tab1 level_of_assignment analytic_method outcome_type, missing
	tablist level_of_assignment outcome_type analytic_method, sort(v) ab(32)
**# Bookmark #1
	tablist contrast_simple_number contrast_id study_id es_id if missing(outcome_type) & missing(level_of_assignment), sort(v) ab(32)
		* contrast-id==56; es_id== 201: "NP: Outcome does not appear to be included in MRG. No QCd but see note if we want to think about entering information for this outcome."
		* contrast-id==57; es_id==205 - 208: "NP: MRG not in Dropbox folder"
		* contrast-id==93; es_id==335 - 340: "NP: I don't see this MRG in the Dropbox folder."
		* contrast_simple_number d-0: Asking if these are additions by RTL b/c MRGs say not eligible/not reviewed but database inlcudes sample sizes, effect sizes, and standard errors for these.
	
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
	replace icc="." if inlist(icc,"Not in article","Not in article but see comment","Not presented in report","Not reported in main article.","Not reported in the main article","Not reported in this study")
	replace mean_i_sd="10.4" if mean_i_sd=="`10.4"
	foreach var of varlist mean_?_*adj mean_?_sd regression_coef_se_?c model_Rsqrd icc {
		display as input _n "Numeric variable: `var'"
		destring `var', replace
	}
	
	/* Calculate necessary variables from study-reported data */
			
		*-> Total sample of individuals
			assert !missing(n_t_indiv) 
			tablist study_id contrast_id es_id n_i_indiv n_c_indiv n_t_indiv if n_i_indiv + n_c_indiv != n_t_indiv, sort(v) ab(32)
			replace n_t_indiv=  n_i_indiv +  n_c_indiv if study_id=="MI20802" & contrast_id=="86426" & es_id==167
			assert n_i_indiv + n_c_indiv == n_t_indiv
			tablist n_i_indiv n_c_indiv n_t_indiv, sort(v)

		*-> Total sample of clusters
			assert missing(n_t_cluster) if level_of_assignment!="cluster"
			tablist level_of_assignment n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster", sort(v)
			replace n_t_cluster= n_i_cluster + n_c_cluster
			tablist level_of_assignment n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster", sort(v)
			tablist level_of_assignment study_id contrast_id es_id n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster" & missing(n_t_cluster), sort(v) ab(32) 
				*study_id=="MI21847" & contrast_id=="87196" & es_id=="203" -> cluster sample sizes not reported for this outcome in MRG
			
		*-> Average number of individuals per cluster
			tablist level_of_assignment level_of_assignment n_i_indiv n_c_indiv n_t_indiv n_i_cluster n_c_cluster n_t_cluster n_avg_cluster if level_of_assignment=="cluster", sort(v) ab(32)
			replace n_avg_cluster= n_t_indiv/n_t_cluster
			tablist study_id contrast_id level_of_assignment n_i_indiv n_c_indiv n_t_indiv n_i_cluster n_c_cluster n_t_cluster n_avg_cluster if level_of_assignment=="cluster", sort(v) ab(32)
			assert missing(n_avg_cluster) if level_of_assignment!="cluster"
			
		*-> Small-sample bias adjustment (ω)
			assert omega==.
			label variable omega "small-sample bias adjustment (ω)"
			replace omega= (1-(3/((4*n_t_indiv)-9)))
			summarize omega
			assert !missing(omega)
			assert inrange(omega, 0.9, 0.9999)
				
		*-> Pooled standard deviation (S)
			replace sd_pooled = sqrt( ( ((n_i_indiv-1)*mean_i_sd^2) + ((n_c_indiv-1)*mean_c_sd^2) ) / (n_i_indiv + n_c_indiv - 2) )
			label variable sd_pooled "pooled standard deviation"
			tablist level_of_assignment sd_pooled n_i_indiv n_c_indiv mean_i_sd mean_c_sd, sort(v) ab(32)
			summarize sd_pooled mean_i_sd mean_c_sd
			
		*-> Log odds ratio
			tablist study_id contrast_id es_id level_of_assignment analytic_method outcome_type mean_?_unadj mean_?_adj log_odds_ratio if outcome_type=="dichotomous", sort(v) ab(32)
			foreach var of varlist mean_?_*adj {
				replace `var'= `var' / 100 if outcome_type=="dichotomous"
			}
			generate OR=.
			replace OR= (mean_i_unadj * (1-mean_c_unadj))  /  (mean_c_unadj * (1-mean_i_unadj)) if outcome_type=="dichotomous"
			replace log_odds_ratio= ln(OR) if outcome_type=="dichotomous"  
			tablist study_id contrast_id es_id level_of_assignment analytic_method outcome_type mean_?_unadj mean_?_adj OR log_odds_ratio if outcome_type=="dichotomous", sort(v) ab(32)
			
			
*=========================================================================================
* II) CONVERT EFFECT SIZES FROM 4.0 to 4.1
*=========================================================================================  

	/* Prepare data and variables for ES conversions */
	summarize es_official_40, detail
	list study_id contrast_simple_number contrast_id es_id outcome outcome_type es_official_40 mrg_link if missing(es_official_40), ab(32)
	
	generate es_converted_41=.
	label variable es_converted_41 "effect size, 4.1"

	/* Individual-level Assignment */
	tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual", sort(v) ab(32)
		
		*-> Continuous Measures
			
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & outcome_type=="continuous", sort(v) ab(32)
			count if level_of_assignment=="individual" & outcome_type=="continuous"
			
			//Note: For continuous measures with individual-level assignment using any analytical method besides difference-in-differences, the v4.1 effect size calulations are the same as under v4.0.
			replace es_converted_41=es_official_40 if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method!="difference-in-differences"
			
			//Note: For continuous measures with individual-level assignment analyzed using difference-in-differences, the v4.1 effect size calulations are:
			//			• the same as under v4.0 if the pre- and posttests are not the same; but 
			//     		• different from v4.0 if the pre- and posttests are the same.
			//      However, there are no continuous measures with individual-level assignment analyzed using difference-in-differences in the NNMA database.
			count if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="difference-in-differences"
			assert r(N)==0
			
			tablist level_of_assignment analytic_method outcome_type es_official_40 es_converted_41 if level_of_assignment=="individual" & outcome_type=="continuous", sort(v) ab(32)
			assert !missing(es_converted_41) if level_of_assignment=="individual" & outcome_type=="continuous"
			summarize es_official_40 es_converted_41 if level_of_assignment=="individual" & outcome_type=="continuous"
			
			/*
			**-->> Recalculate effect sizes based on no adjustment as a check
			generate es_converted_41_check_unadj=.
			generate es_converted_41_check_adj=.
			
			tablist level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj sd_pooled es_official_40 es_converted_41 if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment", sort(v) ab(32)
			replace es_converted_41_check_unadj= (omega * (mean_i_unadj - mean_c_unadj)) / sd_pooled if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment"
			replace es_converted_41_check_adj= (omega * (mean_i_adj - mean_c_adj)) / sd_pooled if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment"
			tablist analytic_method outcome_type mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj sd_pooled es_official_40 es_converted_41 es_converted_41_check_unadj es_converted_41_check_adj if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment", sort(v) ab(32)	
			summarize es_converted_41 es_converted_41_check_unadj es_converted_41_check_adj if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="no adjustment"
			*/
			
		*-> Dichotmous Measures
			
			tablist level_of_assignment analytic_method outcome_type log_odds_ratio if level_of_assignment=="individual" & outcome_type=="dichotomous", sort(v) ab(32)			
			count if level_of_assignment=="individual" & outcome_type=="dichotomous"
			
			//Note: For dichotomous measures with individual-level assignment using analytical any method besides difference-in-differences:
			//			• the Cox effect size (d_Cox) was calculated with the small-sample bias adjustment omega under v4.0 (p. E-7, v4.0 procedures handbook);
			//          • however, under v4.1, this adjustment is not applied (equation E.4.3, v4.1 procedures handbook).
			replace es_converted_41= log_odds_ratio/1.65 if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences" // (!) Double check LOR calculation given differences in ES values between 4.0 and 4.1.
			tablist level_of_assignment analytic_method outcome_type outcome log_odds_ratio es_official_40 es_converted_41 omega if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences", sort(v) ab(32) // (!) These 4.1 recalculations are not similar to 4.0 values. 

			//Note: For dichotomous measures with individual-level assignment using difference-in-differences:
			//			• the Cox effect size (d_Cox) was calculated with the small-sample bias adjustment omega under v4.0 (p. E-7, v4.0 procedures handbook);
			//          • however, under v4.1, this adjustment is not applied (equation E.4.3, v4.1 procedures handbook);
			//          • in addition, v4.1 adds the population correlation to the diff-in-diff adjustment compared to v4.0
			//      However, there are no dichotomous measures with individual-level assignment analyzed using difference-in-differences in the NNMA database.
			count if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method=="difference-in-differences"
			assert r(N)==0
			
			tablist level_of_assignment analytic_method outcome_type es_official_40 es_converted_41 omega if level_of_assignment=="individual" & outcome_type=="dichotomous", sort(v) ab(32)
			assert !missing(es_converted_41) if level_of_assignment=="individual" & outcome_type=="dichotomous"
			summarize es_official_40 es_converted_41 if level_of_assignment=="individual" & outcome_type=="dichotomous"
	
	/* Cluster-level Assignment */
	//Note:  All cluster-level assignment studies report student-level effect sizes (i.e., there are no cluster-level effect sizes reported in the cNMA database). 
	tablist level_of_assignment analytic_method outcome_type if level_of_assignment=="cluster"
	
		*-> Continuous Measures
			
			tablist level_of_assignment analytic_method outcome_type if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32)
			count if level_of_assignment=="cluster" & outcome_type=="continuous"
			
			//Note: For continuous measures with cluster-level assignment, recalculate the student-level ES using the unbiased formula in Table 1 of the Supplement, which imposes a bias correction, in place of [E.5.1] on page E-9 of v4.1 Procedures 
			//      which is the same equation as on page E-9 of v4.0 Procedures. Note that the bias correction uses the intraclass correlation coefficient (ICC).
			tablist level_of_assignment analytic_method outcome_type icc if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32)  
			replace icc= 0.2 if icc==. & level_of_assignment=="cluster" & outcome_type=="continuous" // (!) "As defaults, the WWC uses the ICC values of .20 for achievement outcomes and .10 for all other outcomes, but will use study-reported ICC values when available. (Procedures handbook 4.1, p. 20)"
			tablist level_of_assignment analytic_method outcome_type icc if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32)
			
			*Calculate 4.1 effect size for outcome measures of cluster-level assignment studies: unbiased [E.5.1] formula of Table 1 of the Supplement
			replace es_converted_41 = es_official_40 * sqrt( 1 - ( (2*(n_avg_cluster-1)*icc) / (n_t_indiv-2) ) ) if level_of_assignment=="cluster" & outcome_type=="continuous"
			tablist level_of_assignment analytic_method outcome_type es_official_40 es_converted_41 icc n_avg_cluster n_t_cluster n_t_indiv if level_of_assignment=="cluster"  & outcome_type=="continuous", sort(v) ab(32)
			replace es_converted_41 = es_official_40 if missing(es_converted_41) & level_of_assignment=="cluster" & outcome_type=="continuous"
			tablist level_of_assignment analytic_method outcome_type es_official_40 es_converted_41 icc n_avg_cluster n_t_cluster n_t_indiv if level_of_assignment=="cluster"  & outcome_type=="continuous", sort(v) ab(32)
			
			tablist level_of_assignment analytic_method outcome_type es_official_40 es_converted_41 if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32)
			assert !missing(es_converted_41) if level_of_assignment=="cluster" & outcome_type=="continuous"
			summarize es_official_40 es_converted_41 if level_of_assignment=="cluster" & outcome_type=="continuous"
			
			/*
			**-->> Recalculate effect sizes based on no adjustment as a check
			generate es_converted_41_check_cluster=.
			
			tablist level_of_assignment analytic_method outcome_type regression_coef sd_pooled mean_i_adj mean_c_adj mean_i_unadj mean_c_unadj es_official_40 es_converted_41 if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32)
			replace es_converted_41_check_cluster = ((omega*regression_coef)/sd_pooled) * sqrt( 1 - ( (2*(n_avg_cluster-1)*icc) / (n_t_indiv-2) ) ) if level_of_assignment=="cluster" & outcome_type=="continuous" &  analytic_method=="HLM/multilevel regression"
			replace es_converted_41_check_cluster = ((omega*(mean_i_adj-mean_c_adj))/sd_pooled) * sqrt( 1 - ( (2*(n_avg_cluster-1)*icc) / (n_t_indiv-2) ) ) if level_of_assignment=="cluster" & outcome_type=="continuous" &  analytic_method=="no adjustment" & !missing(mean_i_adj) & !missing(mean_c_adj)
			replace es_converted_41_check_cluster = ((omega*(mean_i_unadj-mean_c_unadj))/sd_pooled) * sqrt( 1 - ( (2*(n_avg_cluster-1)*icc) / (n_t_indiv-2) ) ) if level_of_assignment=="cluster" & outcome_type=="continuous" &  analytic_method=="no adjustment" & !missing(mean_i_unadj) & !missing(mean_c_unadj)
			replace es_converted_41_check_cluster = ((omega*(mean_i_adj-mean_c_unadj))/sd_pooled) * sqrt( 1 - ( (2*(n_avg_cluster-1)*icc) / (n_t_indiv-2) ) ) if level_of_assignment=="cluster" & outcome_type=="continuous" &  analytic_method=="no adjustment" & !missing(mean_i_adj) & !missing(mean_c_unadj)
			tablist level_of_assignment analytic_method outcome_type regression_coef mean_i_adj mean_c_adj mean_i_unadj mean_c_unadj es_official_40 es_converted_41 es_converted_41_check_cluster if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32)
			summarize es_official_40 es_converted_41 es_converted_41_check_cluster if level_of_assignment=="cluster" & outcome_type=="continuous" & !missing(es_converted_41_check_cluster)
			*/
			
		*-> Dichotmous Measures
			
			//Note: None	
			count if level_of_assignment=="cluster" & outcome_type=="dichotomous"	
			assert r(N)==0
	
	/* Finalize effect size conversions */
	tablist contrast_simple_number study_id contrast_id es_id level_of_assignment outcome_type analytic_method es_official_40 es_converted_41 if missing(es_converted_41), sort(v) ab(32)
		* contrast_simple_number==56; es_id== 201: "NP: Outcome does not appear to be included in MRG. No QCd but see note if we want to think about entering information for this outcome."
		* contrast_simple_number==57; es_id==205 - 208: "NP: MRG not in Dropbox folder"
		* contrast_simple_number==93; es_id==335 - 340: "NP: I don't see this MRG in the Dropbox folder."
		* contrast_simple_number d-0: Asking if these are additions by RTL b/c MRGs say not eligible/not reviewed but database inlcudes sample sizes, effect sizes, and standard errors for these.
	replace es_converted_41=es_official_40 if missing(es_converted_41)
	tablist contrast_simple_number study_id contrast_id es_id level_of_assignment analytic_method outcome_type es_official_40 es_converted_41 if missing(es_converted_41), sort(v) ab(32)
	replace es_converted_41=.m if missing(es_converted_41)
	tablist contrast_simple_number study_id contrast_id es_id level_of_assignment analytic_method outcome_type es_official_40 es_converted_41, sort(v) ab(32)
	summarize es_official_40 es_converted_41
	
	
*=========================================================================================
* III) CONVERT STANDARD ERRORS OF EFFECT SIZES FROM 4.0 to 4.1
*=========================================================================================     

	//Note: Effect size standard errors (SEs) were not calculated under 4.0. The SEs in this database were calculated using equation [E.1.4] of Procedures Handbook Version 4.1 (page E.3).

	/* Prepare data and variables for SE conversions */
	summarize se_e14_40, detail
	generate se_converted_41=.
	label variable se_converted_41 "standard error of effect size, 4.1"
	
	/* Individual-level Assignment */
	tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual", sort(v) ab(32)	
		
		*-> Continuous measures
		tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & outcome_type=="continuous", sort(v) ab(32)
		
			**-> ANCOVA/regression/tstat: decision tree
			count if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous"
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous", sort(v) ab(32)
			tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc model_Rsqrd t_stat if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous", sort(v)	ab(32)
				
				***-> Regression coefficient standard error reported: ([E.7.1] supp, Individual assignment column of Table 2 of Supplement)
				count if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & (!missing(regression_coef_se_uc) | !missing(regression_coef_se_cc) )
				tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & (!missing(regression_coef_se_uc) | !missing(regression_coef_se_cc) )
				replace se_converted_41= omega * sqrt( ((regression_coef_se_uc/sd_pooled)^2) + ((es_converted_41^2)/(2*n_t_indiv)) ) if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_uc)
				replace se_converted_41= omega * sqrt( ((regression_coef_se_cc/sd_pooled)^2) + ((es_converted_41^2)/(2*n_t_indiv)) ) if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc)
				tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc se_e14_40 se_converted_41 if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & (!missing(regression_coef_se_uc) | !missing(regression_coef_se_cc) )
				summarize se_e14_40 se_converted_41 if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression") & outcome_type=="continuous" & (!missing(regression_coef_se_uc) | !missing(regression_coef_se_cc) )
				
				***-> Regression coefficient standard error reported AND model R2 value is reported: [E.2.2] supp/v4.1, Individual assignment column of Table 3 of Supplement; same as [E.2.2] in v4.1 Procedures, page E-5
				// Note: None
				count if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc)
				assert r(N)==0
			
				***-> Cannot use either of the two above AND t-statistic reported: (SE=b/t)
				count if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc)
				tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc model_Rsqrd t_stat se_converted_41 se_e14_40 se_converted_41 if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc), sort(v) ab(32)
				replace se_converted_41 = (regression_coef / t_stat) if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc)
				tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc model_Rsqrd t_stat se_converted_41 se_e14_40 se_converted_41 if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc), sort(v) ab(32)
				tablist study_id contrast_id es_id level_of_assignment outcome_type analytic_method regression_coef model_Rsqrd t_stat se_converted_41 se_e14_40 se_converted_41 if se_converted_41>1 & !missing(se_converted_41) & level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc), sort(v) ab(32)
				//Note: The three SEs calculated with formula SE= b/t look way off. Use [E.1.4] instead.
				replace se_converted_41= se_e14_40 if inlist(es_id,51,52,53)
				tablist study_id contrast_id es_id level_of_assignment outcome_type analytic_method regression_coef model_Rsqrd t_stat se_converted_41 se_e14_40 se_converted_41 if inlist(es_id,51,52,53)
				tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc model_Rsqrd t_stat se_converted_41 se_e14_40 se_converted_41 if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc), sort(v) ab(32)
			
				***-> Cannot use either of the three above: no change/ [E.1.4] v4.1, page E-3, v4.1 procedures handbook
				replace se_converted_41= se_e14_40 if missing(se_converted_41) & level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous"
				
			tablist level_of_assignment outcome_type analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc model_Rsqrd t_stat se_converted_41 se_e14_40 if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous", sort(v)	ab(32) 
			summarize se_e14_40 se_converted_41 if level_of_assignment=="individual" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous"
			
			**-> Gain Scores
			// Note: None
			count if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="gain score"
			assert r(N)==0
			
			**-> Difference-in-differences
			// Note: None
			count if level_of_assignment=="individual" & outcome_type=="continuous" & analytic_method=="difference-in-differences" 
			assert r(N)==0
			
			**-> No adjustment (!) ANOVA F-test
			// Note: Use [E.1.4], page E-3 of v4.1 Procedures); Calculation is same as under 4.0.
			count if level_of_assignment=="individual" & inlist(analytic_method, "no adjustment","ANOVA F-test") & outcome_type=="continuous"
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & inlist(analytic_method, "no adjustment","ANOVA F-test") & outcome_type=="continuous", sort(v) ab(32)
			replace se_converted_41= se_e14_40 if level_of_assignment=="individual" & inlist(analytic_method, "no adjustment","ANOVA F-test") & outcome_type=="continuous"
			tablist level_of_assignment outcome_type analytic_method se_converted_41 se_e14_40 if level_of_assignment=="individual" & inlist(analytic_method, "no adjustment","ANOVA F-test") & outcome_type=="continuous", sort(v) ab(32)
			summarize se_converted_41 se_e14_40 if level_of_assignment=="individual" & inlist(analytic_method, "no adjustment","ANOVA F-test") & outcome_type=="continuous"

		*-> Dichotomous measures
		tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="individual" & outcome_type=="dichotomous", sort(v) ab(32)	
		
			**-> Indiv/DnD/dichotomous: ([E.4.4], Individual assignment column of Table 3 of Supplement)
			//Note: None
			count if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method=="difference-in-differences"
			assert r(N)==0
			
			**-> Indiv/AnyOtherAnalysis/dichotomous: ([E.4.4], Individual assignment column of Table 3 of Supplement)
			count if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences"
			tablist study_id contrast_id es_id level_of_assignment outcome_type analytic_method n_i_indiv n_c_indiv mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj se_converted_41 if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences", sort(v) ab(32)	
			replace se_converted_41= (1/1.65) * ( sqrt(             (1/(mean_i_unadj*n_i_indiv))   +   (1/((1-mean_i_unadj)*n_i_indiv))   +   (1/(mean_c_unadj*n_c_indiv))   +   (1/((1-mean_c_unadj)*n_c_indiv))               ) ) if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences"
			tablist study_id contrast_id es_id level_of_assignment outcome_type analytic_method n_i_indiv n_c_indiv mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj se_e14_40 se_converted_41 if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences", sort(v) ab(32)	
			summarize se_e14_40 se_converted_41 if level_of_assignment=="individual" & outcome_type=="dichotomous" & analytic_method!="difference-in-differences"
			
	/* Cluster-level Assignment */
	//Note:  All cluster-level assignment studies report student-level effect sizes (i.e., there are no cluster-level effect sizes reported in the cNMA database).
	tablist level_of_assignment analytic_method outcome_type if level_of_assignment=="cluster", sort(v) ab(32)
	
		*-> Continuous measures
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="cluster" & outcome_type=="continuous", sort(v) ab(32)
	
			**-> ANCOVA/regression/tstat: decision tree
			count if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous"
			tablist level_of_assignment outcome_type analytic_method if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous", sort(v)	ab(32)
			tablist study_id contrast_id es_id level_of_assignment outcome_type analytic_method regression_coef study_clustered regression_coef_se_uc regression_coef_se_cc model_Rsqrd t_stat if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous", sort(v)	ab(32)
				
				***-> Regression coefficient standard error reported, study corrected for clustering: [E.7.0] supp, Cluster assignment column of Table 2 of Supplement
				assert study_clustered==1 if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc)
				count if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc)
				tablist level_of_assignment outcome_type analytic_method regression_coef study_clustered regression_coef_se_cc n_avg_cluster n_t_indiv icc if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc), sort(v) ab(32)
				// 𝛾= 1 − (2(𝑛−1)icc)/(N-2))
				generate y= 1 - (  (2*(n_avg_cluster-1)*icc) / (n_t_indiv-2)  ) if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc)
				// ℎ =   [(𝑁−2)−2(𝑛−1)icc]^2    /   (𝑁−2)(1−icc)^2 + 𝑛(𝑁−2𝑛)icc^2 +2(𝑁−2𝑛)𝜌(1−icc)
				generate h= (   ( (n_t_indiv-2)-(2*(n_avg_cluster-1)*icc) )^2   )  /  (   ( (n_t_indiv-2)*((1-icc)^2) )  +  ( n_avg_cluster*(n_t_indiv-(2*n_avg_cluster))*(icc^2) )  +  ( 2*(n_t_indiv-(2*n_avg_cluster))*(1-icc) )   ) ///
							if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc)
				
				tablist level_of_assignment outcome_type analytic_method regression_coef study_clustered regression_coef_se_cc n_avg_cluster n_t_indiv icc y h se_converted_41 if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc), sort(v) ab(32)
				replace se_converted_41= omega * sqrt( ((regression_coef_se_cc/sd_pooled)^2)*y + ((es_converted_41^2)/(2*h)) ) if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc)
				tablist level_of_assignment outcome_type analytic_method regression_coef study_clustered regression_coef_se_cc n_avg_cluster n_t_indiv icc y h se_e14_40 se_converted_41 if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc), sort(v) ab(32)
				summarize se_e14_40 se_converted_41 if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_cc) &!missing(se_converted_41)
				
				***-> Regression coefficient standard error reported, study did not for clustering: [E.7.1] supp, Cluster assignment column of Table 2 of Supplement
				assert study_clustered==0 if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_uc)
				count if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(regression_coef_se_uc)
				assert r(N)==0
				// Note: None
				
				***-> Regression coefficient standard error not reported AND model R2 value is reported: [E.2.2] supp/v4.1, Cluster assignment column of Table 3 of Supplement; same as [E.2.2] in v4.1 Procedures, page E-5
				// Note: None
				count if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc)
				assert r(N)==0
			
				***-> Cannot use either of the three above AND the study cluster corrected AND t-statistic reported: (SE=b/t)
				// Note: None
				count if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous" & !missing(t_stat) & missing(model_Rsqrd) & missing(regression_coef_se_uc) & missing(regression_coef_se_cc)
				assert r(N)==0
				
				***-> Indiv/ANCOVAregression/continuous; cannot use either of the three above: use [E.5.2] corrected formula supp, Table 4 of Supplement
				replace se_converted_41= omega * sqrt(    ///
													( (n_i_indiv+n_c_indiv)/(n_i_indiv*n_c_indiv) ) * (1 + (n_avg_cluster-1)*icc) + ///
													(es_official_40^2)* (         ///
														(    (n_t_indiv-2)*((1-icc)^2) + (n_avg_cluster*(n_t_indiv- (2*n_avg_cluster))*(icc^2)) + (2*(n_t_indiv-(2*n_avg_cluster))*icc*(1-icc))                   ) / /// numerator
														(2* ( ( (n_t_indiv-2)- ( 2*(n_avg_cluster-1)*icc )  )^2 )  ) /// denominator
														)       ///
									         ) if missing(se_converted_41) & level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous"
				
				//Note: n_avg_cluster is missing for the last observation within the ANCOVA/regression/tstat decision tree for which a 4.1 standard error has yet to be calculated with the available data so even [E.5.2] cannot be used. Resort to [E.1.4] as already calculated in se_e14_40.
				tablist study_id contrast_id es_id n_i_indiv n_c_indiv n_avg_cluster if missing(se_converted_41) & level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous", sort(v) ab(32)
				replace se_converted_41=se_e14_40 if missing(se_converted_41) & level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous"
				
			tablist study_id contrast_id es_id level_of_assignment outcome_type analytic_method se_e14_40 se_converted_41 if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous", sort(v)	ab(32) 
			assert !missing(se_converted_41) if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous"
			summarize se_e14_40 se_converted_41 if level_of_assignment=="cluster" & inlist(analytic_method,"ANCOVA","OLS regression","HLM/multilevel regression","t-stat") & outcome_type=="continuous"
			
			**-> No adjustment
			tablist study_id contrast_id es_id level_of_assignment outcome_type analytic_method if level_of_assignment=="cluster" & analytic_method=="no adjustment", sort(v) ab(32)
			tablist study_id contrast_id es_id level_of_assignment outcome_type analytic_method n_i_indiv n_c_indiv n_avg_cluster icc se_e14_40 se_converted_41 if level_of_assignment=="cluster" & analytic_method=="no adjustment", sort(v) ab(32) 
			replace se_converted_41= omega * sqrt(    ///
													( (n_i_indiv+n_c_indiv)/(n_i_indiv*n_c_indiv) ) * (1 + (n_avg_cluster-1)*icc) + ///
													(es_official_40^2)* (         ///
														(    (n_t_indiv-2)*((1-icc)^2) + (n_avg_cluster*(n_t_indiv- (2*n_avg_cluster))*(icc^2)) + (2*(n_t_indiv-(2*n_avg_cluster))*icc*(1-icc))                   ) / /// numerator
														(2* ( ( (n_t_indiv-2)- ( 2*(n_avg_cluster-1)*icc )  )^2 )  ) /// denominator
														)       ///
									         )  if level_of_assignment=="cluster" & analytic_method=="no adjustment" & outcome_type=="continuous" 	
			tablist level_of_assignment outcome_type analytic_method n_i_indiv n_c_indiv n_avg_cluster icc se_e14_40 se_converted_41 if level_of_assignment=="cluster" & analytic_method=="no adjustment", sort(v) ab(32)
			summarize se_e14_40 se_converted_41 if level_of_assignment=="cluster" & analytic_method=="no adjustment"
				
		*-> Dichotmous Measures
			
			//Note: None	
			count if level_of_assignment=="cluster" & outcome_type=="dichotomous"	
			assert r(N)==0
		
	/* Finalize standard error conversions */
	tablist contrast_simple_number study_id contrast_id es_id level_of_assignment outcome_type analytic_method se_e14_40 se_converted_41 if missing(se_converted_41), sort(v) ab(32)
		* contrast_simple_number==56; es_id== 201: "NP: Outcome does not appear to be included in MRG. No QCd but see note if we want to think about entering information for this outcome."
		* contrast_simple_number==57; es_id==205 - 208: "NP: MRG not in Dropbox folder"
		* contrast_simple_number==93; es_id==335 - 340: "NP: I don't see this MRG in the Dropbox folder."
		* contrast_simple_number d-0: Asking if these are additions by RTL b/c MRGs say not eligible/not reviewed but database inlcudes sample sizes, effect sizes, and standard errors for these.
	replace se_converted_41=se_e14_40 if missing(se_converted_41)
	tablist contrast_simple_number study_id contrast_id es_id level_of_assignment outcome_type analytic_method se_e14_40 se_converted_41 if missing(se_converted_41), sort(v) ab(32)
	replace se_converted_41=.m if missing(se_converted_41)
	tablist contrast_simple_number study_id contrast_id es_id level_of_assignment outcome_type analytic_method se_e14_40 se_converted_41, sort(v) ab(32)
	summarize se_e14_40 se_converted_41	
		
	/* Save final ES/SE conversion database */
	capture drop *_check_*
	drop h
	quietly compress
	sort contrast_simple_number
	save "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 COMPLETED.dta", replace 
	
	
*=========================================================================================
* IV) DESCRIPTIVE STATISTICS
*=========================================================================================     
	
	/* Effect sizes */
	summarize es_official_40 es_converted_41
	tablist study_id contrast_id contrast_simple_number es_id if missing(es_converted_41), sort(v) ab(32)
	table (level_of_assignment outcome_type analytic_method), statistic(mean es_official_40 es_converted_41) statistic(n es_official_40 es_converted_41) nototals
	
	/* Standard errors */
	summarize se_e14_40 se_converted_41	
	tablist study_id contrast_id contrast_simple_number es_id if missing(es_converted_41), sort(v) ab(32)
	table (level_of_assignment outcome_type analytic_method), statistic(mean se_e14_40 se_converted_41) statistic(n se_e14_40 se_converted_41)  nototals	
	

log close convert_es_se
