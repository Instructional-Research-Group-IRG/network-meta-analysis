
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
*** Last date modified: April 2, 2025												          ***
***																							  ***
*** Notes:																					  ***
***																							  ***
***																						      ***
*************************************************************************************************

clear all
version 18.5
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
	import excel "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 v5.1_CLEAN.xlsx", sheet("Data inventory for 4.0 to 4.1") firstrow clear
	save "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 v5.1_CLEAN.dta", replace

	/* Clean up variable names */
	include "$root\convert_es_se_40_to_41_revise_variable_names.do"
	
	/* Drop uneccesary columns */
	drop mrg_link StudyCitationfull_citation
	
	/*Keep only valid rows */
	count
	drop if missing(contrast_simple_number)
	count
	
	/* Save cNMA database as .dta */
	save "$root\Data inventory for 4.0 to 4.dta", replace
	
	/* Explore key variables */
	tab1 level_of_assignment analytic_method outcome_type, missing
	tablist level_of_assignment analytic_method outcome_type, sort(v) ab(32)
	tablist contrast_id study_id if , sort(v) ab(32)
	
	/* Clean up string variables */
	foreach var of varlist _all {
		capture confirm string variable `var'
		if _rc==0 {
			display as input _n "String variable: `var'"
			replace `var'= strtrim(stritrim(`var'))
		}
	}
	
	/* Clean up numeric variables */
	foreach var of varlist mean_?_*adj mean_?_sd {
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
			assert missing(Smallsamplebiascorrectionω)
			drop Smallsamplebiascorrectionω
			
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
			generate OR= (mean_i_adj* (1-mean_c_adj))  /  (mean_c_adj* (1-mean_i_adj)) if outcome_type=="dichotomous"
			replace log_odds_post= ln(OR) if outcome_type=="dichotomous"  
			tablist level_of_assignment analytic_method outcome_type mean_?_unadj mean_?_adj OR log_odds_post if outcome_type=="dichotomous", sort(v) ab(32)
			
  
*=========================================================================================
* II) CONVERT EFFECT SIZES FROM 4.0 to 4.1
*=========================================================================================  

	/* Prepare data and variables for ES conversions */
	summarize es_official_40, detail
	generate es_converted_41=.
	generate es_converted_41_check=.
	label variable es_converted_41 "effect size converted from 4.0 to 4.1"

	/* Effect sizes that are calculated the same under 4.0 and 4.1 */
	generate es_nochange=0
	
		*-> Individual-level assignment, analytic method other than difference-in-differences, continuous outcomes
			// Note: The effect size formula for continuous outcome measures from studies using individual-level assignment and an analytic method other than difference-in-differences remains unchanged from 4.0 to 4.1.
			//       Row 3 of <<Guide to 4.0 to 4.1 ES SE conversions.xldx>>
			replace es_nochange=1 if level_of_assignment=="individual" & analytic_method!="difference-in-differences" & !missing(analytic_method) & outcome_type=="continuous"
			tablist es_nochange level_of_assignment analytic_method outcome_type, sort(v) ab(32)
			replace es_converted_41=es_official_40 if es_nochange==1
			bysort es_nochange: summarize es_converted_41
			tablist es_nochange level_of_assignment analytic_method outcome_type es_official_40 es_converted_41 if es_nochange==1, sort(v) ab(32)
			
			**-->> Recalculate effect sizes based on no adjustment as a check
			tablist es_nochange level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj S_pooled_sd es_official_40 es_converted_41 if es_nochange==1 & analytic_method=="no adjustment", sort(v) ab(32)
			replace es_converted_41_check= (omega * (mean_i_unadj - mean_c_unadj)) / S_pooled_sd if es_nochange==1 & analytic_method=="no adjustment"
			tablist es_nochange level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj S_pooled_sd es_official_40 es_converted_41 es_converted_41_check if es_nochange==1 & analytic_method=="no adjustment" & !missing(es_converted_41_check), sort(v) ab(32)
			summarize es_converted_41 es_converted_41_check if es_nochange==1 & analytic_method=="no adjustment" & !missing(es_converted_41_check)
			summarize es_converted_41 es_converted_41_check if es_nochange==1 & analytic_method=="no adjustment" & !missing(es_converted_41_check) // (!) Negative ESs?
			
		*-> Additional adjustment if officially reported effect size calculated with unadjusted means but adjusted means are available
			// Note: (!) Ask team about this. (!) Also about negative effect sizes if positive result is decrease in outcome.
			tablist es_nochange level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj mean_i_sd mean_c_sd es_official_40 es_converted_41 if level_of_assignment=="individual" & analytic_method=="no adjustment" & outcome_type=="continuous" & !missing(mean_i_sd) & !missing(mean_c_sd), sort(v) ab(32)
			replace es_nochange=2 if level_of_assignment=="individual" & analytic_method=="no adjustment" & outcome_type=="continuous" & !missing(mean_i_sd) & !missing(mean_c_sd)
			replace es_converted_41= (omega * (mean_i_adj - mean_c_adj)) / S_pooled_sd if es_nochange==2
			tablist es_nochange level_of_assignment analytic_method outcome_type mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj mean_i_sd mean_c_sd es_official_40 es_converted_41 if es_nochange==2, sort(v) ab(32)
			
	/* Effect sizes that are calculated differently under 4.0 and 4.1 */
	
		*-> Individual-level assignment, analytic method difference-in-differences, continuous outcomes
			count if level_of_assignment=="individual" & analytic_method=="difference-in-differences" & !missing(analytic_method) & outcome_type=="continuous"
			assert r(N)==0
			*tablist level_of_assignment analytic_method outcome_type pre_post_same_measure pre_post_correlation es_official_40 es_converted_41 if level_of_assignment=="individual" & analytic_method=="difference-in-differences" & !missing(analytic_method) & outcome_type=="continuous", sort(v) ab(32)
	
		*-> Individual-level assignment, analytic method other than difference-in-differences, dichotomous outcomes
			tablist level_of_assignment analytic_method outcome_type mean_?_unadj mean_?_adj log_odds_post es_official_40 es_converted_41 if level_of_assignment=="individual" & analytic_method!="difference-in-differences" & !missing(analytic_method) & outcome_type=="dichotomous", sort(v) ab(32)
			replace es_converted_41= log_odds_post/1.65
			tablist level_of_assignment analytic_method outcome_type mean_?_unadj mean_?_adj log_odds_post OR es_official_40 es_converted_41 if level_of_assignment=="individual" & analytic_method!="difference-in-differences" & !missing(analytic_method) & outcome_type=="dichotomous", sort(v) ab(32)
			
		*-> Individual-level assignment, analytic method difference-in-differences, dichotomous outcomes
			count if level_of_assignment=="individual" & analytic_method=="difference-in-differences" & !missing(analytic_method) & outcome_type=="dichotomous"
			assert r(N)==0
			*tablist level_of_assignment analytic_method outcome_type pre_post_same_measure pre_post_correlation es_official_40 es_converted_41 if level_of_assignment=="individual" & analytic_method=="difference-in-differences" & !missing(analytic_method) & outcome_type=="dichotomous", sort(v) ab(32)

/*		
		*-> Cluster-level assignment
			// Note: Revise the student-level ES from studies using cluster-level assignment using the unbiased formula in Table 1 of the Supplement, 
			//	     which imposes a bias correction, in place of [E.5.1] on page E-9 of v4.1 Procedures/biased formula in Table 1 of the Supplement.
			//       (?) All cluster-level assignment studies report student-level effect sizes (i.e., there are no cluster-level effect sizes reported in the cNMA database).
			
			*Put estimated impacts across cluster studies into single variable for use as b in the unbiased [E.5.1] formula of Table 1 of the Supplement
			generate b_cluster=. // This is the estimated difference between the intervention and comparison groups the source of which will differe depending on the analytic method used and data available.
			tablist analytic_method if level_of_assignment=="cluster", sort(v) ab(32)
			
				**HLM & OLS: use regression coefficient if reported
				tablist level_of_assignment analytic_method regression_coef regression_coef_se_uc regression_coef_se_cc model_Rsqrd if level_of_assignment=="cluster" & inlist(analytic_method,"HLM/multilevel regression","OLS regression"), sort(v) ab(32)
				replace b_cluster= regression_coef if level_of_assignment=="cluster" & inlist(analytic_method,"HLM/multilevel regression","OLS regression")
				tablist analytic_method b_cluster regression_coef /*study_need_to_cluster*/ regression_coef_se_uc regression_coef_se_cc model_Rsqrd if level_of_assignment=="cluster" & inlist(analytic_method,"HLM/multilevel regression","OLS regression"), sort(v) ab(32)
				tablist study_id contrast_id regression_coef regression_coef_se_uc regression_coef_se_cc mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj mean_i_sd mean_c_sd if missing(b_cluster) & level_of_assignment=="cluster" & inlist(analytic_method,"HLM/multilevel regression","OLS regression"), sort(v) ab(32) 
			
				**No adjustment
				tablist level_of_assignment analytic_method b_cluster regression_coef mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj mean_i_sd mean_c_sd if level_of_assignment=="cluster" & analytic_method=="no adjustment", sort(v) ab(32)
				replace b_cluster=  mean_i_adj-mean_c_adj if !missing(mean_i_adj) & !missing(mean_c_adj) & level_of_assignment=="cluster" & analytic_method=="no adjustment"
				replace b_cluster=  mean_i_unadj-mean_c_unadj if !missing(mean_i_unadj) & !missing(mean_c_unadj) & missing(b_cluster) & level_of_assignment=="cluster" & analytic_method=="no adjustment"
				tablist level_of_assignment analytic_method b_cluster regression_coef mean_i_unadj mean_c_unadj mean_i_adj mean_c_adj mean_i_sd mean_c_sd if level_of_assignment=="cluster" & analytic_method=="no adjustment", sort(v) ab(32)

				replace icc="." if inlist(icc,"Not in article","Not in article but see comment","Not presented in report") 
				destring icc, replace
				replace icc= 0.2 if icc==. & level_of_assignment=="cluster" // (!) Placeholder. Needs to be updated by team. "As defaults, the WWC uses the ICC values of .20 for achievement outcomes and .10 for all other outcomes, but will use study-reported ICC values when available. (Procedures handbook 4.1, p. 20)"
			
			*Calculate 4.1 effect size for outcome measures of cluster-level assignment studies: unbiased [E.5.1] formula of Table 1 of the Supplement
			replace es_converted_41 = ((omega*b_cluster)/S_pooled_sd) * sqrt( 1 - ( (2*(n_avg_cluster-1)*icc) / n_t_indiv-2) ) if level_of_assignment=="cluster"
			tablist analytic_method es_official_40 es_converted_41 omega b_cluster icc S_pooled_sd n_avg_cluster n_t_indiv if level_of_assignment=="cluster", sort(v) ab(32)
*/			
	
*=========================================================================================
* III) CONVERT STANDARD ERRORS OF EFFECT SIZES FROM 4.0 to 4.1
*=========================================================================================     
	
	//Note: Effect size standard errors (SEs) were not calculated under 4.0. The SEs in this database were calculated using equation [E.1.4] of Procedures Handbook Version 4.1 (page E.3).

	/* Prepare data and variables for SE conversions */
	summarize se_e14_40, detail
	generate se_converted_41=.
	label variable se_converted_41 "standard error of effect size converted from 4.0 to 4.1"
	
	/* Effect sizes that are calculated the same under 4.0 and 4.1 */	
	generate se_nochange=0
	
		*-> Individual-level assignment, analytic method "no adjustment"", continuous outcomes
		replace se_nochange=1 if level_of_assignment=="individual" & analytic_method=="no adjustment" & outcome_type=="continuous"
		tablist level_of_assignment analytic_method outcome_type se_nochange, sort(v)
		replace se_converted_41=se_e14_40 if se_nochange==1
		bysort se_nochange: summarize es_converted_41
	
	/* Standard errors that are calculated the differently under 4.0 and 4.1 */
	
		*-> Indiv/ANCOVAregression/continuous; regression coefficient standard error reported: ([E.7.1] supp, Individual assignment column of Table 2 of Supplement)
		
		*-> Indiv/ANCOVAregression/continuous; model R2 value is reported: ([E.2.2] supp/v4.1, Individual assignment column of Table 3 of Supplement; same as [E.2.2] in v4.1 Procedures, page E-5)
		
		*-> Indiv/ANCOVAregression/continuous; cannot use either of the two above AND/OR only t-statistic reported: (SE=b/t)
		
		*-> Indiv/ANCOVAregression/continuous; cannot use either of the three above: (no change/ [E.1.4] v4.1, page E-3, v4.1 procedures handbook)
		
		*-> Indiv/GainScore/continuous: ([E.3.1] supp	Individual assignment column of Table 3 of Supplement)
		
		


• use [E.3.1] (Individual assignment column of Table 3 of Supplement)"	[E.3.1] supp	Individual assignment column of Table 3 of Supplement

 		
	/* Save final ES/SE conversion database */
**# Bookmark #2
	drop *_check OR
	quietly compress
	sort contrast_simple_number
	save "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 v5.1_COMPLETED.dta", replace 
	
	
*=========================================================================================
* IV) DESCRIPTIVE STATISTICS
*=========================================================================================     
	
	/* */


log close convert_es_se
