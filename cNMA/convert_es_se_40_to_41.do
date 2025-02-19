
capture log close _all
log using "C:\Users\sethb\Documents\Career\freelance\IRG\assignments\network meta-analysis\network-meta-analysis\cNMA\convert_es_se_40_to_41.log", replace name(convert_es_se)


*************************************************************************************************
***                                                                                           ***
*** Program name: convert_es_se_40_to_41.do                                                   ***
*** Project: Component Network Analysis														  ***
*** Purpose: Clean TBF Market Garden 2024 data                                                ***    
***																							  ***
*** Contents:																				  ***
***    0) SET UP CODE																		  ***
***    I) LOAD CNMA DATABASE																  ***
***    II) CONVERT EFFECT SIZES FROM 4.0 to 4.1												  ***
***    III) CONVERT STANDARD ERRORS OF EFFECT SIZES FROM 4.0 to 4.1     					  ***
***																							  ***
*** Authors: Seth B. Morgan																      ***
*** Start date: February 19, 2025															  ***
*** Last date modified: February 19, 2025												      ***
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
* I) LOAD CNMA DATABASE
*=========================================================================================

	/* Load in cNMA database */
	import excel "$root\DATABASE Converting ESs and SEs from 4.0 to 4.1 v5_Data inventory for 4.0 to 4.xlsx", sheet("DATABASE Converting ESs and SEs") firstrow clear

	/* Clean up variable names */
	include "$root\convert_es_se_40_to_41_revise_variable_names.do"
	
	/*Keep only valid rows */
	count
	drop if missing(contrast_simple_number)
	count
	
	/* Drop uneccesary columns */
	drop mrg_link
	
	/* Save cNMA database as .dta */
	save "$root\Data inventory for 4.0 to 4.dta", replace
	
	/* Explore key variables */
	tab1 level_of_assignment analytic_method outcome_type, missing
	tablist level_of_assignment analytic_method outcome_type, sort(v)
	
	/* Calculate necessary variables from study-reported data */
			
		*-> Total sample of individuals
			assert !missing(n_t_indiv) 
			assert n_i_indiv + n_c_indiv == n_t_indiv
			tablist n_i_indiv n_c_indiv n_t_indiv, sort(v)

		*-> Total sample of clusters
			assert missing(n_t_cluster) if level_of_assignment!="cluster"
			tablist n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster", sort(v)
			replace n_t_cluster= n_i_cluster + n_c_cluster
			tablist n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster", sort(v)
			tablist contrast_id n_i_cluster n_c_cluster n_t_cluster if level_of_assignment=="cluster" & missing(n_t_cluster), sort(v) // (Send to team for correction in database- contrast IDs: 87196, 90508)
			
		*-> Average number of individuals per cluster
			replace n_avg_cluster= n_t_indiv/n_t_cluster
			tablist level_of_assignment n_i_indiv n_c_indiv n_t_indiv n_i_cluster n_c_cluster n_t_cluster n_avg_cluster n_avg_cluster, sort(v) ab(32)
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
			generate S_pooled_sd = sqrt( ( ( (n_i_indiv-1)*mean_i_sd^2) + ( (n_c_indiv-1)*mean_c_sd^2) ) / (n_i_indiv + n_c_indiv - 2) )
			label variable S_pooled_sd "pooled standard deviation"
			tablist S_pooled_sd n_i_indiv n_c_indiv mean_i_sd mean_c_sd, sort(v) ab(32)
		
  
*=========================================================================================
* II) CONVERT EFFECT SIZES FROM 4.0 to 4.1
*=========================================================================================  

	/* Prepare data and variables for ES conversions */
	summarize es_official_40, detail
	generate es_converted_41=.
	label variable es_converted_41 "effect size converted from 4.0 to 4.1"

	/* Effect sizes that are calculated the same under 4.0 and 4.1 */
	generate es_nochange=0
	
		*-> Individual-level assignment, analytic method other than difference-in-differences, continuous outcomes
			// Note: The effect size formula for continuous outcome measures from studies using individual-level assignment and an analytic method other than difference-in-differences remains unchanged from 4.0 to 4.1.
			//       Row 3 of <<Guide to 4.0 to 4.1 ES SE conversions.xldx>>
			replace es_nochange=1 if level_of_assignment=="individual" & analytic_method!="difference-in-differences" & outcome_type=="continuous"
			tablist level_of_assignment analytic_method outcome_type es_nochange, sort(v)
			replace es_converted_41=es_official_40 if es_nochange==1
			bysort es_nochange: summarize es_converted_41
		
	/* Effect sizes that are calculated differently under 4.0 and 4.1 */
	
		*-> Individual-level assignment, analytic method other than difference-in-differences, dichotomous outcomes
		
		*-> Individual-level assignment, analytic method difference-in-differences, continuous outcomes
		
		*-> Cluster-level assignment
			// Note: Revise the student-level ES from studies using cluster-level assignment using the unbiased formula in Table 1 of the Supplement, 
			//	     which imposes a bias correction, in place of [E.5.1] on page E-9 of v4.1 Procedures/biased formula in Table 1 of the Supplement.
			//       (?) All cluster-level assignment studies report student-level effect sizes (i.e., there are no cluster-level effect sizes reported in the cNMA database).
			generate b=1 // (!) Placeholder. Needs to updated with the estimate of the unstandardized difference between intervention and comparison group means, the column for which depends on the analyic method used.
			generate icc=1 // (!) Placeholder. Needs to updated by team.
			replace es_converted_41 = ((omega*b)/S_pooled_sd) * sqrt( 1 - ( (2*(n_avg_cluster-1)*icc) / n_t_indiv-2) ) if level_of_assignment=="cluster"
			tablist es_official_40 es_converted_41 omega b icc S_pooled_sd n_avg_cluster n_t_indiv if level_of_assignment=="cluster", sort(v) ab(32)
    
	
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
 		
	 
*=========================================================================================
* IV) DESCRIPTIVE STATISTICS
*=========================================================================================     
	


log close convert_es_se
