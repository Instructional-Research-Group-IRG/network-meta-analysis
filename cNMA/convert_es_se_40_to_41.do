
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
***    I) LOAD RAW DATA																		  ***
***    II) MANAGE VARIABLES																	  ***
***    III) CLEAN DATA																		  ***
***    IV) DESCRIPTIVE STATISTICS														      ***
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
	include "$root\convert_es_se_40_to_41_cleanup_variable_names.do"
	
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
		replace es_nochange=1 if level_of_assignment=="individual" & analytic_method!="difference-in-differences" & outcome_type=="continuous"
		tablist level_of_assignment analytic_method outcome_type es_nochange, sort(v)
		replace es_converted_41=es_official_40 if es_nochange==1
		bysort es_nochange: summarize es_converted_41
		
	/* Effect sizes that are calculated the differently under 4.0 and 4.1 */
	
		*-> Individual-level assignment, analytic method other than difference-in-differences, dichotomous outcomes
		
		*-> Individual-level assignment, analytic method difference-in-differences, continuous outcomes
		
		*-> Cluster-level assignment
    
	
*=========================================================================================
* III) CONVERT STANDARD ERRORS FROM 4.0 to 4.1
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
