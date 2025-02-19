
capture log close _all
log using "C:\Users\sethb\Documents\Career\freelance\IRG\assignments\network meta-analysis\network-meta-analysis\cNMA\convert_es_se_40_to_41.log", replace name(convert_es_se)


*************************************************************************************************
***                                                                                           ***
*** Program name: convert_es_se_40_to_41.do                                                   ***
*** Project: Component Network Analysis                               				          ***
*** Purpose: Clean TBF Market Garden 2024 data                                                ***    
***																	 				          ***
*** Contents:                                                       				          ***
***    0) SET UP CODE                              				                              ***
***    I) LOAD RAW DATA                                                                       ***
***    II) MANAGE VARIABLES                                                                   ***
***    III) CLEAN DATA                                                                        ***
***    IV) DESCRIPTIVE STATISTICS                                                             ***
***                                                                                           ***
*** Authors: Seth B. Morgan                                 				                  ***
*** Start date: February 19, 2025 	   					 	     			                  ***
*** Last date modified: February 19, 2025                                                    ***
***                                                                                           ***
*** Notes:                                                                                    ***
***                                                                                           ***
***                                                                                           ***
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
	tab1 level_of_assignment analysis_method, missing
	tablist level_of_assignment analysis_method, sort(v)
	
  
*=========================================================================================
* II) CONVERT EFFECT SIZES FROM 4.0 to 4.1
*=========================================================================================  

	/* */

	/* */

	/* */
	
    
*=========================================================================================
* III) CONVERT STANDARD ERRORS FROM 4.0 to 4.1
*=========================================================================================     

	/* */
		
 		
	 
*=========================================================================================
* IV) DESCRIPTIVE STATISTICS
*=========================================================================================     
	


log close convert_es_se
