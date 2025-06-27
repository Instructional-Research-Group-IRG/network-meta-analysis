global root "C:\Users\sethb\Documents\Career\freelance\IRG\assignments\network meta-analysis\network-meta-analysis\cNMA"

import delimited "$root\es_se_41_data_merge.csv", varnames(1) bindquote(strict)  clear 

	count
	describe
	isid study_id contrast_id es_id 
	
	rename (outcome outcome_type es_converted_41 se_converted_41) (measure_name measure_type effect_size_converted standard_error_converted)
	foreach var of varlist contrast- analytic_method {
		rename `var' `var'_converted
	}
	foreach var of varlist _all {
		capture confirm string variable `var'
		if _rc==0 replace `var'= stritrim(strtrim(`var'))
	}
	replace contrast_id= strlower(contrast_id)

	tempfile es_se_41_data_merge
	save `es_se_41_data_merge'
	
import delimited "$root\NMA_data_analysis_subset_grpID_merge.csv", varnames(1) bindquote(strict)  clear 
	
	count
	describe
	isid study_id contrast_id es_id 
	
	rename (contrast_name measure_name domain effect_size standard_error) (contrast_nnma measure_name_nnma domain_nnma effect_size_nnma standard_error_nnma)
	foreach var of varlist _all {
		capture confirm string variable `var'
		if _rc==0 replace `var'= stritrim(strtrim(`var'))
	}
	replace contrast_id= strlower(contrast_id)

	tempfile nnma_merge
	save `nnma_merge'	

import delimited "$root\cNMA_data_4.1_merge.csv", varnames(1) bindquote(strict) clear
	
	count
	describe
	tabulate es_id, missing

	duplicates report study_id contrast_id es_id 
	duplicates tag study_id contrast_id es_id, generate(dups)
	list if dups>0
	drop if study_id=="NA"
	isid study_id contrast_id es_id 
	destring es_id, replace
	drop dups 
	foreach var of varlist _all {
		capture confirm string variable `var'
		if _rc==0 replace `var'= stritrim(strtrim(`var'))
	}
	rename (contrast_name) (contrast)
	foreach var of varlist contrast- standard_error {
		rename `var' `var'_cnmadatabase
	}
	
	tempfile cnma_merge
	save `cnma_merge'

merge 1:1 study_id contrast_id es_id using `es_se_41_data_merge', generate(cnma_esse_merge)
	tabulate cnma_esse_merge
	
merge 1:1 study_id contrast_id es_id using `nnma_merge', generate(cnma_esse_nnma_merge)
	tabulate cnma_esse_nnma_merge
	tablist cnma_esse_merge cnma_esse_nnma_merge, sort(v)
	sort study_id contrast_id es_id cnma_esse_merge cnma_esse_nnma_merge
	
	*drop simple_number level_of_assignment_converted analytic_method_converted
	*gsort study_id contrast_id -cnma_esse_merge es_id /*effect_size_cnmadatabase effect_size_converted*/
	*order study_id contrast_id es_id contrast_name contrast_esse domain domain_esse measure_name outcome_esse effect_size es_converted_41 standard_error se_converted_41 cnma_merge
	*browse
	*browse if inlist(study_id,"MI20409","MI20284","MI20406")
	foreach id in MI20409 MI20284 MI20406 MI20755 MI20759 {
		export excel using "$root\cnma_merge_errors.xlsx" if inlist(study_id,"`id'"), firstrow(variables) sheet("`id'", replace) 
	}
	tempfile cnma_esse_merge
	save `cnma_esse_merge'
	
use `es_se_41_data_merge', replace
merge 1:1 study_id contrast_id es_id using `nnma_merge', generate(cnma_esse_nnma_merge)
	tabulate cnma_esse_nnma_merge
	
use `nnma_merge', replace	
merge 1:1 study_id contrast_id es_id using `es_se_41_data_merge', generate(cnma_esse_nnma_merge2)
	tabulate cnma_esse_nnma_merge2

use `cnma_merge', replace
merge 1:1 study_id contrast_id es_id using `nnma_merge', generate(cnma_esse_nnma_merge)
	tabulate cnma_esse_nnma_merge	