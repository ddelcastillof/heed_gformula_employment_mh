* heed_test01v4-gform_dk.do

use "T:\projects\HEED\DataAnalysis\heed_test_gform.dta", clear
keep sf12mcs_dv hiqual_dv  econ_emp_bin Lecon_emp_bin ghqcase4 Lghqcase4 econ_dist pidp wave
expand 2 if wave==10 // duplicate final wave to have end of follow up outcome
sort pidp wave
bysort pidp wave: replace wave=11 if _n==2 // change wave number of final obs.
sort pidp wave
*replace sf12mcs_dv=. if wave!=11 // only end of follow-up has an outcome observation
* This change is only to make clear that the exposure is correctly defined, it is
* possible to have other values of the outcome to feed in to estimation commands

local varlist "econ_emp_bin Lecon_emp_bin ghqcase4 Lghqcase4 econ_dist"

* Set ther variables to missing for final wave
foreach var of local varlist {
	replace `var'=. if wave==11
}

*replace hiqual_dv=. if wave==11
* Time-invariant variables (e.g.. hiqual_dv) must not be set to missing since
* this creates variation over time



	gformula sf12mcs_dv  econ_emp_bin Lecon_emp_bin ghqcase4 Lghqcase4 econ_dist hiqual_dv pidp wave, ///
	outcome(sf12mcs_dv) ///
	commands(sf12mcs_dv :regress, econ_emp_bin: logit, econ_dist: logit  )  ///
	equations(sf12mcs_dv:  Lghqcase4   i.Lecon_emp_bin i.econ_emp_bin, ///
	econ_emp_bin:  i.hiqual_dv  i.Lecon_emp_bin i.Lghqcase4, ///
	econ_dist:  i.hiqual_dv  i.Lghqcase4   i.econ_emp_bin ) ///
	idvar(pidp) tvar(wave) ///
	varyingcovariates(econ_dist) intvars(econ_emp_bin) ///
	interventions( ///
	econ_emp_bin=0 if wave>=9&wave<=10, /// 00 (1)
	econ_emp_bin=0 if wave==9 \ econ_emp_bin=1 if wave==10) /// 00 (2)
	pooled eofu ///
	fixed(hiqual_dv ) ///
	laggedvars(econ_emp_bin  Lecon_emp_bin Lghqcase4 ) ///
	lagrules(econ_emp_bin: econ_emp_bin 1, Lecon_emp_bin: econ_emp_bin 2, Lghqcase4: ghqcase4 1) ///
	 ///
	seed(79) samples(2) 

	
* Task 2
* Prediction of manual g-computation

* Store observed states
gen orig_emp=econ_emp
gen orig_lemp=L.econ_emp


* Restrict sample to allow exposure and outcome in same year
keep if wave!=11
gen Lecon_emp=L.econ_emp
* Males only. Expectation of effect around 2.

capture drop prob_treat prob_compare pom_treat pom_compare

* Step 1: estimate outcome model using observed data
regress sf12mcs_dv	i.Lghqcase4 i.econ_emp Lecon_emp

* Change treatment to regime of interest and predict outcome
replace econ_emp=1
replace Lecon_emp=0
predict prob_treat

* Change treatment to comparator of interest and predict outcome
replace econ_emp=0
replace Lecon_emp=0
predict prob_compare

* Return to observed treatment states
replace econ_emp=orig_emp
replace Lecon_emp=orig_lemp

* Step 2: estimate probability of predicted outcomes given observed history
reg prob_treat i.Lghqcase4

replace Lecon_emp=0
* Generate prediction under regime of interest
predict double pom_treat


* return to observed treatment values
replace Lecon_emp=orig_lemp

reg prob_compare i.Lghqcase4

replace Lecon_emp=0
* Generate prediction under comparator of interest
predict double pom_compare

* return to observed treatment values
replace Lecon_emp=orig_lemp

mean pom*

* Average treatment effect
lincom _b [ pom_treat ] - _b [ pom_compare ] // ATE and biased confidence interval
