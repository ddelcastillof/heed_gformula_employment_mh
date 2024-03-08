* file_name.do: hfcovid_model06v3-poverty_dk: Step 2 g-formula estimates for direct effect of poverty transitions
* Daniel Kopasker 010322


* This is an output phase and assumes hfcovid_clean06v*-sample2_dk.do was run first.

** Stata recognises lines starting with an asterisk as comments
// Stata recognises anything written after double fwd slashes as comments
/// Three backslashes are a line delimiter
/* Everything between these
symbols is treated as a comment
*/


********************************************************
**** Section 1: Setup 					****************
********************************************************


capture log close // close log file if one is open, ignore otherwise



clear all    // drops any data currently in the working directory, 
macro drop _all // drops an macros currently in the memory
version 17   // version control: version of Stata in which this file is written
set more off // With -set more off- Stata runs the entire do-file without pausing

*set linesize 80 // specifies screen width


* set up a local macro containing file path to raw data directory
// --> CHANGE THE FILE PATH TO WHERE THE DATA IS STORED
* note that quotation marks are needed in case of spaces in file names.
*local data "C:\___Daniel_local\myonedrive\OneDrive - University of Glasgow\HF_project_wdir\Data-Files"


* change to working directory - this is the directory Stata will save files to
// --> CHANGE THE WORKING DIRECTORY TO YOUR OWN PROJECT SPECIFIC WORKING FILE FOLDER. 
*local working "T:\projects\HEED\Data"


*cd "`working'"

* open log file
* NB ".log" suffix makes log file plain ascii text; else is smcl format.
* log files can be read with Notepad and Word, smcl files require stata
log using hfcovid_model_physical_health_TK_short_14, text replace  

* Specify dataset to be used
*use "T:\projects\HEED\Data\USoc prepared data\heed_analysis.dta", clear 
use "T:\projects\HEED\Data\USoc prepared data\Test_Harry14.dta", clear 
*use "T:\projects\HEED\Data\USoc prepared data\Test_Harry14.dta", clear 


******************************************************
**** Section 2: Tasks						**********
******************************************************
numlabel _all,remove

* Check the source data
datasignature confirm
notes _dta
xtset pidp wave

gen Lecon_emp=L.econ_emp
gen intdaty_lag=L.intdaty_dv
gen Lhome_owner=L.home_owner
gen Lmastat_dv=L.mastat_dv
gen Ldnc=L.dnc
gen Lgor_dv = L.gor_dv
gen Lghqcase4 = L.ghqcase4
gen Dlog_income = D.log_income
gen Lecon_dist = L.econ_dist
gen Llog_income = L.log_income
gen Lsf12pcs_dv = L.sf12pcs_dv


gen Lecon_econ= .

replace Lecon_econ=0 if econ_emp==1 & Lecon_emp==1 
replace Lecon_econ=1 if econ_emp==1 & Lecon_emp==2 
replace Lecon_econ=2 if econ_emp==1 & Lecon_emp==3 
replace Lecon_econ=3 if econ_emp==1 & Lecon_emp==4 
replace Lecon_econ=4 if econ_emp==2 & Lecon_emp==1 
replace Lecon_econ=5 if econ_emp==2 & Lecon_emp==2 
replace Lecon_econ=6 if econ_emp==2 & Lecon_emp==3 
replace Lecon_econ=7 if econ_emp==2 & Lecon_emp==4 
replace Lecon_econ=8 if econ_emp==3 & Lecon_emp==1 
replace Lecon_econ=9 if econ_emp==3 & Lecon_emp==2 
replace Lecon_econ=10 if econ_emp==3 & Lecon_emp==3 
replace Lecon_econ=11 if econ_emp==3 & Lecon_emp==4 
replace Lecon_econ=12 if econ_emp==4 & Lecon_emp==1  
replace Lecon_econ=13 if econ_emp==4 & Lecon_emp==2 
replace Lecon_econ=14 if econ_emp==4 & Lecon_emp==3 
replace Lecon_econ=15 if econ_emp==4 & Lecon_emp==4 

label define lecon_econ 0 "Emp_both" 1 "Emp_stu" 2 "Emp_notemp" 3 "Emp_ret" 4 "Stu_emp" 5 "Stu_both" 6 "Stu_notemp" 7 "Stu_ret" 8 "Notemp_emp" 9 "Notemp_stu" 10 "Notemp_both" 11 "Notemp_ret" 12 "Ret_emp" 13 "Ret_stu" 14 "Ret_nonemp" 15 "Ret_both"

label values Lecon_econ lecon_econ

tab Lecon_econ


recode racel_dv (1/4=0) (5/97=1) (else=.), gen (wnw_race)

label define wnw_race 0 "white" 1 "non-white"
label values wnw_race wnw_race
tab wnw_race

gen econ_emp_bin= econ_emp if inlist(econ_emp, 1, 3)
replace econ_emp_bin=0 if econ_emp==1
replace econ_emp_bin=1 if econ_emp==3

label define econ_emp_bin 0 "Employed" 1 "Non-employed"
label values econ_emp_bin econ_emp_bin

gen Lecon_emp_bin= L.econ_emp_bin

xtset pidp wave

label define mastat 1 "Partnered", modify

label define mastat 2 "Single-never married", modify

label define mastat 3 "Previously partnered", modify

bysort	pidp:	gen	cum_emp=sum(econ_emp_bin)

gen Lcum_emp=L.cum_emp

gen age_sample=1 if inrange(age_dv,25,65)

keep if age_sample==1

recode gor_dv (1/10=1) (11=2) (12=3) (13=4), gen (ukcount)

label define ukcount 1 "England" 2 "Wales" 3 "Scotland" 4 "NI"
label values ukcount ukcount
tab ukcount

gen Lukcount= L.ukcount

*Can't converge because of gor_dv/ gor_unem

	gformula sf12pcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12pcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc gor_dv Lgor_dv ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=7, outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit, gor_dv: ologit )  ///
	equations(sf12pcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income sf12pcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.gor_dv i.ghqcase4 log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.gor_dv i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	gor_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income ghqcase4 mastat_dv dnc gor_dv) intvars(econ_emp_bin) ///
	interventions( ///
	econ_emp_bin=0 if wave=<10, /// 0-000
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 0-100
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-010
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-001
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-110
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-011 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-101 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=>8 ) /// 0-111
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lhome_owner Lmastat_dv Ldnc Lgor_dv Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lgor_dv: gor_dv 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv) ///
	seed(79) samples(10)  simulations(10)

	
*Replace gor_dv with ukcount
		gformula sf12pcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12pcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc ukcount Lukcount ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=7, outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit, ukcount: ologit )  ///
	equations(sf12pcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lukcount i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lukcount i.Lghqcase4 Llog_income sf12pcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.ukcount i.ghqcase4 log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.ukcount i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lukcount Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lukcount Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lukcount Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	ukcount: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lukcount Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income ghqcase4 mastat_dv dnc ukcount) intvars(econ_emp_bin) ///
	interventions( ///
	econ_emp_bin=0 if wave=<10, /// 0-000
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 0-100
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-010
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-001
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-110
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-011 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-101 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=>8 ) /// 0-111
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lhome_owner Lmastat_dv Ldnc Lukcount Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lukcount: ukcount 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv) ///
	seed(79) samples(10)  simulations(10)	

	
	*excluding regions (gor_dv) (Model 1)
	gformula sf12pcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12pcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc gor_dv Lgor_dv ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=7, outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit)  ///
	equations(sf12pcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income sf12pcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.gor_dv i.ghqcase4 log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.gor_dv i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income ghqcase4 mastat_dv dnc) intvars(econ_emp_bin) ///
	interventions( ///
	econ_emp_bin=0 if wave=<10, /// 0-000
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 0-100
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-010
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-001
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-110
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-011 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-101 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=>8 ) /// 0-111
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lhome_owner Lmastat_dv Ldnc Lgor_dv Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lgor_dv: gor_dv 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv) ///
	seed(79) samples(10)  simulations(10)	
	
	
	*1000 bootstraps (Model 1)
	
		gformula sf12pcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12pcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc gor_dv Lgor_dv ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=7, outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit)  ///
	equations(sf12pcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income sf12pcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.gor_dv i.ghqcase4 log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.gor_dv i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income ghqcase4 mastat_dv dnc) intvars(econ_emp_bin) ///
	interventions( ///
	econ_emp_bin=0 if wave=<10, /// 0-000
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 0-100
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-010
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-001
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-110
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-011 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-101 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=>8 ) /// 0-111
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lhome_owner Lmastat_dv Ldnc Lgor_dv Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lgor_dv: gor_dv 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv) ///
	seed(79) samples(1000) simulations(1000)	
	

*whole sample with 1000 bootstraps excluding regions on the working age population (25 to 65y old) (Model 1)

	
		gformula sf12pcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12pcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc gor_dv Lgor_dv ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=7, outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit)  ///
	equations(sf12pcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income sf12pcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.gor_dv i.ghqcase4 log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.gor_dv i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income ghqcase4 mastat_dv dnc) intvars(econ_emp_bin) ///
	interventions( ///
	econ_emp_bin=0 if wave=<10, /// 0-000
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 0-100
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-010
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-001
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-110
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-011 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-101 
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=>8 ) /// 0-111
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lhome_owner Lmastat_dv Ldnc Lgor_dv Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lgor_dv: gor_dv 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv) ///
	seed(79) samples(1000) 	saving (gform_MCsims_1000)  replace
	

*Including previous history of at least one incident of employment excluding regions (Model 2)	

bysort pidp: egen past_emp7 = max(econ_emp_bin) if wave<=7

	gformula sf12pcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12pcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc gor_dv Lgor_dv ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=7 , outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit )  ///
	equations(sf12pcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income sf12pcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.gor_dv i.ghqcase4 log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.gor_dv i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income ghqcase4 mastat_dv dnc) intvars(econ_emp_bin) ///
	interventions( ///
	past_emp=0 if wave<=7 \ econ_emp_bin=0 if wave>=8 & wave=<10, /// 0-000
	past_emp=0 if wave<=7 \econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 0-100
	past_emp=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-010
	past_emp=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-001
	past_emp=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-110
	past_emp=0 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-011 
	past_emp=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-101 
	past_emp=0 if wave<=7 \ econ_emp_bin=1 if wave=>8,  /// 0-111
	past_emp=1 if wave<=7 \ econ_emp_bin=0 if wave>=8 & wave=<10, /// 1-000
	past_emp=1 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 1-100
	past_emp=1 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 1-010
	past_emp=1 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 1-001
	past_emp=1 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 1-110
	past_emp=1 if wave<=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=1 if wave=10, /// 1-011 
	past_emp=1 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 1-101 
	past_emp=1 if wave<=7 \ econ_emp_bin=1 if wave=>8 ) /// 1-111	
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lhome_owner Lmastat_dv Ldnc Lgor_dv Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag ) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lgor_dv: gor_dv 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv) ///
	seed(79) samples(10) simulations(10) 		


	
	
	*different variant adding \ econ_emp_bin==0 if wave<=7 \ (Model 3)
		gformula sf12pcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12pcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc gor_dv Lgor_dv ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=7 , outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit )  ///
	equations(sf12pcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income sf12pcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.gor_dv i.ghqcase4 log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.gor_dv i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income ghqcase4 mastat_dv dnc) intvars(econ_emp_bin) ///
	interventions( ///
	past_emp7=0 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=0 if wave>=8 & wave=<10, /// 0-0-000
	past_emp7=0 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 0-0-100
	past_emp7=0 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave==9 \ econ_emp_bin=0 if wave==10, /// 0-0-010
	past_emp7=0 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave==9 \ econ_emp_bin=1 if wave==10, /// 0-0-001
	past_emp7=0 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave==9 \ econ_emp_bin=0 if wave==10, /// 0-0-110
	past_emp7=0 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave==9 \ econ_emp_bin=1 if wave==10, /// 0-0-011 
	past_emp7=0 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave==9 \ econ_emp_bin=1 if wave==10, /// 0-0-101 
	past_emp7=0 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave>=8,  /// 0-0-111
	past_emp7=1 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=0 if wave>=8 , /// 1-0-000
	past_emp7=1 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>8, /// 1-0-100
	past_emp7=1 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 1-0-010
	past_emp7=1 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 1-0-001
	past_emp7=1 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10, /// 1-0-110
	past_emp7=1 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=1 if wave=10, /// 1-0-011 
	past_emp7=1 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 1-0-101 
	past_emp7=1 & econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=>8 ) /// 1-0-111	
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lhome_owner Lmastat_dv Ldnc Lgor_dv Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag ) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lgor_dv: gor_dv 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv) ///
	seed(79) samples(10) simulations(10) 
	
	
	
		*different variant adding \ an additional wave (0000 0001 0010 0011 0100 0101 0110 0111 1000 1001 1010 1011 1100 1101 1110 1111) (Model 4)
		gformula sf12pcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12pcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc gor_dv Lgor_dv ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=6 , outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit )  ///
	equations(sf12pcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income sf12pcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.gor_dv i.ghqcase4 log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.gor_dv i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income ghqcase4 mastat_dv dnc) intvars(econ_emp_bin) ///
	interventions(   /// 
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=0 if wave>6, /// 0-0000
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=0 if wave>6 & wave<10 \ econ_emp_bin=1 if wave=10, /// 0-0001
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=0 if wave>6 & wave>9 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave=10 , /// 0-0010
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=0 if wave<9 \ econ_emp_bin=1 if wave>=9, /// 0-0011
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>=9, /// 0-0100
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=0 if wave=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-0101 
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave=7 \ econ_emp_bin=1 if wave>7 & wave<10 \ econ_emp_bin=0 if wave==10, /// 0-0110 
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave=>7,  /// 0-0111
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave=7 \ econ_emp_bin=0 if wave>=8, /// 0-1000
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave=7 \ econ_emp_bin=0 if wave>7 & wave<10 \ econ_emp_bin=1 if wave==10, /// 0-1001
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave=9 \ econ_emp_bin=0 if wave==10, /// 0-1010
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave=7 \ econ_emp_bin=0 if wave=8 \ econ_emp_bin=1 if wave>8, /// 0-1011
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave>6 & wave<9 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=0 if wave=10, /// 0-1100
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave>6 & wave<9 \ econ_emp_bin=0 if wave=9 \ econ_emp_bin=1 if wave=10, /// 0-1101 
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave>6 & wave<10 \ econ_emp_bin=0 if wave=10, /// 0-1110 
	econ_emp_bin==0 if wave<=6 \ econ_emp_bin=1 if wave=>7 ) /// 0-1111
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lhome_owner Lmastat_dv Ldnc Lgor_dv Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag ) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lgor_dv: gor_dv 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv) ///
	seed(79) samples(10) simulations(10)

	
