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

*gen lecon_econ= econ_emp*L.econ_emp

label define lecon_econ 0 "Emp_both" 1 "Emp_stu" 2 "Emp_notemp" 3 "Emp_ret" 4 "Stu_emp" 5 "Stu_both" 6 "Stu_notemp" 7 "Stu_ret" 8 "Notemp_emp" 9 "Notemp_stu" 10 "Notemp_both" 11 "Notemp_ret" 12 "Ret_emp" 13 "Ret_stu" 14 "Ret_nonemp" 15 "Ret_both"

label values Lecon_econ lecon_econ

tab Lecon_econ


recode racel_dv (1/4=0) (5/97=1) (else=.), gen (wnw_race)

label define wnw_race 0 "white" 1 "non-white"
label values wnw_race wnw_race
tab wnw_race

*Dan's code

/*

local outcome 			"scghq1_dv"
local exposures "exp_poverty exp_incchange Dlog_income"
local demog 	"age_dv age_sq hiqual_dv  "
local health 	"scsf1"
local area 		"gor_dv"
local ses 		"econ_benefits home_owner econ_incquint"
local home		"mastat_dv dnc"
local confounders	"`health' `area' `ses' `home'"
local lag_confounders	"Lecon_benefits Lhome_owner Lmastat_dv Ldnc  L`health' L`area' Lecon_incquint L`outcome'"


local fdemog 	"age_dv age_sq i.hiqual_dv" // demographic variables with factor notation
local id 		"pidp"
local year 		"intdaty_dv"
local time 		"wave"

local sample 	"sample_step2"

local weight 			"indscus_lw"

*gen Lecon_benefit=L.econ_benefit

gformula `outcome' exp_poverty econ_benefits Lecon_benefit intdaty_dv intdaty_lag hiqual_dv `id' `time', outcome(`outcome') ///
	commands(`outcome':regress, exp_poverty: mlogit, econ_benefits: logit)  ///
	equations(`outcome': exp_poverty Lecon_benefit hiqual_dv intdaty_dv, ///
	exp_poverty: Lecon_benefit intdaty_dv hiqual_dv, ///
	econ_benefits: Lecon_benefit intdaty_dv hiqual_dv) ///
	idvar(`id') tvar(`time') varyingcovariates(econ_benefits) intvars(exp_poverty) fixed(hiqual_dv) ///
	interventions(exp_poverty=1 if t<6, exp_poverty=0 if t<=1) ///
	pooled laggedvars(Lecon_benefit intdaty_lag) ///
	lagrules(Lecon_benefit: econ_benefit 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv) derrules(intdaty_dv: intdaty_lag+1) ///
	seed(79) samples(2) simulations(3)


gformula sf12pcs_dv sex_dv age_dv age_sq wnw_race hiqual_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc gor_dv Lgor_dv Lghqcase4 intdaty_dv intdaty_lag Dlog_income exp_incchange econ_emp Lecon_emp Lecon_dist econ_dist pidp wave, outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp: mlogit, Dlog_income: regress, econ_dist: logit)  ///
	equations(sf12pcs_dv: econ_emp Lecon_dist hiqual_dv intdaty_dv, ///
	econ_emp: age_dv age_sq Lecon_dist dnc intdaty_dv hiqual_dv wnw_race sex_dv intdaty_dv, ///
	econ_dist: Dlog_income Lghqcase4 exp_incchange intdaty_dv, /// 
	Dlog_income: sex_dv age_dv wnw_race hiqual_dv Lhome_owner Lmastat_dv dnc Lgor_dv Lghqcase4 intdaty_dv econ_emp intdaty_dv) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist Dlog_income) intvars(econ_emp) fixed(hiqual_dv wnw_race sex_dv) ///
	interventions(econ_emp=3 if t<6, econ_emp=1 if t<=1) ///
	pooled laggedvars(Lhome_owner Lmastat_dv Lgor_dv Lghqcase4 intdaty_lag Lecon_emp Lecon_dist) ///
	lagrules(Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Lgor_dv: gor_dv 1,  intdaty_lag: intdaty_dv 1,  Lecon_emp: econ_emp 1, Lecon_dist: econ_dist 1) ///
	derived(intdaty_dv) derrules(intdaty_dv: intdaty_lag+1) ///
	seed(79) samples(2) simulations(3)

*/	
	
	
	




*egen p_id=group(pidp)
*egen count = tag(pidp)
*expand p_id if count==1, gen(newvar)

*tab count econ_emp
*drop if count==1&econ_emp!=1
*bysort pidp (wave) : gen cum_econ_emp = sum(econ_emp)
*drop p_id count cum_econ_emp

*stset pidp, failure(econ_emp== 2 3 4)

*step by step (trying interventions) 

gen econ_emp_bin= econ_emp if inlist(econ_emp, 1, 3)
replace econ_emp_bin=0 if econ_emp==1
replace econ_emp_bin=1 if econ_emp==3

label define econ_emp_bin 0 "Employed" 1 "Non-employed"
label values econ_emp_bin econ_emp_bin

gen Lecon_emp_bin= L.econ_emp_bin

*egen keyvar=rownonmiss (age_all sex eth_bin countriesuk tujbpl_imp_rev pre_post qual5_1 finnow_bin1_1 healthcond_2 jbsize_rev_imp) if wavenum>=9

*stset wave, if (econ_emp!=4) after(econ_emp==1) failure(econ_emp==3) id(pidp) exit(econ_emp== 4)
*keep if econ_emp_bin!=.
*keep if wave>=7
*stset wave, origin(econ_emp_bin==0) 

*long version

/*/
browse sf12pcs_dv intdaty_dv econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist pidp wave
gen include=1 if Lecon_emp_bin==0&wave==7
replace include=1 if include[_n+1]==1
replace include=1 if include[_n-1]==1&pidp[_n-1]==pidp

xtset pidp wave


*streset, enter(episode_int)

*keep if _st==1
*drop _st _origin
*keep if wave>=7
by pidp:egen firsttime_1 = min(cond(inlist(econ_emp_bin, 0, 1, .), wave, .)) 

by pidp: egen lasttime_1 = max(cond(inlist(econ_emp_bin, 0, 1, .), wave, .)) 

gen times_part_1= (lasttime_1 - firsttime_1)

replace times_part_1=times_part_1+1
tab times_part_1

xtset pidp wave
bysort pidp (wave) : gen miss_wave = wave-wave[_n-1]
replace miss_wave=miss_wave-1
gen which_wave_miss=wave-miss_wave
replace which_wave_miss=. if miss_wave==0
replace miss_wave=0 if wave==firsttime_1&miss_wave==.
keep if miss_wave==0
drop which_wave_miss miss_wave
drop if times_part_1==1
keep if inrange(age_dv, 25, 64)

*bysort pidp (wave) : egen cum_wave = count(wave)

xtset pidp wave

*one by one variable	
keep if wave>=7&econ_emp_bin!=.&sf12pcs_dv!=.&sex_dv!=.&age_dv!=.&age_sq!=.&wnw_race!=.&hiqual_dv!=.&home_owner!=.&mastat_dv!=.&dnc!=.&gor_dv!=.&ghqcase4!=.&intdaty_dv!=.&log_income!=.&exp_incchange!=.&econ_emp_bin!=.&econ_dist!=.&pidp!=.&wave!=.
keep if times_part_1==4

*drop if Lecon_emp_bin==.
drop if include==.
drop include
drop if Lecon_emp_bin==.

drop firsttime_1 lasttime_1 times_part_1

by pidp:egen firsttime_1 = min(cond(inlist(econ_emp_bin, 0, 1, .), wave, .)) 

by pidp: egen lasttime_1 = max(cond(inlist(econ_emp_bin, 0, 1, .), wave, .)) 

gen times_part_1= (lasttime_1 - firsttime_1)

replace times_part_1=times_part_1+1
tab times_part_1

xtset pidp wave
bysort pidp (wave) : gen miss_wave = wave-wave[_n-1]
replace miss_wave=miss_wave-1
gen which_wave_miss=wave-miss_wave
replace which_wave_miss=. if miss_wave==0
replace miss_wave=0 if wave==firsttime_1&miss_wave==.
keep if miss_wave==0
drop which_wave_miss miss_wave
drop if times_part_1==1
keep if times_part_1==4

drop firsttime_1 lasttime_1 times_part_1



*set trace on

*problem: getting comformability error- Full model in 3 

*(1)
	gformula sf12pcs_dv intdaty_dv econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist pidp wave, outcome(sf12pcs_dv) ///
	commands(sf12pcs_dv :regress, econ_emp_bin: logit, econ_dist: logit)  ///
	equations(sf12pcs_dv: i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: sf12pcs_dv i.intdaty_dv, ///
	econ_dist:sf12pcs_dv i.econ_emp_bin i.intdaty_dv) /// 
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist) intvars(econ_emp_bin) ///
	interventions(econ_emp_bin=0 if wave=<10, ///
	econ_emp_bin=1 if wave<=8 \ econ_emp_bin=0 if wave>8 & wave<10, /// 
	econ_emp_bin=0 if wave<=8 \ econ_emp_bin=1 if wave>8 & wave<=9 \ econ_emp_bin=0 if wave>9 & wave<=10, ///
	econ_emp_bin=0 if wave<=8 \ econ_emp_bin=0 if wave>8 & wave<=9 \ econ_emp_bin=1 if wave>9 & wave<=10, ///
	econ_emp_bin=1 if wave<=8 \ econ_emp_bin=1 if wave>8 & wave<=9 \ econ_emp_bin=0 if wave>9 & wave<=10, ///
	econ_emp_bin=0 if wave<=8 \ econ_emp_bin=1 if wave>8 & wave<=9 \ econ_emp_bin=1 if wave>9 & wave<=10, ///
	econ_emp_bin=1 if wave<=8 \ econ_emp_bin=0 if wave>8 & wave<=9 \ econ_emp_bin=1 if wave>9 & wave<=10, ///
	econ_emp_bin=0 if wave<=10) ///
	pooled eofu laggedvars(Lecon_emp_bin Lecon_dist) ///
	lagrules(Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1) ///
	seed(79) samples(2) simulations(3)
	

*000/100/010/001/110/011/101/111 

*/

*(2) - Here after (3- chan
*egen agebands5=cut(age_dv), at(25(5)65)
*tab agebands5

*duplicates report age_dv pidp
*duplicates tag age_dv pidp, gen (dupiage)
*gen age_rec=age_dv
*bysort pidp: replace age_rec= age_dv+0.5 if age_rec==age_rec[_n-1]
*duplicates report age_rec pidp, gen (dupiage2)
xtset pidp wave

label define mastat 1 "Partnered", modify

label define mastat 2 "Single-never married", modify

label define mastat 3 "Previously partnered", modify


recode gor_dv (10=1) (9=2) (7=3) (5=4) (12=5) (2=6) (11=7) (13=8) (4=9) (6=10) (8=11) (1=12), gen (gor_unem)	

gen Lgor_unem= L.gor_unem

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
	
	
	*mcs-10 bootstraps
gen	Lsf12mcs_dv = L.sf12mcs_dv
	
		gformula sf12mcs_dv sex_dv age_dv age_sq hiqual_dv Lsf12mcs_dv home_owner Lhome_owner mastat_dv Lmastat_dv dnc Ldnc gor_dv Lgor_dv sf12pcs_dv Lsf12pcs_dv ghqcase4 Lghqcase4 log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave if wave>=7, outcome(sf12mcs_dv) ///
	commands(sf12mcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress, ghqcase4: logit, mastat_dv:ologit, dnc:ologit)  ///
	equations(sf12mcs_dv: i.sex_dv i.hiqual_dv Lsf12pcs_dv Lsf12mcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income i.Lecon_emp_bin i.Lecon_dist i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv age_dv age_sq i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv i.Lghqcase4 Llog_income sf12pcs_dv sf12mcs_dv Lecon_dist i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv age_dv age_sq i.home_owner i.mastat_dv i.dnc i.gor_dv i.ghqcase4 sf12mcs_dv log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: Llog_income sf12pcs_dv sf12mcs_dv Lsf12mcs_dv age_dv age_sq i.hiqual_dv i.econ_emp_bin i.home_owner i.mastat_dv i.dnc i.ghqcase4 i.gor_dv i.Ldnc i.Lghqcase4 i.intdaty_dv, ///
	ghqcase4: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv Lsf12mcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	mastat_dv: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv Lsf12mcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag, ///
	dnc: i.sex_dv age_dv age_sq i.hiqual_dv i.Lghqcase4 Lsf12pcs_dv Lsf12mcs_dv i.Lhome_owner i.Lmastat_dv i.Ldnc i.Lgor_dv Llog_income i.Lecon_emp_bin i.econ_emp_bin i.Lecon_dist i.econ_dist i.intdaty_lag) ///
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
	pooled eofu fixed(hiqual_dv sex_dv) laggedvars(Lsf12pcs_dv Lsf12mcs_dv Lhome_owner Lmastat_dv Ldnc Lgor_dv Lghqcase4 Llog_income Lecon_emp_bin Lecon_dist intdaty_lag) ///
	lagrules(Lsf12pcs_dv: sf12pcs_dv 1, Lsf12mcs_dv: sf12mcs_dv 1, Lhome_owner: home_owner 1, Lmastat_dv: mastat_dv 1, Ldnc: dnc 1, Lgor_dv: gor_dv 1, Lghqcase4: ghqcase4 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
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

	
