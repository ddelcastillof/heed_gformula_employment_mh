
use "T:\projects\HEED\Data\USoc prepared data\Test_Harry14.dta", clear  /*Open dataset*/ 


gen age_sample=1 if inrange(age_dv,25,64)  /*Defining sample in terms of age*/

keep if age_sample==1    /*Deleting all people younger than 25 (24 and younger) and older than 65 (65 included) */

*Generating our exposure (Employment status)
tab jbstat, miss
gen les_c4=.
replace les_c4=1 if jbstat==1 | jbstat==2 
*employed = self-employed, paid employed
replace les_c4=2 if jbstat==7
* full-time student
replace les_c4=3 if jbstat==3 | jbstat==5 | jbstat==6 | jbstat==8 | jbstat==9 | jbstat==10 | jbstat==11 | jbstat==97
* unemployed, maternity leave, family care, long-term sick, govt. training scheme, unpaid family business, apprentice, doing something else
replace les_c4=4 if jbstat==4
* Retired

*labelling variable
label drop jst /* seems there was a jst label stored - I deleted this */

* Here I am redefining its values*
label define jst 1 "Employed or self-employed"  2 "Student" 3 "Not employed" ///
4 "Retired" 
label values les_c4 jst
* labelling variable
label variable les_c4 "Broad employment state"

* Generate long-term sick, retired, and in education dummies
drop econ_ltsick
gen econ_ltsick=(jbstat==8)
label var econ_ltsick "Long-term sick"
drop econ_retire
gen econ_retire=(les_c4==4)
label var econ_retire "Retired"
gen econ_student=(les_c4==2)
label var econ_student "Student"

* Clean job status variable
* Key is to ensure not-employed are at risk of work
* In SimPaths students, retired, and long-term sick are not at risk of work
tab les_c4, miss
drop econ_emp
gen econ_emp=.
replace econ_emp=1 if les_c4==1
replace econ_emp=2 if les_c4==2
replace econ_emp=3 if les_c4==3 & econ_ltsick!=1
replace econ_emp=4 if les_c4==4
replace econ_emp=5 if econ_ltsick==1
label drop jbf
label define jbf 1 "Employed or self-employed"  2 "Student" 3 "Not employed (at risk of work)" 4 "Retired" 5 "Long-term sick"
label values econ_emp jbf
label variable econ_emp "Economic: employment state"

tab econ_emp

*Creating employment status as binary*
gen econ_emp_bin= econ_emp if inlist(econ_emp, 1, 3)
replace econ_emp_bin=0 if econ_emp==1
replace econ_emp_bin=1 if econ_emp==3

label define econ_emp_bin 0 "Employed" 1 "Non-employed"
label values econ_emp_bin econ_emp_bin

gen Lecon_emp_bin= L.econ_emp_bin

*keep only those who are either employed, not employed or have missing values
keep if inlist(econ_emp,1,3,.)

*Creating one-wave lags for the variable we would need later on
gen Lecon_emp=L.econ_emp /*Economic activity (0:Employed, 1: Unemployed*/
gen intdaty_lag=L.intdaty_dv /*Year minus 2000*/
gen Lhome_owner=L.home_owner /*homeowner (0: Renter, 1: Owner */
gen Lmastat_dv=L.mastat_dv /*De facto marital status (1: Partnered, 2: Single and never married, 3: Previously partnered) */
gen Ldnc=L.dnc  /*Number of depedent children over 18 (0, 1, 2, 3, 4 or more)*/
gen Lgor_dv = L.gor_dv /*Government office region (12 values)*/
gen Lghqcase4 = L.ghqcase4 /*GHQ caseness (0: No, 1: Yes)*/
gen Dlog_income = D.log_income /*log of equivalised household income (difference between two consecutive waves)*/
gen Lecon_dist = L.econ_dist /*economic distress (0: No, 1: Yes)*/
gen Llog_income = L.log_income /*log of equivalised household income*/
gen Lsf12pcs_dv = L.sf12pcs_dv /*SF-12 Physical Component Summary*/
gen Lsf12mcs_dv = L.sf12mcs_dv /*SF-12 Mental Component Summary*/
gen Lage_dv=L.age_dv

*Recode ethnicity into white/nonwhite

recode racel_dv (1/4=0) (5/97=1) (else=.), gen (wnw_race)

*label variable
label define wnw_race 0 "white" 1 "non-white"
label values wnw_race wnw_race

* Fixing all values of the variables we would be considering as fixed in the gformula command based on their baseline values (her baseline is Wave 6 but it can change) 
* create new variable for regions assigning the Wave 7 value to all susequent waves
clonevar gor_re_base=gor_dv if wave==6 /*create a clone means that all variable attributes and value labels are copied too */
by pidp: replace gor_re_base=gor_re_base[_n-1] if wave==7&gor_dv[_n-1]!=.
by pidp: replace gor_re_base=gor_re_base[_n-2] if wave==8&gor_dv[_n-2]!=.
by pidp: replace gor_re_base=gor_re_base[_n-3] if wave==9&gor_dv[_n-3]!=.
by pidp: replace gor_re_base=gor_re_base[_n-4] if wave==10&gor_dv[_n-4]!=.


gen Lgor_re_base=L.gor_re_base

* create new variable for mrital_status assigning the Wave 7 value to all susequent waves
clonevar mastat_dv_base=mastat_dv if wave==6
by pidp: replace mastat_dv_base=mastat_dv_base[_n-1] if wave==7&gor_dv[_n-1]!=.
by pidp: replace mastat_dv_base=mastat_dv_base[_n-2] if wave==8&gor_dv[_n-2]!=.
by pidp: replace mastat_dv_base=mastat_dv_base[_n-3] if wave==9&gor_dv[_n-3]!=.
by pidp: replace mastat_dv_base=mastat_dv_base[_n-4] if wave==10&gor_dv[_n-4]!=.

gen Lmastat_dv_base=L.mastat_dv_base

	
* create new variable for home_owners assigning the Wave 7 value to all susequent waves
clonevar home_owner_base=home_owner if wave==6
by pidp: replace home_owner_base=home_owner_base[_n-1] if wave==7&gor_dv[_n-1]!=.
by pidp: replace home_owner_base=home_owner_base[_n-2] if wave==8&gor_dv[_n-2]!=.
by pidp: replace home_owner_base=home_owner_base[_n-3] if wave==9&gor_dv[_n-3]!=.
by pidp: replace home_owner_base=home_owner_base[_n-4] if wave==10&gor_dv[_n-4]!=.

gen Lhome_owner_base=L.home_owner_base

	
* create new variable for No of depedent children assigning the Wave 7 value to all susequent waves
clonevar dnc_base=dnc if wave==6
by pidp: replace dnc_base=dnc_base[_n-1] if wave==7&gor_dv[_n-1]!=.
by pidp: replace dnc_base=dnc_base[_n-2] if wave==8&gor_dv[_n-2]!=.
by pidp: replace dnc_base=dnc_base[_n-3] if wave==9&gor_dv[_n-3]!=.
by pidp: replace dnc_base=dnc_base[_n-4] if wave==10&gor_dv[_n-4]!=.


gen Ldnc_base=L.dnc_base



