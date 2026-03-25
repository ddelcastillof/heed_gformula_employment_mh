***************************************************************************************
* PROJECT:              Health Equity and its Economic Determinants
* DO-FILE NAME:         02_prepare_variables.do
* DESCRIPTION:          Creates dnc, home_owner, econ_benefits, econ_dist, household income
***************************************************************************************
* COUNTRY:              UK
* DATA:         	    UKHLS EUL version - UKDA-6614-stata [to wave o]
* AUTHORS: 				Darwin del Castillo
* LAST UPDATE:          17 Mar 2026
* NOTES:				Called from 00_master.do - see master file for further details
*						Use -9 for missing values 
***************************************************************************************

*****************************************************************************************************
cap log close
log using "${dir_log}/02_create_UKHLS_variables.log", replace
*****************************************************************************************************

use "${dir_out}/ukhls_pooled_all_obs_01.dta", clear
lab define dummy 1 "yes" 0 "no"

set seed 12345

/**************************************************************************************
* SAMPLE PREPROCESSING
*************************************************************************************/

***Drop IEMB: 
fre hhorig
/*hhorig	-- Sample origin, household
1  ukhls gb 2009-10	
2  ukhls ni 2009-10	
3  bhps gb 1991	
4  bhps sco 1999	
5  bhps wal 1999	
6  bhps ni 2001	
7  ukhls emboost 2009-10
8  ukhls iemb 2014-15
21 ukhls gps2 gb 2022-2023
22 ukhls gps2 ni 2022-2023
*/
drop if hhorig == 8

***Keep everyone at this stage as we need info on partners and parents who do not have full/proxi interviews in some waves*/
fre ivfio
/*
ivfio -- Individual level outcome
1  full interview
2  proxy interview
9  lost capi intvw		
10 refusal		
11 other non-intvw		
12 moved		
14 ill/away during survey period				          
15 too infirm/elderly		
16 language difficulties		
18 unknown eligibility		
21 youth interview		
22 youth: refusal		
24 child under 10 
25 youth non-interview	
53 non-cont/non-int hh	
63 chd <16 non-cont/non-int hh	
74 ineligible - aged 10-15	(iemb only)				          
81 prev wave adamant refusal	
84 Ineligible for individual interview as nr in last 3 waves 				          

keep if ivfio == 1 | ivfio == 2 | ivfio == 21 | ivfio == 24 
fre ivfio 
*/

/**************************************************************************************
* HOUSEHOLD IDENTIFIER (needed for dnc bysort)
*************************************************************************************/
clonevar idhh = hidp
la var idhh "Household identifier"


/**************************************************************************************
* INDIVIDUAL IDENTIFIER
*************************************************************************************/
clonevar idperson = pidp
la var idperson "Unique cross wave identifier"

xtset idperson swv


/**************************************************************************************
* AGE (dag) — needed for dnc, dhhtp_c4
*************************************************************************************/
gen dag = age_dv
la var dag "Age"


/**************************************************************************************
* GENDER (dgn)
*************************************************************************************/
gen dgn = sex_dv
recode dgn 2=0   // 0=female, 1=male
lab define dgn 1 "men" 0 "women"
lab val dgn dgn
la var dgn "Gender"


/**************************************************************************************
* REGION (drgn1)
*************************************************************************************/
gen drgn1 = .
replace drgn1 = 1  if gor_dv == 1
replace drgn1 = 2  if gor_dv == 2
replace drgn1 = 4  if gor_dv == 3
replace drgn1 = 5  if gor_dv == 4
replace drgn1 = 6  if gor_dv == 5
replace drgn1 = 7  if gor_dv == 6
replace drgn1 = 8  if gor_dv == 7
replace drgn1 = 9  if gor_dv == 8
replace drgn1 = 10 if gor_dv == 9
replace drgn1 = 11 if gor_dv == 10
replace drgn1 = 12 if gor_dv == 11
replace drgn1 = 13 if gor_dv == 12
la var drgn1 "Region"


/**************************************************************************************
* HEALTH STATUS (dhe)
* Reverse-coded so 5=Excellent, 1=Poor
*************************************************************************************/
replace scsf1 = . if scsf1 < 0
recode scsf1 (5=1) (4=2) (3=3) (2=4) (1=5), gen(dhe)
la var dhe "Health status"


/**************************************************************************************
* LIFE SATISFACTION (dls)
* Rescaled from 1-7 to 0-10
*************************************************************************************/
gen dls = sclfsato
replace dls = . if sclfsato < 0
replace dls = (dls - 1) * 10/6
la var dls "Life satisfaction (0-10)"


/**************************************************************************************
* PARTNER IDENTIFIER (needed for dcpst)
*************************************************************************************/
clonevar idpartner = ppid
la var idpartner "Unique cross wave identifier of partner"


/**************************************************************************************
* PARTNERSHIP STATUS (needed for dhhtp_c4)
*************************************************************************************/
gen dcpst = .
replace dcpst = 1 if idpartner > 0 & !missing(idpartner) //partnered
replace dcpst = 2 if idpartner < 0 | missing(idpartner)  //single
lab var dcpst "Partnership status"
lab def dcpst 1 "partnered" 2 "single"
lab val dcpst dcpst


/**************************************************************************************
* DEPENDENT CHILDREN (dnc)
* Requires globals: $age_max_dep_child (set in 00_master.do)
* Requires source vars: age_dv, pns1pid, pns2pid, depchl_dv
*************************************************************************************/
gen depChild = 1 if (age_dv >= 0 & age_dv < ${age_max_dep_child}) & (pns1pid > 0 | pns2pid > 0) & (depchl_dv == 1)
bys swv idhh: egen dnc = sum(depChild)
drop depChild
la var dnc "Number of dependent children 0-${age_max_dep_child}"


/**************************************************************************************
* ADULT CHILD FLAG (needed for dhhtp_c4)
*************************************************************************************/
cap gen adultchildflag = 0


/**************************************************************************************
* HOUSEHOLD COMPOSITION (needed for yhhnb equivalisation)
* Requires globals: $age_becomes_responsible (set in 00_master.do)
*************************************************************************************/
cap gen dhhtp_c4 = .
replace dhhtp_c4 = 1 if dcpst == 1 & dnc == 0                                                          //Couple, no children
replace dhhtp_c4 = 2 if dcpst == 1 & dnc > 0 & !missing(dnc)                                           //Couple, children
replace dhhtp_c4 = 3 if (dcpst == 2) & (dnc == 0 | dag <= ${age_becomes_responsible} | adultchildflag == 1) //Single, no children
replace dhhtp_c4 = 4 if (dcpst == 2) & dnc > 0 & !missing(dnc) & dhhtp_c4 != 3                         //Single, children

la def dhhtp_c4_lb 1 "Couple with no children" 2 "Couple with children" 3 "Single with no children" 4 "Single with children"
la values dhhtp_c4 dhhtp_c4_lb
la var dhhtp_c4 "Household composition"


/**************************************************************************************
* CPI (inflation adjustment, base year = 2015)
* Source: ONS https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l522/mm23
*************************************************************************************/
gen CPI = .
replace CPI = 0.879 if intdaty_dv == 2009
replace CPI = 0.901 if intdaty_dv == 2010
replace CPI = 0.936 if intdaty_dv == 2011
replace CPI = 0.96  if intdaty_dv == 2012
replace CPI = 0.982 if intdaty_dv == 2013
replace CPI = 0.996 if intdaty_dv == 2014
replace CPI = 1     if intdaty_dv == 2015
replace CPI = 1.01  if intdaty_dv == 2016
replace CPI = 1.036 if intdaty_dv == 2017
replace CPI = 1.06  if intdaty_dv == 2018
replace CPI = 1.078 if intdaty_dv == 2019
replace CPI = 1.089 if intdaty_dv == 2020
replace CPI = 1.116 if intdaty_dv == 2021
replace CPI = 1.205 if intdaty_dv == 2022
replace CPI = 1.286 if intdaty_dv == 2023
replace CPI = 1.329 if intdaty_dv == 2024
replace CPI = 1.329 if intdaty_dv == 2025 //to update when available


/**************************************************************************************
* GROSS PERSONAL NON-BENEFIT INCOME (ypnb) — needed for yhhnb
* Components: labour income + pensions + private income sources
* Requires: inc_pp, inc_tu, inc_ma, inc_fm, inc_oth from 01_merge.do income section
*************************************************************************************/
recode fimnlabgrs_dv fimnpen_dv inc_pp inc_tu inc_ma inc_fm inc_oth (-9=.) (-1=.)
egen ypnb = rowtotal(fimnlabgrs_dv fimnpen_dv inc_pp inc_tu inc_ma inc_fm inc_oth)
replace ypnb = 0 if ypnb < 0
replace ypnb = ypnb / CPI //adjust for inflation


/**************************************************************************************
* PARTNER'S GROSS PERSONAL NON-BENEFIT INCOME (ypnbsp) — needed for yhhnb
*************************************************************************************/
preserve
keep swv idperson idhh ypnb
rename ypnb ypnbsp
rename idperson idpartner
save "$dir_out/temp_ypnb", replace
restore
merge m:1 swv idpartner idhh using "$dir_out/temp_ypnb"
keep if _merge == 1 | _merge == 3
drop _merge
replace ypnbsp = ypnbsp / CPI


/**************************************************************************************
* MODIFIED OECD EQUIVALENCE SCALE (needed for yhhnb equivalisation)
*************************************************************************************/
gen depChild_013  = 1 if (dag >= 0  & dag <= 13) & (pns1pid > 0 | pns2pid > 0) & (depchl_dv == 1)
gen depChild_1418 = 1 if (dag >= 14 & dag <= 18) & (pns1pid > 0 | pns2pid > 0) & (depchl_dv == 1)
bys swv idhh: egen dnc013  = sum(depChild_013)
bys swv idhh: egen dnc1418 = sum(depChild_1418)
drop depChild_013 depChild_1418

gen moecd_eq = .
replace moecd_eq = 1.5                                if dhhtp_c4 == 1
replace moecd_eq = 0.3*dnc013 + 0.5*dnc1418 + 1.5   if dhhtp_c4 == 2
replace moecd_eq = 1                                  if dhhtp_c4 == 3
replace moecd_eq = 0.3*dnc013 + 0.5*dnc1418 + 1      if dhhtp_c4 == 4
drop dnc013 dnc1418


/**************************************************************************************
* HOUSEHOLD INCOME (yhhnb)
* Sum of individual + partner income if coupled; individual only if single
* Equivalised by modified OECD scale and CPI-adjusted
*************************************************************************************/
egen yhhnb = rowtotal(ypnb ypnbsp) if dhhtp_c4 == 1 | dhhtp_c4 == 2
replace yhhnb = ypnb if dhhtp_c4 == 3 | dhhtp_c4 == 4
replace yhhnb = (yhhnb / moecd_eq) / CPI

gen yhhnb_asinh = asinh(yhhnb)
gen log_income = yhhnb_asinh
la var yhhnb "Equivalised household gross non-benefit income (real)"
la var yhhnb_asinh "Equivalised household gross non-benefit income (IHS-transformed)"
la var log_income "Log equivalised household income (IHS transformation of yhhnb)"

/**************************************************************************************
* INCOME QUINTILES (econ_incquint)
* Within-wave quintiles of equivalised household income
*************************************************************************************/
gen econ_incquint = .
levelsof swv, local(waves)
foreach w of local waves {
    xtile tmp_q = yhhnb if swv == `w', nq(5)
    replace econ_incquint = tmp_q if swv == `w'
    drop tmp_q
}
la var econ_incquint "Household income quintile (within wave)"


/**************************************************************************************
* HOME OWNERSHIP (home_owner)
* Requires source var: hsownd (housing tenure from hhresp)
*************************************************************************************/
gen home_owner = .
replace home_owner = 1 if hsownd >= 1 & hsownd <= 3
replace home_owner = 0 if inlist(hsownd, 4, 5)
replace home_owner = . if hsownd < 0 | hsownd > 5 
lab var home_owner "Home ownership dummy"


/**************************************************************************************
* ACTIVITY STATUS (les_c3, les_c4) — needed for unemp
*************************************************************************************/
recode jbstat (1 2 5 12 13 14 15 = 1 "Employed or self-employed") ///
	(7 = 2 "Student") ///
	(3 6 8 10 11 97 9 4 = 3 "Not employed") ///
	, into(les_c3)
replace les_c3 = . if les_c3 < 0
la var les_c3 "Activity status"

clonevar les_c4 = les_c3
replace les_c4 = 4 if jbstat == 4
lab var les_c4 "Activity status (incl. retired)"
lab define les_c4 1 "Employed or self-employed" 2 "Student" 3 "Not employed" 4 "Retired"
lab val les_c4 les_c4
replace les_c4 = . if les_c4 < 0


/**************************************************************************************
* UNEMPLOYMENT DUMMY (unemp)
* Requires: jbstat, les_c3, les_c4, dag
* Requires global: $age_seek_employment (set in 00_master.do)
*************************************************************************************/
gen unemp = (jbstat == 3)
replace unemp = . if les_c3 == .
replace unemp = . if dag < $age_seek_employment
replace unemp = 0  if les_c4 == 4 & unemp == 1 //impose consistency with retirement
lab var unemp "Unemployed dummy"


/**************************************************************************************
* ECONOMIC BENEFITS (econ_benefits)
* Requires: fihhmnsben_dv, benefits_uc, benefits_lb from 01_merge.do
*************************************************************************************/
gen econ_benefits = .
replace econ_benefits = 1 if fihhmnsben_dv > 0 & fihhmnsben_dv != .
replace econ_benefits = 0 if fihhmnsben_dv == 0
label var econ_benefits "Household income includes any benefits"

replace econ_benefits = 1 if benefits_uc == 1

gen econ_benefits_nonuc = econ_benefits
replace econ_benefits_nonuc = 0 if benefits_uc == 1
label var econ_benefits_nonuc "Household income includes non-UC benefits"

gen econ_benefits_uc = econ_benefits
replace econ_benefits_uc = 0 if benefits_uc == 0
label var econ_benefits_uc "Household income includes UC benefits"

gen econ_benefits_lb = benefits_lb
replace econ_benefits_lb = 0 if econ_benefits_uc == 1
label var econ_benefits_lb "Household income includes Legacy Benefits"


/**************************************************************************************
* ECONOMIC DISTRESS / FINANCIAL DISTRESS (econ_dist)
* Based on finnow: 1-3 = not in distress, 4-5 = in distress
* Missing values kept as missing (no imputation)
*************************************************************************************/
recode finnow (1 2 3 = 0) (4 5 = 1) (else = .), gen(econ_dist)
lab var econ_dist "Financial distress (binary)"


/**************************************************************************************
* RECODE MISSINGS
*************************************************************************************/
foreach var in dnc home_owner econ_benefits econ_benefits_nonuc econ_benefits_uc econ_benefits_lb econ_dist yhhnb dhhtp_c4 unemp {
	qui recode `var' (-9/-1 = .)
}


/**************************************************************************************
* RENAME swv -> wave (to match R pipeline naming convention)
*************************************************************************************/
rename swv wave


/**************************************************************************************
* SAVE
*************************************************************************************/
save "$dir_out/ukhls_pooled_all_obs_02.dta", replace
cap log close
