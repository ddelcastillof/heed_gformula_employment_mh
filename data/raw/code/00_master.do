***************************************************************************************
* PROJECT:              Health Equity and its Economic Determinants
* DO-FILE NAME:         00_master.do
* DESCRIPTION:          Main do-file to merge files
***************************************************************************************
* COUNTRY:              UK
* DATA:         	    UKHLS EUL version - UKDA-6614-stata [to wave o]
*						WAS EUL version - UKDA-7215-stata [to wave 7]
* AUTHORS: 				Darwin del Castillo
* LAST UPDATE:          17 Mar 2026
***************************************************************************************
/*
* Stata packages to install 
ssc install fre
ssc install tsspell 
ssc install carryforward 
ssc install outreg2
ssc install filelist
ssc install gologit2
ssc install estout
*/

clear all
set more off
set maxvar 30000
set matsize 1000

/**************************************************************************************
* DEFINE DIRECTORIES
*************************************************************************************/

* Working directory
global dir_work "/Users/darwin.delcastillofernandez/Documents/GitHub/heed_gformula_employment_mh/.worktrees/fix-import-data"

* Directory containing do files
global dir_do "${dir_work}/data/raw/code"

* Directory containing log files
global dir_log "${dir_work}/data/raw/log"

* Directory containing output files
global dir_out "${dir_work}/data/output"

* Directory containing UKHLS data
global dir_ukhls_data "${dir_work}/data/raw/ukhls"

* Directory containing graphs for sanity checks
global dir_graphs "${dir_work}/data/raw/graphs"


/**************************************************************************************
* DEFINE OTHER GLOBAL VARIABLES
*************************************************************************************/

/*
* UKHLS Wave letters: we start from 2010 - wave 2 - b
first initial population starts in 2011 (as some variables are using data from the previous wave)
wave 1  a 2009-2011 (last year is small)
wave 2  b 2010-2012
wave 3  c 2011-2013
wave 4  d 2012-2014
wave 5  e 2013-2015
wave 6  f 2014-2016
wave 7  g 2015-2017
wave 8  h 2016-2018
wave 9  i 2017-2019
wave 10 j 2018-2020
wave 11 k 2019-2021
wave 12 l 2020-2022
wave 13 m 2021-2023
wave 14 n 2022-2024
wave 15 o 2023-2025
*/

global UKHLS_panel_waves "a b c d e f g h i j k l m n o"
global UKHLS_panel_waves_numbers "1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" //ukhls_waves_numbers
global UKHLS_waves_prefixed "a_ b_ c_ d_ e_ f_ g_ h_ i_ j_ k_ l_ m_ n_ o_"

* Define threshold ages
/*
The thresholds defined below ensure consistency between the assumptions 
applied in the simulation and the structure of the initial population data. 
They specify the ages at which certain life-course transitions are permitted 
or enforced within the model. These limits reflect both modelling conventions 
and empirical considerations drawn from observed data.
*/
 	
* Age become an adult in various dimensions	
global age_becomes_responsible 18 

global age_seek_employment 16 
	
global age_leave_school 16 

global age_form_partnership 18 

global age_have_child_min 18 	

global age_leave_parental_home 18

	
* Age can/must/cannot make various transitions 	
global age_max_dep_child 17     

global age_adult 18 

global age_can_retire 50

global age_force_retire 75             

global age_force_leave_spell1_edu 30   

global age_have_child_max 49  	// allow this to be led by the data  	

cap log close
log using "${dir_log}/00_master.log", replace

/**************************************************************************************
* ROUTE TO WORKER FILES 
**************************************************************************************/
* process UKHLS data
do "${dir_do}/01_merge.do"

/* prepare simulated and observed data */
do "${dir_do}/02_prepare_variables.do"
