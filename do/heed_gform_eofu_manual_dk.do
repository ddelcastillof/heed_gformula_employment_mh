* Form simulated dataset called sim3
clear
clear matrix
set seed 10330
set obs 2000
gen u=uniform()<0.4 // random error
qui gen l0=uniform()<exp(0.4-0.3*u)/(1+exp(0.4-0.3*u)) 							// binary confounder at baseline. Affected only by random component.
qui gen a0=uniform()<exp(0.65-0.5*l0)/(1+exp(0.65-0.5*l0)) 						// Treatment at baseline. Affected by baseline confounder.
qui gen l1=uniform()<exp(0.25+0.3*a0+0.2*l0-0.2*u-0.05*a0*l0)/ ///
(1+exp(0.25+0.3*a0+0.2*l0-0.2*u-0.05*a0*l0))									// binary confounder at T1. Affected by lagged treatment and confounder, plus random component
qui gen a1=uniform()<exp(0.4+0.5*a0-0.3*l1-0.4*a0*l1)/ ///
(1+exp(0.4+0.5*a0-0.3*l1-0.4*a0*l1))											// treatment at T1. Affected by lagged treatment and current confounder.
qui gen l2=uniform()<exp(0.25+0.3*a1+0.2*l1-0.2*u-0.05*a1*l1)/ ///
(1+exp(0.25+0.3*a1+0.2*l1-0.2*u-0.05*a1*l1))									// binary confounder at T2. Affected by lagged treatment and confounder, plus random component
qui gen a2=uniform()<exp(0.4+0.5*a1-0.3*l2-0.4*a1*l2)/ ///
(1+exp(0.4+0.5*a1-0.3*l2-0.4*a1*l2))  											// treatment at T2. Affected by lagged treatment and current confounder.
qui gen l3=uniform()<exp(0.25+0.3*a2+0.2*l2-0.2*u-0.05*a2*l2)/ ///
(1+exp(0.25+0.3*a2+0.2*l2-0.2*u-0.05*a2*l2)) 									// binary confounder at T3. Affected by lagged treatment and confounder, plus random component
qui gen a3=uniform()<exp(0.4+0.5*a2-0.3*l3-0.4*a2*l3)/ ///
(1+exp(0.4+0.5*a2-0.3*l3-0.4*a2*l3))											// treatment at T3. Affected by lagged treatment and current confounder.
qui gen y=2.5-0.5*(a0+a1+a2+a3)-u+0.2*invnormal(uniform())						// continuous outcome at end of exposure. Affected by a lags of treatment, plus random component
save "T:\projects\HEED\Data\heed_gform_example.dta", replace





* Manually apply the g-computation formula for the 16 hypothetical interventions
clear
clear matrix
set mem 400m
capture program drop gcomp_sim3
program define gcomp_sim3, rclass
capture drop i-cuma
qui gen i=_n 																	// generate initial person id
qui set obs 34000 																// creat lines of missing observations for sixteen interventions plus baseline
qui replace i=i[_n-2000] if i==. 												// repeat person ids to further interevntion rows
qui gen int_no=(_n-i)/2000														// generate intervention number
qui replace l0=l0[_n-2000] if l0==. 											// populate other interventions with baseline confounders
qui replace a0=0 if int_no==1 | int_no==2 | int_no==3 | int_no==4 ///
| int_no==5 | int_no==6 | int_no==7 | int_no==8									// generate intervention patterns
qui replace a0=1 if int_no==9 | int_no==10 | int_no==11 | int_no==12 ///
| int_no==13 | int_no==14 | int_no==15 | int_no==16
qui replace a1=0 if int_no==1 | int_no==2 | int_no==3 | int_no==4 | ///
int_no==9 | int_no==10 | int_no==11 | int_no==12
qui replace a1=1 if int_no==5 | int_no==6 | int_no==7 | int_no==8 | ///
int_no==13 | int_no==14 | int_no==15 | int_no==16
qui replace a2=0 if int_no==1 | int_no==2 | int_no==5 | int_no==6 | ///
int_no==9 | int_no==10 | int_no==13 | int_no==14
qui replace a2=1 if int_no==3 | int_no==4 | int_no==7 | int_no==8 | ///
int_no==11 | int_no==12 | int_no==15 | int_no==16
qui replace a3=0 if int_no==1 | int_no==3 | int_no==5 | int_no==7 | ///
int_no==9 | int_no==11 | int_no==13 | int_no==15
qui replace a3=1 if int_no==2 | int_no==4 | int_no==6 | int_no==8 | ///
int_no==10 | int_no==12 | int_no==14 | int_no==16
qui gen l0a0=l0*a0																// generate interaction between baseline treatment and confounder
qui logit l1 l0 a0 l0a0															// prediction model for confounder value at T1 given lagged treatment and confounder, including interaction
qui predict pl1																	// predict confounder value at T1
qui replace l1=uniform()<pl1 if l1==.
qui gen l1a1=l1*a1																// generate treatment-confounder interaction at T1
qui logit l2 l1 a1 l1a1															// prediction model for confounder value at T2 given lagged treatment and confounder, including interaction
qui predict pl2																	// predict confounder value at T2
qui replace l2=uniform()<pl2 if l2==.
qui gen l2a2=l2*a2																// generate treatment-confounder interaction at T2
qui logit l3 l2 a2 l2a2															// prediction model for confounder value at T3 given lagged treatment and confounder, including interaction
qui predict pl3																	// predict confounder value at T3
qui replace l3=uniform()<pl3 if l3==.
qui regress y a0 a1 a2 a3 l0 l1 l2 l3											// outcome prediction model given all lags of treatment and confounders
qui predict py																	// predict outcome
qui replace y=py if y==.														// replace outcome with prediction when missing
qui regress y i.int_no if int_no>0												// estimate potential mean outcome for each intervention
return scalar po1=_b[_cons]
return scalar po2=_b[_cons]+_b[2.int_no]
return scalar po3=_b[_cons]+_b[3.int_no]
return scalar po4=_b[_cons]+_b[4.int_no]
return scalar po5=_b[_cons]+_b[5.int_no]
return scalar po6=_b[_cons]+_b[6.int_no]
return scalar po7=_b[_cons]+_b[7.int_no]
return scalar po8=_b[_cons]+_b[8.int_no]
return scalar po9=_b[_cons]+_b[9.int_no]
return scalar po10=_b[_cons]+_b[10.int_no]
return scalar po11=_b[_cons]+_b[11.int_no]
return scalar po12=_b[_cons]+_b[12.int_no]
return scalar po13=_b[_cons]+_b[13.int_no]
return scalar po14=_b[_cons]+_b[14.int_no]
return scalar po15=_b[_cons]+_b[15.int_no]
return scalar po16=_b[_cons]+_b[16.int_no]
qui gen cuma=a0+a1+a2+a3														// calculate cumulative exposure
qui regress y cuma if int_no>0													// estimate marginal structural model of cumulative exposure post baseline
return scalar gamma_int=_b[_cons]
return scalar gamma_sum=_b[cuma]
qui drop if int_no!=0															// drop baseline observation
end
use "T:\projects\HEED\Data\heed_gform_example.dta", clear
bootstrap r(po1) r(po2) r(po3) r(po4) r(po5) r(po6) r(po7) r(po8) ///
r(po9) r(po10) r(po11) r(po12) r(po13) r(po14) r(po15) r(po16) ///
r(gamma_int) r(gamma_sum), reps(1000): gcomp_sim3								// bootstrap by potential outcome



* Compare to use of gformula command
use "T:\projects\HEED\Data\heed_gform_example.dta", clear
gen i=_n																		// generate person id
rename y y4																		// rename outcome to indicate end of follow up
gen cuma0=a0																	// generate cumulative exposure
gen cuma1=a0+a1
gen cuma2=a0+a1+a2
gen cuma3=a0+a1+a2+a3
reshape long l a y cuma, i(i) j(t)												// reshape to long format
gen la=l*a																		// generate treatment-confounder interaction
sort i t
by i: gen l_lag=l[_n-1]															// form lagged variables
by i: gen a_lag=a[_n-1]
by i: gen la_lag=la[_n-1]
by i: gen l_lag2=l[_n-2]
by i: gen a_lag2=a[_n-2]
by i: gen l_lag3=l[_n-3]
by i: gen a_lag3=a[_n-3]
by i: gen l_lag4=l[_n-4]
by i: gen a_lag4=a[_n-4]
by i: gen cuma_lag=cuma[_n-1]
replace l_lag=0 if t==0
replace a_lag=0 if t==0
replace l_lag2=0 if t<2
replace a_lag2=0 if t<2
replace l_lag3=0 if t<3
replace a_lag3=0 if t<3
replace l_lag4=0 if t<4
replace a_lag4=0 if t<4
replace la_lag=0 if t==0
replace cuma_lag=0 if t==0
gen l_alag=l*a_lag

gformula i t y l a la cuma cuma_lag l_lag a_lag la_lag l_alag a_lag2 a_lag3 a_lag4 l_lag2 l_lag3 l_lag4, ///
out(y) ///
com(y:regress, l:logit, a:logit) ///
eq(y:a_lag a_lag2 a_lag3 a_lag4 l_lag l_lag2 l_lag3 l_lag4, ///
l:a_lag l_lag la_lag, ///
a:l a_lag l_alag) ///
i(i) ///
t(t) ///
var(l) ///
intvars(a) ///
interventions(a=0 if t<4, a=0 if t<3 \ a=1 if t==3, ///
a=0 if (t<2 | t==3) \ a=1 if t==2, a=0 if t<2 \ a=1 if (t==2 | t==3), ///
a=0 if (t==0 | t==2 | t==3) \ a=1 if t==1, a=0 if (t==0 | t==2) \ ///
a=1 if (t==1 | t==3), a=0 if (t==0 | t==3) \ a=1 if (t==1 | t==2), ///
a=0 if t==0 \ a=1 if (t==1 | t==2 | t==3), a=0 if (t==1 | t==2 | t==3) ///
\ a=1 if t==0, a=0 if (t==1 | t==2) \ a=1 if (t==0 | t==3), ///
a=0 if (t==1 | t==3) \ a=1 if (t==0 | t==2), a=0 if t==1 \ ///
a=1 if (t==0 | t==2 | t==3), a=0 if (t==2 | t==3) \ ///
a=1 if (t==0 | t==1), a=0 if t==2 \ a=1 if (t==0 | t==1 | t==3), ///
a=0 if t==3 \ a=1 if (t==0 | t==1 | t==2), a=1 if t<4) ///
eofu ///
derived(la l_alag cuma) ///
derrules(la:l*a, l_alag:l*a_lag, cuma:cuma_lag+a) ///
lag(l_lag a_lag la_lag a_lag2 a_lag3 a_lag4 l_lag2 l_lag3 l_lag4 cuma_lag) ///
lagrules(l_lag:l 1, l_lag2:l 2, l_lag3:l 3, ///
l_lag4:l 4, a_lag:a 1, a_lag2:a 2, a_lag3:a 3, a_lag4:a 4, ///
la_lag:la 1, cuma_lag:cuma 1) ///
msm(regress y cuma_lag) 
* Outcome equation matches y a0 a1 a2 a3 l0 l1 l2 l3 above.
* Confounder equation matches period-specific equations, e.g. l1 l0 a0 l0a0, above.
* Treatment equation not needed in manual calculation as defined by those of interest