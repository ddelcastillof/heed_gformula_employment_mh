*version 11
clear
clear matrix
set seed 10330
set obs 2000
gen u=uniform()<0.4
qui gen l0=uniform()<exp(0.4-0.3*u)/(1+exp(0.4-0.3*u))
qui gen a0=uniform()<exp(0.65-0.5*l0)/(1+exp(0.65-0.5*l0))
qui gen l1=uniform()<exp(0.25+0.3*a0+0.2*l0-0.2*u-0.05*a0*l0)/ ///
(1+exp(0.25+0.3*a0+0.2*l0-0.2*u-0.05*a0*l0))
qui gen a1=uniform()<exp(0.4+0.5*a0-0.3*l1-0.4*a0*l1)/ ///
(1+exp(0.4+0.5*a0-0.3*l1-0.4*a0*l1))
qui gen l2=uniform()<exp(0.25+0.3*a1+0.2*l1-0.2*u-0.05*a1*l1)/ ///
(1+exp(0.25+0.3*a1+0.2*l1-0.2*u-0.05*a1*l1))
qui gen a2=uniform()<exp(0.4+0.5*a1-0.3*l2-0.4*a1*l2)/ ///
(1+exp(0.4+0.5*a1-0.3*l2-0.4*a1*l2))
qui gen l3=uniform()<exp(0.25+0.3*a2+0.2*l2-0.2*u-0.05*a2*l2)/ ///
(1+exp(0.25+0.3*a2+0.2*l2-0.2*u-0.05*a2*l2))
qui gen a3=uniform()<exp(0.4+0.5*a2-0.3*l3-0.4*a2*l3)/ ///
(1+exp(0.4+0.5*a2-0.3*l3-0.4*a2*l3))
qui gen y=2.5-0.5*(a0+a1+a2+a3)-u+0.2*invnormal(uniform())
*save sim3
gen i=_n
rename y y4
gen cuma0=a0
gen cuma1=a0+a1
gen cuma2=a0+a1+a2
gen cuma3=a0+a1+a2+a3
reshape long l a y cuma, i(i) j(t)
gen la=l*a
sort i t
by i: gen l_lag=l[_n-1]
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

replace a_lag=. if t==4 
replace a_lag2=. if t==4 
replace a_lag3=. if t==4 
replace a_lag4=. if t==4 
replace l_lag=. if t==4 
replace l_lag2=. if t==4 
replace l_lag3=. if t==4 
replace l_lag4=. if t==4 



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
a=0 if t==3 \ a=1 if (t==0 | t==1 | t==2), a=1 if t<4) ///
eofu ///
derived(la l_alag cuma) ///
derrules(la:l*a, l_alag:l*a_lag, cuma:cuma_lag+a) ///
lag(l_lag a_lag la_lag a_lag2 a_lag3 a_lag4 l_lag2 l_lag3 l_lag4 cuma_lag) ///
lagrules(l_lag:l 1, l_lag2:l 2, l_lag3:l 3, ///
l_lag4:l 4, a_lag:a 1, a_lag2:a 2, a_lag3:a 3, a_lag4:a 4, ///
la_lag:la 1, cuma_lag:cuma 1) ///
msm(regress y cuma_lag) ///
	seed(79) samples(2) simulations(2)  
