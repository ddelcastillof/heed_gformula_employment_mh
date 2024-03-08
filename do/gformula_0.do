	use "data"
	
	
	gformula sf12mcs_dv sex_dv age_dv age_sq Lage_dv hiqual_dv Lsf12mcs_dv home_owner_base mastat_dv_base dnc_base gor_re_base sf12pcs_dv Lsf12pcs_dv log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave, outcome(sf12mcs_dv) ///
	commands(sf12mcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress)  ///
	equations(sf12mcs_dv: i.sex_dv age_dv age_sq i.hiqual_dv Lsf12mcs_dv Lsf12pcs_dv i.home_owner_base i.mastat_dv_base i.dnc_base i.gor_re_base i.Lecon_emp_bin i.Lecon_dist Llog_income i.intdaty_dv, ///
	econ_emp_bin: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base i.mastat_dv_base i.dnc_base i.gor_re_base Llog_income sf12pcs_dv Lsf12mcs_dv i.Lecon_emp_bin i.intdaty_dv, ///
	econ_dist: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base i.mastat_dv_base i.dnc_base i.gor_re_base Lsf12pcs_dv log_income i.econ_emp_bin i.intdaty_dv, ///
	log_income: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base i.mastat_dv_base i.dnc_base i.gor_re_base Lsf12pcs_dv Llog_income i.econ_emp_bin Lsf12mcs_dv i.intdaty_dv) ///
	idvar(pidp) tvar(wave) varyingcovariates(econ_dist log_income) intvars(econ_emp_bin) ///
	interventions( ///
	econ_emp_bin=0 if wave<=10, /// 0-00-0
	econ_emp_bin=0 if wave<=8 \ econ_emp_bin=1 if wave>=9 ,  /// 0-01-1
	econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>=9, /// 0-10-0
	econ_emp_bin=1 if wave<=10)   /// 1-11-1
	pooled eofu fixed(hiqual_dv sex_dv home_owner_base mastat_dv_base dnc_base gor_re_base) laggedvars(Lsf12mcs_dv Lsf12pcs_dv Lage_dv Llog_income Lecon_emp_bin Lecon_dist intdaty_lag) ///
	lagrules(Lsf12mcs_dv: sf12mcs_dv 1, Lsf12pcs_dv: sf12pcs_dv 1, Lage_dv: age_dv 1, Llog_income: log_income 1, Lecon_emp_bin: econ_emp_bin 1, Lecon_dist: econ_dist 1, intdaty_lag: intdaty_dv 1) ///
	derived(intdaty_dv age_sq age_dv) derrules(intdaty_dv: intdaty_lag+1, age_sq: age_dv*age_dv, age_dv: Lage_dv +1) ///
	seed(89) samples(10) saving (mh_gform_MCsims_10_2)  replace	