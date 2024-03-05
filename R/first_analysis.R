library(gfoRmula)
source("custom_helpers.R")

# stata code in -----------------------------------------------------------

#' use "data"
#' 
#' 
#' gformula sf12mcs_dv sex_dv age_dv age_sq Lage_dv hiqual_dv Lsf12mcs_dv home_owner_base mastat_dv_base dnc_base gor_re_base sf12pcs_dv Lsf12pcs_dv log_income Llog_income econ_emp_bin Lecon_emp_bin econ_dist Lecon_dist intdaty_dv intdaty_lag pidp wave, 
#' 
#' outcome(sf12mcs_dv) ///
#' 
#' commands(sf12mcs_dv :regress, econ_emp_bin: logit, econ_dist: logit, log_income: regress)  ///
#' 
#' equations(
#'    sf12mcs_dv: i.sex_dv age_dv age_sq i.hiqual_dv Lsf12mcs_dv Lsf12pcs_dv i.home_owner_base i.mastat_dv_base i.dnc_base i.gor_re_base i.Lecon_emp_bin i.Lecon_dist Llog_income i.intdaty_dv, ///
#'    econ_emp_bin: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base i.mastat_dv_base i.dnc_base i.gor_re_base Llog_income sf12pcs_dv Lsf12mcs_dv i.Lecon_emp_bin i.intdaty_dv, ///
#'    econ_dist: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base i.mastat_dv_base i.dnc_base i.gor_re_base Lsf12pcs_dv log_income i.econ_emp_bin i.intdaty_dv, ///
#'    log_income: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base i.mastat_dv_base i.dnc_base i.gor_re_base Lsf12pcs_dv Llog_income i.econ_emp_bin Lsf12mcs_dv i.intdaty_dv) ///
#' 
#' idvar(pidp)
#' tvar(wave)
#' 
#' varyingcovariates(econ_dist log_income)
#' intvars(econ_emp_bin) ///
#' 
#' interventions(
#'    econ_emp_bin=0 if wave<=10, /// 0-00-0
#'    econ_emp_bin=0 if wave<=8 \ econ_emp_bin=1 if wave>=9 ,  /// 0-01-1
#'    econ_emp_bin=0 if wave<=7 \ econ_emp_bin=1 if wave=8 \ econ_emp_bin=0 if wave>=9, /// 0-10-0
#'    econ_emp_bin=1 if wave<=10)   /// 1-11-1
#' 
#' pooled 
#' 
#' ### continuous_eof
#' eofu 
#' 
#' ### basecovs
#' fixed(hiqual_dv sex_dv home_owner_base mastat_dv_base dnc_base gor_re_base) 
#' 
#' laggedvars(Lsf12mcs_dv Lsf12pcs_dv Lage_dv Llog_income Lecon_emp_bin Lecon_dist intdaty_lag) ///
#' lagrules(
#'   Lsf12mcs_dv: sf12mcs_dv 1,
#'   Lsf12pcs_dv: sf12pcs_dv 1, 
#'   Lage_dv: age_dv 1, 
#'   Llog_income: log_income 1, 
#'   Lecon_emp_bin: econ_emp_bin 1, 
#'   Lecon_dist: econ_dist 1, 
#'   intdaty_lag: intdaty_dv 1) ///
#'   
#'   derived(intdaty_dv age_sq age_dv) 
#'   derrules(
#'     intdaty_dv: intdaty_lag+1, 
#'     age_sq: age_dv*age_dv, 
#'     age_dv: Lage_dv +1) ///
#'   seed(89) 
#'   samples(10) 
#'   saving(mh_gform_MCsims_10_2)  replace	

outcome_type <- "continuous_eof"

covnames <-
  c(
    "econ_emp_bin",
    "econ_dist",
    "log_income",
    "sf12mcs_dv",
    "sf12pcs_dv",
    "age_dv"
  )

outcome_name <- "sf12pcs_dv"
intvars <- c("econ_emp_bin")

covtypes <- c(
  "binary", # econ_emp_bin
  "binary", # econ_dist
  "normal", # log_income
  "normal", # sf12mcs_dv
  "normal", # sf12pcs_dv
  "normal" # age_dv
)

basecovs <- c(
  "age_dv_base",
  "hiqual_dv_base",
  "sex_dv_base",
  "home_owner_base",
  "mastat_dv_base",
  "dnc_base",
  "gor_dv_base"
)

# i.sex_dv age_dv age_sq i.hiqual_dv Lsf12mcs_dv Lsf12pcs_dv i.home_owner_base
# i.mastat_dv_base i.dnc_base i.gor_re_base i.Lecon_emp_bin i.Lecon_dist
# Llog_income i.intdaty_dv

ymodel <- sf12pcs_dv ~  sex_dv_base + stats::poly(age_dv_incr, 2) + hiqual_dv_base +
  lag1_sf12mcs_dv + lag1_sf12pcs_dv + home_owner_base + mastat_dv_base + dnc_base + gor_dv_base + 
  lag1_log_income + lag1_econ_emp_bin + lag1_econ_dist


covparams <- list(
  covmodels = c(
#    econ_emp_bin: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base
#    i.mastat_dv_base i.dnc_base i.gor_re_base Llog_income sf12pcs_dv
#    Lsf12mcs_dv i.Lecon_emp_bin i.intdaty_dv, ///
    econ_emp_bin ~ sex_dv_base + hiqual_dv_base + 
      age_dv_incr + home_owner_base +
      mastat_dv_base + dnc_base + gor_dv_base +
      lag1_log_income + lag1_sf12pcs_dv + lag1_sf12mcs_dv +
      lag1_econ_emp_bin,
#    econ_dist: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base i.mastat_dv_base
#    i.dnc_base i.gor_re_base Lsf12pcs_dv log_income i.econ_emp_bin
#    i.intdaty_dv, ///
    econ_dist ~ sex_dv_base + hiqual_dv_base + 
      age_dv_incr + home_owner_base + mastat_dv_base +
      dnc_base + gor_dv_base + lag1_sf12pcs_dv + log_income +
      econ_emp_bin,
#    log_income: i.sex_dv i.hiqual_dv Lage_dv i.home_owner_base i.mastat_dv_base
#    i.dnc_base i.gor_re_base Lsf12pcs_dv Llog_income i.econ_emp_bin Lsf12mcs_dv
#    i.intdaty_dv) ///
    log_income ~ sex_dv_base + hiqual_dv_base + 
      age_dv_incr + home_owner_base + mastat_dv_base +
      dnc_base + gor_dv_base + lag1_sf12pcs_dv + lag1_log_income + econ_emp_bin + lag1_sf12mcs_dv,
    sf12mcs_dv ~ sex_dv_base + stats::poly(age_dv_incr, 2) + hiqual_dv_base +
      lag1_sf12mcs_dv + lag1_sf12pcs_dv + home_owner_base + mastat_dv_base + dnc_base + gor_dv_base + 
      lag1_log_income + lag1_econ_emp_bin + lag1_econ_dist,
    ymodel,
    age_dv ~ 1
  )
)

# laggedvars(Lsf12mcs_dv Lsf12pcs_dv Lage_dv Llog_income Lecon_emp_bin Lecon_dist intdaty_lag)

lagged_variables <- c(
  "sf12mcs_dv",
  "sf12pcs_dv",
  "log_income",
  "econ_emp_bin",
  "econ_dist"
)

increment_variables <- c(
  "age_dv"
)

histories <- c(lagged, increment)
histvars <- list(lagged_variables, increment_variables)

nsimul <- 100 # Monte Carlo sample size
ncores <- parallel::detectCores() - 1


intvars <- as.list(rep("econ_emp_bin", 8))

interventions <- tibble(econ_emp_bin1 = 0:1,
                        econ_emp_bin2 = 0:1,
                        econ_emp_bin3 = 0:1) |> 
  expand.grid() |> 
  t() |>
  rbind(matrix(0, nrow = 2, ncol = 8), x = _) |> 
  as_tibble() |>
  unclass() |>
  map(~list(c(static, .x)))

int_description <-
  map(interventions, ~paste(.x[[1]][-1], collapse = "-") |> paste0("econ_emp_bin:", x = _))

time_points <- length(unique(data$t0))

gform_cont_eof <- gformula(obs_data = expanded_data,
                           outcome_type = outcome_type,
                           id = "pidp",
                           time_name = "t0",
                           time_points = time_points,
                           covnames = covnames,
                           outcome_name = outcome_name,
                           covtypes = covtypes,
                           covparams = covparams,
                           # covpredict_custom = covpredict_custom,
                           ymodel = ymodel,
                           
                           intvars = intvars,
                           interventions = interventions,
                           int_descript = int_description,
                           
                           histories = histories,
                           histvars = histvars,
                           basecovs = basecovs,
                           seed = 1234,
                           parallel = FALSE,
                           nsamples = 200) # Number of bootstrap samples

