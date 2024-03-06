library(haven)
library(tidyverse)
library(data.table)


# TODO add intdaty_lag?

data_in <- read_stata("T:/projects/HEED/Data/USoc prepared data/Test_Harry14.dta")

interim_new_vars <- data_in |>
  select(-age_sq) |>
  #  gen age_sample=1 if inrange(age_dv,25,64)  /*Defining sample in terms of age*/
  #  keep if age_sample==1    /*Deleting all people younger than 25 (24 and younger) and older than 65 (65 included) */
  filter(age_dv >= 25, age_dv <= 65) |> 
  mutate(
    across(c(pidp, ppid, hidp, wave, pns1pid, pns2pid),
           as.integer),
    across(where(haven::is.labelled),
           haven::zap_labels),
    econ_emp_bin = case_when(econ_emp == 1 ~ 0, econ_emp == 3 ~ 1))

tidied_data <- interim_new_vars |> 
  mutate(
    # *Generating our exposure (Employment status)
    les_c4 = case_when(
      jbstat %in% 1:2 ~ "Employed or self-employed",
      jbstat == 4 ~ "Retired",
      jbstat == 7 ~ "Student",
      jbstat %in% c(3, 5:6, 8:11, 97) ~ "Not employed"
    ),
    # * Generate long-term sick, retired, and in education dummies
    econ_ltsick = jbstat == 8,
    econ_retire = les_c4 == "Retired",
    econ_student = les_c4 == "Student",
    # * Clean job status variable
    econ_emp = les_c4,
    econ_emp = if_else(econ_ltsick, "Long-term sick", econ_emp),
    econ_emp = if_else(econ_emp == "Not employed", "Not employed (at risk of work)", econ_emp),
    # *Creating employment status as binary*
    # label define econ_emp_bin 0 "Employed" 1 "Non-employed"
    econ_emp_bin = case_when(econ_emp == "Employed or self-employed" ~ 0, econ_emp == "Not employed (at risk of work)" ~ 1),
    # *Recode ethnicity into white/nonwhite
    wnw_race = case_when(racel_dv %in% 1:4 ~ 0, racel_dv %in% 5:97 ~  1, .default = NA)
  ) |> 
  # *keep only those who are either employed, not employed or have missing values
  filter(econ_emp %in% c("Not employed (at risk of work)", "Employed or self-employed") | is.na(econ_emp))


#' Lagging not needed
#' *Creating one-wave lags for the variable we would need later on
#' gen Lecon_emp_bin= L.econ_emp_bin
#' gen Lecon_emp=L.econ_emp /*Economic activity (0:Employed, 1: Unemployed*/
#' gen intdaty_lag=L.intdaty_dv /*Year minus 2000*/
#' gen Lhome_owner=L.home_owner /*homeowner (0: Renter, 1: Owner */
#' gen Lmastat_dv=L.mastat_dv /*De facto marital status (1: Partnered, 2: Single and never married, 3: Previously partnered) */
#' gen Ldnc=L.dnc  /*Number of depedent children over 18 (0, 1, 2, 3, 4 or more)*/
#' gen Lgor_dv = L.gor_dv /*Government office region (12 values)*/
#' gen Lghqcase4 = L.ghqcase4 /*GHQ caseness (0: No, 1: Yes)*/
#' gen Dlog_income = D.log_income /*log of equivalised household income (difference between two consecutive waves)*/
#' gen Lecon_dist = L.econ_dist /*economic distress (0: No, 1: Yes)*/
#' gen Llog_income = L.log_income /*log of equivalised household income*/
#' gen Lsf12pcs_dv = L.sf12pcs_dv /*SF-12 Physical Component Summary*/
#' gen Lsf12mcs_dv = L.sf12mcs_dv /*SF-12 Mental Component Summary*/
#' gen Lage_dv=L.age_dv
#' 

# Creating baselines
# should hiqual be included here?
# Expanding and only including wave 6 onwards

expanded_data <- tidied_data |>
  select(# id and time vars
    pidp,
    wave,
    # time-varying vars
    sf12mcs_dv,
    sf12pcs_dv,
    log_income,
    econ_emp_bin,
    econ_dist,
    age_dv,
    sex_dv,
    gor_dv,
    mastat_dv,
    home_owner,
    dnc,
    hiqual_dv) |>
  full_join(tibble(wave = 1:10) |> 
              expand_grid(tibble(pidp = unique(tidied_data$pidp))),
            by = join_by(wave, pidp)) |> 
  arrange(pidp, wave) |> 
  group_by(pidp) |> 
  fill(everything()) |> 
  ungroup()
  

pop_data <- expanded_data |> 
  select(pidp, wave, age_dv, sex_dv, gor_dv, mastat_dv, home_owner, dnc, hiqual_dv) |> 
  filter(wave == 6, !is.na(age_dv)) |>
  select(-wave) |> 
  rename_with(~paste0(.x, "_base"), -c(pidp)) |> 
  expand_grid(
    tibble(
      wave = 1:10,
      t0 = -5:4
    )
  ) |> 
  left_join(expanded_data, by = join_by(pidp, wave)) |>
  select(
    # id and time vars
    pidp,
    wave,
    t0,
    # time-varying vars
    sf12mcs_dv,
    sf12pcs_dv,
    age_dv,
    log_income,
    econ_emp_bin,
    econ_dist,
    # base vars
    age_dv_base,
    sex_dv_base,
    gor_dv_base,
    mastat_dv_base,
    home_owner_base,
    dnc_base,
    hiqual_dv_base,
  ) |> 
  as.data.table()


# Experimental - leaving out Harry's additional correctors
# data2 <- interim_new_vars |> 
#   select(pidp, wave, age_dv, sex_dv, gor_dv, mastat_dv, home_owner, dnc, hiqual_dv) |> 
#   filter(wave == 6, !is.na(age_dv)) |>
#   select(-wave) |> 
#   rename_with(~paste0(.x, "_base"), -c(pidp)) |> 
#   expand_grid(
#     tibble(
#       wave = 6:10,
#       t0 = 0:4
#     )
#   ) |> 
#   left_join(interim_new_vars, by = join_by(pidp, wave, t0)) |>
#   select(
#     # id and time vars
#     pidp,
#     wave,
#     t0,
#     # time-varying vars
#     sf12mcs_dv,
#     sf12pcs_dv,
#     age_dv,
#     log_income,
#     econ_emp_bin,
#     econ_dist,
#     # base vars
#     age_dv_base,
#     sex_dv_base,
#     gor_dv_base,
#     mastat_dv_base,
#     home_owner_base,
#     dnc_base,
#     hiqual_dv_base,
#   )
