import_data <- function() {
  require(tidyverse)
  
  data_file <- here::here("data", "pop_data.rds")
  
  if (file.exists(data_file)) {
    pop_data <- readRDS(data_file)
    message(data_file, " already exists, reading in...")
  } else {
    message("Creating ", data_file)
    
  data_in <-
    haven::read_stata("T:/projects/HEED/Data/USoc prepared data/Test_Harry14.dta")
  
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
      econ_emp_bin = case_when(econ_emp == 1 ~ 0, econ_emp == 3 ~ 1)
    )
  
  # interim_new_vars <- fst::read_fst("T:/projects/HEED/Data/USoc prepared data/test_data.fst")
  
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
      econ_emp = if_else(
        econ_emp == "Not employed",
        "Not employed (at risk of work)",
        econ_emp
      ),
      # *Creating employment status as binary*
      # label define econ_emp_bin 0 "Employed" 1 "Non-employed"
      econ_emp_bin = case_when(
        econ_emp == "Employed or self-employed" ~ 0,
        econ_emp == "Not employed (at risk of work)" ~ 1
      ),
      # *Recode ethnicity into white/nonwhite
      wnw_race = case_when(racel_dv %in% 1:4 ~ 0, racel_dv %in% 5:97 ~  1, .default = NA)
    ) |>
    # *keep only those who are either employed, not employed or have missing values
    filter(
      econ_emp %in% c("Not employed (at risk of work)", "Employed or self-employed") |
        is.na(econ_emp)
    )
  
  
  expanded_data <- tidied_data |>
    select(
      # id and time vars
      pidp,
      wave,
      # time-varying vars
      sf12mcs_dv,
      sf12pcs_dv,
      log_income,
      econ_emp_bin,
      econ_dist,
      age_dv,
      wnw_race,
      sex_dv,
      gor_dv,
      mastat_dv,
      home_owner,
      dnc,
      hiqual_dv
    ) |>
    full_join(tibble(wave = 1:10) |>
                expand_grid(tibble(pidp = unique(tidied_data$pidp))),
              by = join_by(wave, pidp)) |>
    arrange(pidp, wave) |>
    group_by(pidp) |>
    fill(everything()) |>
    ungroup()
  
  
  pop_data <- expanded_data |>
    select(
      pidp,
      wave,
      age_dv,
      sex_dv,
      gor_dv,
      mastat_dv,
      home_owner,
      dnc,
      hiqual_dv,
      wnw_race,
      sf12mcs_dv,
      sf12pcs_dv
    ) |>
    filter(wave == 8, !is.na(age_dv)) |>
    select(-wave) |>
    rename_with( ~ paste0(.x, "_base"), -c(pidp)) |>
    expand_grid(tibble(wave = 1:10,
                       t0 = -5:4)) |>
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
      wnw_race_base,
      sf12mcs_dv_base,
      sf12pcs_dv_base
    )  
  
  saveRDS(pop_data, data_file)
  
  }
  
  pop_data
}