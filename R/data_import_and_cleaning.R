import_data <- function() {
  require(tidyverse)

  final_cache <- here::here("data", "pop_data_new.rds")
  fst_cache   <- here::here("data", "raw", "heed_raw.fst")
  raw_dta     <- here::here("data", "raw", "ukhls_pooled_all_obs_02.dta")

  if (file.exists(final_cache)) {
    message(final_cache, " already exists, reading in...")
    return(readRDS(final_cache))
  }

  if (!dir.exists(here::here("data", "raw"))) dir.create(here::here("data", "raw"), recursive = TRUE)

  labels_cache <- here::here("data", "raw", "value_labels.rds")

  if (!file.exists(fst_cache)) {
    message("Reading raw .dta (slow)...")
    raw <- haven::read_stata(raw_dta)

    saveRDS(
      list(
        value_labels = lapply(raw, \(x) if (haven::is.labelled(x)) attr(x, "labels") else NULL) |> Filter(Negate(is.null), x = _),
        var_labels   = lapply(raw, \(x) attr(x, "label")) |> Filter(Negate(is.null), x = _)
      ),
      labels_cache
    )

    fst::write_fst(dplyr::mutate(raw, dplyr::across(dplyr::where(haven::is.labelled), haven::zap_labels)), fst_cache)
    rm(raw)
  }

  message("Reading from .fst cache...")
  data_in <- fst::read_fst(fst_cache)

  tidied_data <- data_in |>
    filter(between(age_dv, 25, 64)) |>
    mutate( 
      across(where(is.numeric), \(x) if_else(x < 0, NA, x)),
      across(c(gor_dv, hiqual_dv, econ_incquint, econ_dist, econ_benefits, mastat_dv, home_owner, dnc), as_factor),
      across(c(pidp, ppid, hidp, wave, pns1pid, pns2pid, age_dv), as.integer),
      # *Generating our exposure (Employment status)
      les_c4 = case_when(
        les_c4 == 1 ~ "Employed or self-employed",
        les_c4 == 2 ~ "Student",
        les_c4 == 3 ~ "Not employed",
        les_c4 == 4 ~ "Retired",
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
      econ_incquint,
      econ_emp_bin,
      econ_dist,
      econ_benefits,
      gor_dv,
      mastat_dv,
      home_owner,
      dnc,
      age_dv,
      # base vars
      wnw_race,
      sex_dv,
      hiqual_dv
    ) |>
    mutate(
      wnw_race = factor(wnw_race),
      response = 1) |>
    full_join(tibble(wave = 1:10) |>
                expand_grid(tibble(pidp = unique(tidied_data$pidp))),
              by = join_by(wave, pidp)) |>
    replace_na(list(response = 0)) |> 
    arrange(pidp, wave) |>
    group_by(pidp) |>
    fill(hiqual_dv, .direction = "down") |>
    fill(sex_dv, wnw_race, .direction = "downup") |>
    fill(gor_dv, .direction = "downup") |>
    ungroup()
  
  
  pop_data <- expanded_data |>
    select(
      pidp,
      wave,
      response,
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
    mutate(age_dv = mean(age_dv - wave + 1, na.rm = TRUE) |> round(), .by = "pidp") |> 
    filter(wave == 1) |>
    select(-wave) |>
    rename_with( ~ paste0(.x, "_base"), -c(pidp)) |>
    expand_grid(tibble(wave = 1:10,
                       t0 = 0:9)) |>
    left_join(expanded_data, by = join_by(pidp, wave)) |>
    select(
      # id and time vars
      pidp,
      wave,
      response,
      t0,
      # time-varying vars
      sf12mcs_dv,
      sf12pcs_dv,
      log_income,
      econ_incquint,
      econ_emp_bin,
      econ_dist,
      econ_benefits,
      gor_dv,
      mastat_dv,
      home_owner,
      dnc,
      age_dv,
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
  
  saveRDS(pop_data, final_cache)

  pop_data
}