build_data <- function(data = pop_data, round_start, round_end, step, outcome) {

  # verify data is DT
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }

  step <- rlang::arg_match(step, values = c("three", "four", "five"))

  outcome <- rlang::arg_match(outcome, values = c("MCS", "PCS"))

  message(paste0("Building data for ", step, " (waves ", round_start, "-", round_end, ")"))

  eligible_pidp <- intersect(
    data[wave == round_start & response == 1, pidp],
    data[wave == round_end & response == 1, pidp]
  )

  pop_DT <- data[pidp %in% eligible_pidp]

# long format variable shifting
  long_data <- pop_DT[, `:=`(
    t0 = t0 - 2L, 
    pcs_lagged = shift(sf12pcs_dv, type = "lag"),
    mcs_lagged = shift(sf12mcs_dv, type = "lag"),
# the other time-varying confounders should also be lagged
    dnc_fact_lagged = shift(dnc_fact, type = "lag"),
    home_owner_lagged = shift(home_owner, type = "lag"),
    econ_benefits_lagged = shift(econ_benefits, type = "lag"),
#    gor_dv_fact_lagged = shift(gor_dv_fact, type = "lag"),
#    age_dv_lagged = shift(age_dv, type = "lag"),
    mastat_dv_lagged = shift(mastat_dv, type = "lag")
    )][wave %in% round_start:round_end]

# keeping those with at least 2 observed MCS outcomes and no baseline missing
  no_mh_outcomes <- long_data[,
   .(n_missing = sum(is.na(sf12mcs_dv)), base_missing = all(is.na(sf12mcs_dv_base))),
   by = pidp
  ][n_missing >= 2 & base_missing == TRUE, .(pidp)]

  long_data <- long_data[!no_mh_outcomes, on = .(pidp)]

# matrix of possible intervention patterns according to number of waves included (0: no intervention, 1: intervention)
  if (step == "three") {
   intervention_pattern_3 <- asplit(as.matrix(CJ(0:1, 0:1, 0:1)), 1)
  } else if (step == "four") {
   intervention_pattern_4 <- asplit(as.matrix(CJ(0:1, 0:1, 0:1, 0:1)), 1)
  } else if (step == "five") {
   intervention_pattern_5 <- asplit(as.matrix(CJ(0:1, 0:1, 0:1, 0:1, 0:1)), 1)
  }

  wide_data <- long_data
  
  if (outcome == "MCS") {
    wide_data <- wide_data |> 
      make_wide(pidp,
                t0,
                base_cols = c(sex_dv_base,
                              hiqual_dv_fact_base,
                              race_base,
                              sf12mcs_dv_base),
                outcome = sf12mcs_dv,
                age_dv,
                gor_dv_fact,
                pcs_lagged,
                econ_dist_bin_fact,
                dnc_fact_lagged,
                home_owner_lagged,
                econ_benefits_lagged,
                mastat_dv_lagged,
                econ_emp_bin_fact,
                log_income,
                waves = c(0:(round_end - round_start))
                ) |>
      data.table::as.data.table()
} else if (outcome == "PCS") {
    wide_data <- wide_data |>
      make_wide(pidp,
                t0,
                base_cols = c(sex_dv_base,
                              hiqual_dv_fact_base,
                              race_base,
                              sf12mcs_dv_base),
                outcome = sf12pcs_dv,
                age_dv,
                gor_dv_fact,
                mcs_lagged,
                econ_dist_bin_fact,
                dnc_fact_lagged,
                home_owner_lagged,
                econ_benefits_lagged,
                mastat_dv_lagged,
                econ_emp_bin_fact,
                log_income,
                waves = c(0:(round_end - round_start))
                ) |>
      data.table::as.data.table()
}
    wide_data <- set_exposure(wide_data, 
                              exposure = "econ_emp_bin_fact")
  
    return(wide_data)
}
