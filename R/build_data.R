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
    mastat_dv_lagged = shift(mastat_dv, type = "lag")
    )][wave %in% round_start:round_end]

# drop people who have BOTH >=2 missing outcome scores AND a fully missing
# baseline, using whichever outcome (MCS or PCS) is being built
  outcome_col <- if (outcome == "MCS") "sf12mcs_dv" else "sf12pcs_dv"
  outcome_sym <- rlang::sym(outcome_col)
  base_sym    <- rlang::sym(paste0(outcome_col, "_base"))
  no_outcome_ids <- rlang::inject(
    long_data[,
     .(n_missing = sum(is.na(!!outcome_sym)), base_missing = all(is.na(!!base_sym))),
     by = pidp
    ][n_missing >= 2 & base_missing == TRUE, .(pidp)]
  )

  long_data <- long_data[!no_outcome_ids, on = .(pidp)]

# matrix of possible intervention patterns according to number of waves included (0: no intervention, 1: intervention)
  if (step == "three") {
   intervention_pattern_3 <- asplit(as.matrix(CJ(0:1, 0:1, 0:1)), 1)
  } else if (step == "four") {
   intervention_pattern_4 <- asplit(as.matrix(CJ(0:1, 0:1, 0:1, 0:1)), 1)
  } else if (step == "five") {
   intervention_pattern_5 <- asplit(as.matrix(CJ(0:1, 0:1, 0:1, 0:1, 0:1)), 1)
  }

  wide_data <- long_data

# DAG-role stems for make_counterfactual_matrix(); roles whose columns are
# baseline-only in a given build are simply unused
  dag_roles <- list(income   = "log_income",
                    distress = "econ_dist_bin_fact")

  if (outcome == "MCS") {
    wide_data <- wide_data |> 
      make_wide(pidp,
                t0,
                base_cols = c(sex_dv_base,
                              hiqual_dv_fact_base,
                              race_base,
                              sf12mcs_dv_base,
                              age_dv_base,
                              gor_dv_fact_base),
                outcome = sf12mcs_dv,
                pcs_lagged,
                econ_dist_bin_fact,
                dnc_fact_lagged,
                home_owner_lagged,
                econ_benefits_lagged,
                mastat_dv_lagged,
                econ_emp_bin_fact,
                log_income,
                waves = c(0:(round_end - round_start)),
                roles = dag_roles
                ) |>
      data.table::as.data.table()
} else if (outcome == "PCS") {
    wide_data <- wide_data |>
      make_wide(pidp,
                t0,
                base_cols = c(sex_dv_base,
                              hiqual_dv_fact_base,
                              race_base,
                              sf12pcs_dv_base,
                              age_dv_base,
                              gor_dv_fact_base),
                outcome = sf12pcs_dv,
                mcs_lagged,
                econ_dist_bin_fact,
                dnc_fact_lagged,
                home_owner_lagged,
                econ_benefits_lagged,
                mastat_dv_lagged,
                econ_emp_bin_fact,
                log_income,
                waves = c(0:(round_end - round_start)),
                roles = dag_roles
                ) |>
      data.table::as.data.table()
}
    wide_data <- set_exposure(wide_data,
                              exposure = "econ_emp_bin_fact")
  
    return(list(
      data         = wide_data,
      intervention = if (step == "three") {
        intervention_pattern_3
      } else if (step == "four") {
        intervention_pattern_4
      } else {
        intervention_pattern_5
      }
    ))
}
