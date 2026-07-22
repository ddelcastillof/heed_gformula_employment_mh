build_data <- function(data = pop_data, round_start, round_end, how_many, outcome) {

  # verify data is DT
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }
  
  # verify round_start and round_end are valid
  if (round_start >= round_end) {
    stop("round_start must be less than round_end")
  } 

  # verify how_many (number of rounds) is valid
  if (how_many %in% c("three", "four", "five")) {
    how_many <- rlang::arg_match(how_many, values = c("three", "four", "five"))
  } else {
    stop("Function can only evaluate three, four or five waves. Please specify the how_many argument")
  }

  # verify which outcome is being used
  if (outcome %in% c("MCS", "PCS")) {
    outcome <- rlang::arg_match(outcome, values = c("MCS", "PCS"))
  } else {
    stop("Function can only evaluate MCS or PCS outcomes. Please specify the outcome argument")
  }

  message(paste0("Building data for ", how_many, "-waves pipeline ", "(", round_start, "-", round_end, ")"))

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

# keeping those with at least 2 observed outcomes and no baseline missing
  outcome_col <- if (outcome == "MCS") "sf12mcs_dv" else "sf12pcs_dv"
  base_col    <- paste0(outcome_col, "_base")
  n_waves     <- round_end - round_start + 1L

  no_mh_outcomes <- long_data[,
    .(n_missing    = sum(is.na(.SD[[1L]])),
      base_missing = all(is.na(.SD[[2L]]))),
    by = pidp,
    .SDcols = c(outcome_col, base_col)
  ][n_missing >= (n_waves - 1L) & base_missing == TRUE, .(pidp)]

  long_data <- long_data[!no_mh_outcomes, on = .(pidp)]

# matrix of possible intervention patterns according to number of waves included (0: no intervention, 1: intervention)
  if (how_many == "three") {
   intervention_pattern <- asplit(as.matrix(CJ(0:1, 0:1, 0:1)), 1)
  } else if (how_many == "four") {
   intervention_pattern <- asplit(as.matrix(CJ(0:1, 0:1, 0:1, 0:1)), 1)
  } else if (how_many == "five") {
   intervention_pattern <- asplit(as.matrix(CJ(0:1, 0:1, 0:1, 0:1, 0:1)), 1)
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
                mediators = c(log_income,
                              econ_dist_bin_fact),
                age_dv,
                gor_dv_fact,
                econ_emp_bin_fact, #exposure
                pcs_lagged,
                dnc_fact_lagged,
                home_owner_lagged,
                econ_benefits_lagged,
                mastat_dv_lagged,
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
                              sf12pcs_dv_base),
                outcome = sf12pcs_dv,
                mediators = c(log_income,
                              econ_dist_bin_fact),
                age_dv,
                gor_dv_fact,
                econ_emp_bin_fact, #exposure
                mcs_lagged,
                dnc_fact_lagged,
                home_owner_lagged,
                econ_benefits_lagged,
                mastat_dv_lagged,
                waves = c(0:(round_end - round_start))
                ) |>
      data.table::as.data.table()
}
    wide_data <- set_exposure(wide_data, 
                              exposure = "econ_emp_bin_fact")
  
    return(list(data = wide_data,
                intervention_pattern = intervention_pattern))
}
