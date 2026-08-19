## Node layout for LTMLE arm. Expand node stems across n_waves waves.

ltmle_nodes <- function(outcome, n_waves) {
  outcome <- rlang::arg_match(outcome, values = c("MCS", "PCS"))
  n_waves <- as.integer(n_waves)
  if (length(n_waves) != 1L || is.na(n_waves) || n_waves < 1L) {
    stop("n_waves must be a single integer >= 1")
  }

  nm <- list(
    # time-invariant, measured before A0
    baseline = c(
      "sex_dv_base",
      "hiqual_dv_fact_base",
      "race_base",
      "gor_dv_fact_base",
      "age_dv_base",
      "age_dv_sq_base",
      if (outcome == "MCS") "sf12mcs_dv_base" else "sf12pcs_dv_base"
    ),
    # time-varying confounders
    conf = c(
      if (outcome == "MCS") "pcs_lagged" else "mcs_lagged",
      "econ_benefits_lagged",
      "home_owner_lagged",
      "mastat_dv_lagged",
      "dnc_fact_lagged"
    ),
    # exposure
    A = "econ_emp_bin_fact",
    # mediators
    post = c("log_income", "econ_dist_bin_fact"),
    # outcome
    Y = if (outcome == "MCS") "sf12mcs_dv" else "sf12pcs_dv"
  )

  waves <- 0:(n_waves - 1L)

  at <- function(stems, t) paste0(stems, "_", t)

  wave_block <- function(t) c(at(nm$conf, t), at(nm$A, t), at(nm$post, t), at(nm$Y, t))

  Lnodes <- c(
    at(nm$post, 0L),
    purrr::list_c(purrr::map(waves[waves > 0L],
                             \(t) c(at(nm$conf, t), at(nm$post, t))))
  )

  list(
    cols     = c(nm$baseline, purrr::list_c(purrr::map(waves, wave_block))),
    Anodes   = at(nm$A, waves),
    Lnodes   = Lnodes,
    Ynodes   = at(nm$Y, waves),
    baseline = nm$baseline,
    n_waves  = n_waves
  )
}
