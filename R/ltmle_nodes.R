## Node layout for the LTMLE arm.
##
## These functions declare the column set and the within-wave ordering that
## prepare_ltmle_data() reshapes the mice object into. ltmle() classifies nodes
## by POSITION, so this order is the contract between the two.
##
## Qform / gform are deliberately not built here: ltmle derives them itself,
## regressing each node on every column that precedes it, which is exactly the
## largest parent set the node ordering below admits.

ltmle_node_names <- function(outcome) {
  outcome <- rlang::arg_match(outcome, values = c("MCS", "PCS"))

  list(
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
    # time-varying confounders, in the within-wave order of prepare_ltmle_data();
    # every one of these precedes A_t in the data
    conf = c(
      if (outcome == "MCS") "pcs_lagged" else "mcs_lagged",
      "econ_benefits_lagged",
      "home_owner_lagged",
      "mastat_dv_lagged",
      "dnc_fact_lagged"
    ),
    # The LTMLE arm is a sensitivity analysis for gFormulaMI, so it must
    # intervene on the same variable set_exposure() tags in build_data().
    A = "econ_emp_bin_fact",
    # measured after A_t and before Y_t: mediators of employment, and
    # exposure-induced confounders of the later waves
    post = c("log_income", "econ_dist_bin_fact"),
    Y = if (outcome == "MCS") "sf12mcs_dv" else "sf12pcs_dv"
  )
}

## Expand the node stems across n_waves waves.

ltmle_nodes <- function(outcome, n_waves) {
  outcome <- rlang::arg_match(outcome, values = c("MCS", "PCS"))

  n_waves <- as.integer(n_waves)
  if (length(n_waves) != 1L || is.na(n_waves) || n_waves < 1L) {
    stop("n_waves must be a single integer >= 1")
  }

  nm    <- ltmle_node_names(outcome)
  waves <- 0:(n_waves - 1L)

  at <- function(stems, t) paste0(stems, "_", t)

  # every node of wave t, in the order ltmle must see them
  wave_block <- function(t) c(at(nm$conf, t), at(nm$A, t), at(nm$post, t), at(nm$Y, t))

  # Everything after A_0 that is neither an A nor a Y node is an L node. The
  # wave-0 confounders precede A_0, so ltmle classifies them by position as
  # baseline covariates (W) and listing them here is an error; the wave-0 post
  # variables follow A_0 and must be listed.
  Lnodes <- c(
    at(nm$post, 0L),
    unlist(lapply(waves[waves > 0L], \(t) c(at(nm$conf, t), at(nm$post, t))),
           use.names = FALSE)
  )

  list(
    cols     = c(nm$baseline, unlist(lapply(waves, wave_block), use.names = FALSE)),
    Anodes   = at(nm$A, waves),
    Lnodes   = Lnodes,
    Ynodes   = at(nm$Y, waves),
    baseline = nm$baseline,
    n_waves  = n_waves
  )
}
