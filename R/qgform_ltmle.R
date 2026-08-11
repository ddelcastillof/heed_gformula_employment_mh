## Auxiliar functions to build Qform and gform for ltmle
## Q and g regression formulas for the LTMLE arm.

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
      "dnc_fact_lagged",
      "econ_emp_bin_fact",
      "log_income"
    ),
    A = "econ_dist_bin_fact",
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
  wave_block <- function(t) c(at(nm$conf, t), at(nm$A, t), at(nm$Y, t))

  # Wave-0 confounders sit before A_0, so ltmle classifies them by position as
  # baseline covariates (W). Listing them as Lnodes is an error, not a choice.
  l_waves <- waves[waves > 0L]

  list(
    cols     = c(nm$baseline, unlist(lapply(waves, wave_block), use.names = FALSE)),
    Anodes   = at(nm$A, waves),
    Lnodes   = if (length(l_waves)) {
      unlist(lapply(l_waves, \(t) at(nm$conf, t)), use.names = FALSE)
    } else {
      character(0)
    },
    Ynodes   = at(nm$Y, waves),
    baseline = nm$baseline,
    n_waves  = n_waves
  )
}

## Reproduce ltmle's parent rule on a prepared data frame
check_form_parents <- function(gform, Qform, data) {
  nms <- names(data)

  rhs_vars <- function(f) all.vars(stats::as.formula(f)[[3L]])

  problems <- character(0)

  check_one <- function(f, node, label) {
    pos <- match(node, nms)
    if (is.na(pos)) {
      problems <<- c(problems, paste0(label, ": node '", node, "' is not a column of data"))
      return(invisible(NULL))
    }
    rhs     <- rhs_vars(f)
    missing <- setdiff(rhs, nms)
    late    <- setdiff(intersect(rhs, nms)[match(intersect(rhs, nms), nms) >= pos], missing)
    if (length(missing)) {
      problems <<- c(problems, paste0(label, " (", node, "): not columns of data: ",
                                      toString(missing)))
    }
    if (length(late)) {
      problems <<- c(problems, paste0(label, " (", node, "): do not precede it: ",
                                      toString(late)))
    }
    invisible(NULL)
  }

  for (i in seq_along(gform)) check_one(gform[[i]], names(gform)[i], paste0("gform[", i, "]"))
  for (i in seq_along(Qform)) check_one(Qform[[i]], names(Qform)[i], paste0("Qform[", i, "]"))

  if (length(problems)) {
    stop("pick_gform_qform(): formulas do not match the data:\n  ",
         paste(problems, collapse = "\n  "), call. = FALSE)
  }

  invisible(TRUE)
}

## Build the Qform / gform pair for one outcome and one wave count.
## outcome  "MCS" or "PCS"
pick_gform_qform <- function(outcome,
                             n_waves = 4L,
                             data = NULL) {

  outcome <- rlang::arg_match(outcome, values = c("MCS", "PCS"))

  n_waves <- as.integer(n_waves)
  if (length(n_waves) != 1L || is.na(n_waves) || n_waves < 1L) {
    stop("n_waves must be a single integer >= 1")
  }

  nm <- ltmle_node_names(outcome)

  at <- function(stems, t) paste0(stems, "_", t)

  # every node of wave k, in data order
  wave_block <- function(k) c(at(nm$conf, k), at(nm$A, k), at(nm$Y, k))

  # the part of the RHS that lives in earlier waves
  past <- function(t) {
    if (t == 0L) {
      character(0)
    } else {
      c(at(nm$A, t - 1L), at(nm$Y, t - 1L))
    }
  }

  waves <- 0:(n_waves - 1L)

  # gform
  gform <- vapply(waves, function(t) {
    rhs <- c(nm$baseline, at(nm$conf, t), past(t))
    paste(at(nm$A, t), "~", paste(rhs, collapse = " + "))
  }, character(1))
  names(gform) <- at(nm$A, waves)

  # Qform
  Qform <- vapply(waves, function(t) {
    rhs <- c(nm$baseline, at(nm$conf, t), at(nm$A, t), past(t))
    paste("Q.kplus1 ~", paste(rhs, collapse = " + "))
  }, character(1))
  names(Qform) <- at(nm$Y, waves)

  if (!is.null(data)) check_form_parents(gform, Qform, data)

  return(list(gform = gform, Qform = Qform))
}


