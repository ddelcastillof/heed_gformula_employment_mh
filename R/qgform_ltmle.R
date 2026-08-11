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

## Reproduce ltmle's Q-block rule.
##
## ltmle does not fit one Q regression per Y node. It collapses each maximal run
## of consecutive L/Y nodes that no A node interrupts into a single block, and
## names that block's regression after the run's FIRST node. Any Qform entry
## named after a node that is not a block leader is silently dropped and
## replaced by ltmle's own default, so the names must be derived, never assumed:
## with A sitting mid-wave, the runs start at the first post-treatment variable
## (log_income_t), not at the outcome.
ltmle_q_blocks <- function(nodes) {
  cols <- nodes$cols
  ly   <- which(cols %in% c(nodes$Lnodes, nodes$Ynodes))
  if (!length(ly)) return(character(0))
  # a gap in the positions means an A node interrupted the run
  cols[ly[c(TRUE, diff(ly) != 1L)]]
}

## Reproduce ltmle's parent rule on a prepared data frame
check_form_parents <- function(gform, Qform, data, q_blocks = NULL) {
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

  # names ltmle will not recognise are dropped silently and replaced by its
  # defaults, so a Qform that passes the parent rule can still go unused
  if (!is.null(q_blocks) && !identical(names(Qform), q_blocks)) {
    problems <- c(problems, paste0(
      "Qform names are not ltmle's Q blocks, so ltmle would drop them and use ",
      "its defaults.\n    have: ", toString(names(Qform)),
      "\n    want: ", toString(q_blocks)))
  }

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

  nm    <- ltmle_node_names(outcome)
  nodes <- ltmle_nodes(outcome, n_waves)

  at <- function(stems, t) paste0(stems, "_", t)

  # the part of the RHS that lives in earlier waves: the previous treatment, the
  # post-treatment variables it induced, and the previous outcome
  past <- function(t) {
    if (t == 0L) {
      character(0)
    } else {
      c(at(nm$A, t - 1L), at(nm$post, t - 1L), at(nm$Y, t - 1L))
    }
  }

  waves <- 0:(n_waves - 1L)

  # gform: only what precedes A_t, so the wave-t post variables are excluded
  gform <- vapply(waves, function(t) {
    rhs <- c(nm$baseline, at(nm$conf, t), past(t))
    paste(at(nm$A, t), "~", paste(rhs, collapse = " + "))
  }, character(1))
  names(gform) <- at(nm$A, waves)

  # Qform: one entry per Q block, keyed on the node that leads the block. The
  # parents are everything preceding that node, which is the largest set ltmle
  # accepts -- variables inside the block (the rest of the post-treatment
  # variables, and the wave's own outcome) come after the block leader and so
  # are not available to its regression.
  q_blocks <- ltmle_q_blocks(nodes)
  Qform <- vapply(q_blocks, function(node) {
    rhs <- nodes$cols[seq_len(match(node, nodes$cols) - 1L)]
    paste("Q.kplus1 ~", paste(rhs, collapse = " + "))
  }, character(1))
  names(Qform) <- q_blocks

  if (!is.null(data)) check_form_parents(gform, Qform, data, q_blocks = q_blocks)

  return(list(gform = gform, Qform = Qform))
}


