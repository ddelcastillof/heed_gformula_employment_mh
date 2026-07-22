# g-formula MI predictor matrix and imputation of counterfactual outcomes

run_gform <- function(wide_mids, wide_data_mi, intervention_pattern, estimand, M = 50) {
  library(magrittr)
  estimand <- as.character(estimand)

  regimes <- intervention_pattern # from list created in build_data()

  # treatment and outcome columns tagged by make_wide()/set_exposure(), so
  # they scale with the number of waves (three/four/five) and the outcome
  # (MCS/PCS); e.g. four waves -> econ_emp_bin_fact_0..3, sf12mcs_dv_3
  trt_vars    <- attr(wide_data_mi, "exposure_vars")
  outcome_final <- attr(wide_data_mi, "outcome_final")

  if (is.null(regimes)) {
    stop("intervention_pattern is NULL: extract the intervention object from the build_data list")
  }
  if (is.null(trt_vars) || is.null(outcome_final)) {
    stop("wide_data_mi lacks 'exposure_vars'/'outcome_final' attributes: ",
         "extract the data from the build_data list")
  }
  if (length(regimes[[1]]) != length(trt_vars)) {
    stop("regime length (", length(regimes[[1]]), ") != number of treatment ",
         "columns (", length(trt_vars), "): ", paste(trt_vars, collapse = ", "))
  }

  predictor_matrix <- make_counterfactual_matrix(wide_data_mi)

  # gFormulaImpute calls mice::mice once per input imputation and returns the
  # syntheses via mice::as.mids() on a stacked long data frame, which rebuilds
  # the mids from scratch: loggedEvents are dropped and imps$loggedEvents is
  # always NULL. Capture them at each mice() call instead.
  # The tracer expression is evaluated inside mice's frame, where `<<-` would
  # walk the mice namespace rather than this function, so a closure over this
  # frame is spliced into the expression and `<<-` resolves through it.
  logged_events <- list()
  n_mice_calls  <- 0L

  collect_logged <- function(res) {
    # mice::as.mids(), used by gFormulaImpute to assemble the returned object,
    # also calls mice(maxit = 0, remove.collinear = FALSE) to build a skeleton.
    # Skip it: only the maxit > 0 synthesis calls are of interest.
    if (is.null(res$iteration) || res$iteration == 0L) return(invisible(NULL))
    n_mice_calls <<- n_mice_calls + 1L
    le <- res$loggedEvents
    if (!is.null(le) && nrow(le) > 0) {
      logged_events[[length(logged_events) + 1L]] <<- le
    }
    invisible(NULL)
  }

  invisible(suppressMessages(trace(
    "mice",
    exit  = bquote(.(collect_logged)(returnValue())),
    print = FALSE,
    where = asNamespace("mice")
  )))
  on.exit(suppressMessages(untrace("mice", where = asNamespace("mice"))),
          add = TRUE)

  # mice warns "Number of logged events: n" once per call, so an M-imputation
  # run emits M near-identical warnings. Muffle them and report the aggregated
  # contents below, which name the offending variables.
  imps <- withCallingHandlers(
    gFormulaMI::gFormulaImpute(
      data             = wide_mids,
      M                = M,
      trtVars          = trt_vars,
      trtRegimes       = regimes,
      predictorMatrix  = predictor_matrix,
      silent           = TRUE
    ),
    warning = function(w) {
      if (grepl("^Number of logged events", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # For a mids input gFormulaImpute unconditionally sets M <- data$m. This is
  # by design, not a quirk: Bartlett et al. (2025) sec. 3.4 generate exactly one
  # synthetic sample per imputation of the observed data (N = 1 in the two-stage
  # scheme), which is what makes syntheticPool's variance estimator unbiased.
  # M and m are therefore the same quantity here and cannot be set apart.
  # The package does say so, but via print() under silent = FALSE, so the
  # silent = TRUE above hides it -- hence this message.
  if (!is.null(wide_mids$m) && wide_mids$m != M) {
    message("run_gformula: gFormulaImpute reset M to ", wide_mids$m,
            " (requested ", M, "): M is bound to the number of imputations in ",
            "wide_mids. Change m in run_mice() to change it.")
  }

  # Report the loggedEvents captured at the mice() calls above, collapsed to
  # one row per distinct (dep, meth, out) with the number of calls affected.
  # Reported here, before pooling, so the diagnostics survive a syntheticPool()
  # failure -- which is exactly when they are most useful.
  if (length(logged_events) == 0) {
    message("run_gformula: no logged events in ", n_mice_calls, " mice() call(s).")
  } else {
    all_le <- do.call(rbind, logged_events)
    tally  <- table(paste(all_le$dep, all_le$meth, all_le$out, sep = " | "))
    tally  <- sort(tally, decreasing = TRUE)
    message("run_gformula: ", length(logged_events), " of ", n_mice_calls,
            " mice() call(s) logged events. dep | meth | out (n calls):")
    for (k in names(tally)) message("  ", k, "  (", tally[[k]], ")")
  }

  fits <- imps %$%
    lm(as.formula(paste(outcome_final, "~", estimand)))

  outvals <- gFormulaMI::syntheticPool(fits)

  regimes_x <- tibble::tibble(
    intervention = regimes |>
      purrr::map(paste, collapse = "-") |>
      purrr::reduce(c)
  )

  out <- outvals |>
    tibble::as_tibble() |>
    tibble::rownames_to_column("Intervention") |>
    dplyr::transmute(
      mi_effect = Estimate,
      mi_se     = sqrt(Total),
      mi_ll     = `95% CI L`,
      mi_ul     = `95% CI U`
    ) |>
    dplyr::bind_cols(regimes_x)

  return(list(results = out, imputations = imps))
}
