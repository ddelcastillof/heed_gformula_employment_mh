# g-formula MI predictor matrix and imputation of counterfactual outcomes

run_gform <- function(wide_mids, wide_data_mi, intervention_pattern, estimand, M = 50, nSim = NULL) {
  library(magrittr)
  estimand <- as.character(estimand)

  regimes <- intervention_pattern # from list created in build_data()

  # treatment and outcome columns are assigned an attribute from make_wide, 
  # which were ported to this point of the pipeline and scale naturally with number of waves
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
  method_vector    <- make_counterfactual_method(wide_data_mi)

  # detecting logged events inside gFormulaMI. Needs to be identified per mice call inside
  # one slot per completed mice() call. After detection, these are assigned to an environment 'calls'
  log_env <- rlang::env(calls = list())

  collect_logged <- function(res) {
    if (is.null(res$iteration) || res$iteration == 0L) return(invisible(NULL))
    log_env$calls <- c(log_env$calls, list(res$loggedEvents))
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

  imps <- withCallingHandlers(
    gFormulaMI::gFormulaImpute(
      data             = wide_mids,
      M                = M,
      trtVars          = trt_vars,
      trtRegimes       = regimes,
      nSim             = nSim,
      method           = method_vector,
      predictorMatrix  = predictor_matrix,
      silent           = TRUE
    ),
    warning = function(w) {
      if (grepl("^Number of logged events", conditionMessage(w))) {
        rlang::cnd_muffle(w)
      }
    }
  )

  if (!is.null(wide_mids$m) && wide_mids$m != M) {
    message("run_gformula: gFormulaImpute reset M to ", wide_mids$m,
            " (requested ", M, "): M is bound to the number of imputations in ",
            "wide_mids. Change m in run_mice() to change it.")
  }

  # Report the loggedEvents captured at the mice() calls above
  n_mice_calls  <- length(log_env$calls)
  logged_events <- purrr::keep(log_env$calls, \(le) !is.null(le) && nrow(le) > 0)

  if (length(logged_events) == 0) {
    message("run_gformula: no logged events in ", n_mice_calls, " mice() call(s).")
  } else {
    tally <- logged_events |>
      purrr::list_rbind() |>
      dplyr::count(dep, meth, out, sort = TRUE)

    message("run_gformula: ", length(logged_events), " of ", n_mice_calls,
            " mice() call(s) logged events. dep | meth | out (n calls):")
    purrr::pwalk(tally, \(dep, meth, out, n)
                 message("  ", dep, " | ", meth, " | ", out, "  (", n, ")"))
  }

  fits <- imps %$%
    lm(as.formula(paste(outcome_final, "~", estimand)))

  outvals <- gFormulaMI::syntheticPool(fits)

  regimes_x <- tibble::tibble(
    intervention = purrr::map_chr(regimes, paste, collapse = "-")
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

  return(list(results = out))
}
