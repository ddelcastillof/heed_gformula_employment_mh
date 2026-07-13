# g-formula MI predictor matrix and imputation of counterfactual outcomes

run_gformula <- function(wide_mids, wide_data_mi, intervention_pattern, estimand, M = 50) {
  library(magrittr)
  estimand <- as.character(estimand)

  regimes <- intervention_pattern # from list created in build_data()

  # treatment and outcome columns tagged by make_wide()/set_exposure(), so
  # they scale with the number of waves (three/four/five) and the outcome
  # (MCS/PCS); e.g. four waves -> econ_emp_bin_fact_0..3, sf12mcs_dv_3
  trt_vars    <- attr(wide_data_mi, "exposure_vars")
  outcome_var <- attr(wide_data_mi, "outcome_var")

  if (is.null(regimes)) {
    stop("intervention_pattern is NULL: pass build_data()$intervention")
  }
  if (is.null(trt_vars) || is.null(outcome_var)) {
    stop("wide_data_mi lacks 'exposure_vars'/'outcome_var' attributes: ",
         "pass build_data()$data")
  }
  if (length(regimes[[1]]) != length(trt_vars)) {
    stop("regime length (", length(regimes[[1]]), ") != number of treatment ",
         "columns (", length(trt_vars), "): ", paste(trt_vars, collapse = ", "))
  }

  predictor_matrix <- make_counterfactual_matrix(wide_data_mi)

  imps <- gFormulaMI::gFormulaImpute(
    data             = wide_mids,
    M                = M,
    trtVars          = trt_vars,
    trtRegimes       = regimes,
    predictorMatrix  = predictor_matrix,
    silent           = TRUE
  )

  fits <- imps %$%
    lm(as.formula(paste(outcome_var, "~", estimand)))

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

  # Detect loggedEvents and report them in the build log.
  le <- imps$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_gformula: no logged events.")
  } else {
    message("run_gformula: ", nrow(le), " logged event(s) during imputation:")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }

  return(list(results = out, imputations = imps))
}
