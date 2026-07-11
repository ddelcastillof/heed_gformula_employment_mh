# g-formula MI predictor matrix and imputation of counterfactual outcomes

run_gformula <- function(wide_mids, wide_data_mi, intervention_pattern, estimand, M = 50) {
  estimand <- as.character(estimand)

  regimes <- intervention_pattern # from list created in build_data()

  predictor_matrix <- make_counterfactual_matrix(wide_data_mi)

  imps <- gFormulaMI::gFormulaImpute(
    data             = wide_mids,
    M                = M,
    trtVars          = c("econ_dist_bin_0"),
    trtRegimes       = regimes,
    predictorMatrix  = predictor_matrix,
    silent           = TRUE
  )

  fits <- imps %$%
    lm(as.formula(paste("sf12mcs_dv_0 ~", estimand)))

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

  return(out)
}
