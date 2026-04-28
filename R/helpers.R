
# Helper: stratified g-formula run

# `run_em_stratum()` filters the full `mids` object to a stratum, runs `gFormulaImpute()`,
# pools the marginal and difference estimates via `syntheticPool()`, and caches the result.

# @ pending: add error tracing from mids imputation inside gFormulaMI

run_em_stratum <- function(mids_full, col, val, predictor_matrix,
                           outcome_var, strata_label, stratum_label,
                           trt_vars, regimes, regime_labels, cache_path,
                           datasets) {

  if (file.exists(cache_path)) {
    message(paste0("Loading cached: ", basename(cache_path)))
    return(readRDS(cache_path))
  }

  message(paste0("Running gFormulaMI — ", strata_label, " = ", stratum_label))

  # Step 1: filter to stratum (50 imputations x stratum n)
  mids_s <- dplyr::filter(mids_full, .data[[col]] == val)

  # Step 2: synthetic counterfactuals
  imps <- gFormulaImpute(
    mids_s,
    trtVars         = trt_vars,
    trtRegimes      = regimes,
    predictorMatrix = predictor_matrix,
    M               = datasets,
    silent          = TRUE
  )

  # filtered mids no longer needed
  rm(mids_s); gc()

  out_col <- paste0(outcome_var, "_2")

  # Step 3: fit marginal and difference models across synthetic datasets
  fits_marginal <- imps %$% lm(reformulate("0 + factor(regime)", out_col))
  fits_diff     <- imps %$% lm(reformulate("factor(regime)",     out_col))

  rm(imps); gc()

  pool_and_label <- function(fits) {
    syntheticPool(fits) |>
      as_tibble(rownames = "row") |>
      transmute(
        mi_effect = Estimate,
        mi_se     = sqrt(Total),
        mi_ll     = `95% CI L`,
        mi_ul     = `95% CI U`
      ) |>
      bind_cols(regime_labels) |>
      mutate(strata_var = strata_label, stratum = stratum_label)
  }

  # Step 4: pool results; release fit lists immediately after
  res <- list(
    marginal = pool_and_label(fits_marginal),
    diff     = pool_and_label(fits_diff)
  )

  rm(fits_marginal, fits_diff); gc()

  saveRDS(res, cache_path)
  res
}