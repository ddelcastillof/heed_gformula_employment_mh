# Reduce a list of ltmle fits to the two numbers Rubin's rules need.
summarise_ltmle_fits <- function(ltmle_fits, regime_labels, imp_idx = NA_integer_) {
  stopifnot(length(ltmle_fits) == length(regime_labels))

  trs <- purrr::map(ltmle_fits, \(fit) summary(fit, estimator = "tmle")$treatment)

  tibble::tibble(
    intervention = as.character(regime_labels),
    imp_idx      = imp_idx,
    estimate     = purrr::map_dbl(trs, "estimate"),
    variance     = purrr::map_dbl(trs, "std.dev")^2
  )
}

# Pool the per-imputation estimates with Rubin's rules.
pool_ltmle <- function(ltmle_summaries) {
  by_regime <- split(ltmle_summaries, ltmle_summaries$intervention)
  by_regime <- by_regime[unique(ltmle_summaries$intervention)]

  purrr::imap_dfr(by_regime, function(d, label) {
    pooled <- mice::pool.scalar(Q = d$estimate, U = d$variance, n = Inf, k = 1)
    se     <- sqrt(pooled$t)
    tibble::tibble(
      intervention = label,
      n_imp        = nrow(d),
      ltmle_effect = pooled$qbar,
      ltmle_se     = se,
      ltmle_ll     = pooled$qbar - 1.96 * se,
      ltmle_ul     = pooled$qbar + 1.96 * se
    )
  }) |>
    dplyr::mutate(dplyr::across(
      c(ltmle_effect, ltmle_se, ltmle_ll, ltmle_ul),
      ~ round(.x * 100, 3)
    ))
}
