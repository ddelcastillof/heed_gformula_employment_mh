# Rubin's rules for the LTMLE arm, in matrix form, plus the reference contrasts.

pool_ltmle <- function(ltmle_msm_fits, y_scale = 100, reference = NULL) {
  if (!is.list(ltmle_msm_fits) || !length(ltmle_msm_fits)) {
    stop("pool_ltmle(): expected a non-empty list of fit_ltmle_imp() results",
         call. = FALSE)
  }
  # A single unbranched fit is still a valid input; wrap it so the code below is uniform.
  if (!is.null(ltmle_msm_fits$estimate)) ltmle_msm_fits <- list(ltmle_msm_fits)

  labels <- ltmle_msm_fits[[1L]]$intervention
  ok <- purrr::map_lgl(ltmle_msm_fits, \(f) identical(f$intervention, labels))
  if (!all(ok)) {
    stop("pool_ltmle(): imputations disagree on the regime set", call. = FALSE)
  }

  M <- length(ltmle_msm_fits)
  R <- length(labels)

  Q <- do.call(rbind, purrr::map(ltmle_msm_fits, \(f) as.vector(f$estimate)))
  U <- purrr::map(ltmle_msm_fits, "cov")

  qbar <- colMeans(Q)
  Ubar <- purrr::reduce(U, `+`) / M
  B    <- if (M > 1L) stats::var(Q) else matrix(0, R, R)
  Tm   <- Ubar + (1 + 1 / M) * B

  qbar <- qbar * y_scale
  Tm   <- Tm * y_scale^2
  names(qbar) <- labels
  dimnames(Tm) <- list(labels, labels)

  ci_tbl <- function(rows, est, se) {
    tibble::tibble(
      intervention = rows,
      n_imp        = M,
      ltmle_effect = round(est, 3),
      ltmle_se     = round(se, 3),
      ltmle_ll     = round(est - 1.96 * se, 3),
      ltmle_ul     = round(est + 1.96 * se, 3)
    )
  }

  # Difference of every regime from the reference, using the pooled covariance.
  if (is.null(reference)) {
    n_nodes   <- length(strsplit(labels[1L], "-", fixed = TRUE)[[1L]])
    reference <- paste(rep(0, n_nodes), collapse = "-")
  }
  if (!reference %in% labels) {
    stop("pool_ltmle(): reference regime '", reference,
         "' is not among the fitted regimes", call. = FALSE)
  }

  others <- setdiff(labels, reference)
  est    <- qbar[others] - qbar[reference]
  v      <- diag(Tm)[others] + Tm[reference, reference] - 2 * Tm[others, reference]

  if (any(v <= 0)) {
    stop("pool_ltmle(): non-positive contrast variance for ",
         paste(others[v <= 0], collapse = ", "), call. = FALSE)
  }

  list(
    estimates = ci_tbl(labels, qbar, sqrt(diag(Tm))),
    contrasts = ci_tbl(others, est, sqrt(v)),
    qbar      = qbar,
    T         = Tm
  )
}
