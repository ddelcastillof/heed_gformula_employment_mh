# Rubin's rules for the LTMLE arm, in matrix form.
#
# fit_ltmle_imp() now returns one ltmleMSM fit per imputation: a vector of regime means
# plus their full covariance matrix. Rubin's rules extend to that unchanged --
#
#   qbar = mean_i q_i
#   Ubar = mean_i U_i                 (within-imputation, averaged)
#   B    = var(q_i)                   (between-imputation, R x R)
#   T    = Ubar + (1 + 1/M) B
#
# -- and because every step is linear in q_i, any contrast c satisfies
# Var(c'qbar) = c'Tc. That is why contrasts are formed AFTER pooling here: it is
# algebraically identical to forming them per imputation and pooling afterwards, but it
# keeps the covariance, which the previous per-regime mice::pool.scalar() loop discarded.
pool_ltmle <- function(ltmle_msm_fits, y_scale = 100) {
  if (!is.list(ltmle_msm_fits) || !length(ltmle_msm_fits)) {
    stop("pool_ltmle(): expected a non-empty list of fit_ltmle_imp() results",
         call. = FALSE)
  }
  # A single unbranched fit is still a valid input; wrap it so the code below is uniform.
  if (!is.null(ltmle_msm_fits$estimate)) ltmle_msm_fits <- list(ltmle_msm_fits)

  labels <- ltmle_msm_fits[[1L]]$intervention
  ok <- vapply(ltmle_msm_fits, function(f) identical(f$intervention, labels), logical(1))
  if (!all(ok)) {
    stop("pool_ltmle(): imputations disagree on the regime set", call. = FALSE)
  }

  M <- length(ltmle_msm_fits)
  R <- length(labels)

  Q <- do.call(rbind, lapply(ltmle_msm_fits, function(f) as.vector(f$estimate)))
  U <- lapply(ltmle_msm_fits, `[[`, "cov")

  qbar <- colMeans(Q)
  Ubar <- Reduce(`+`, U) / M
  B    <- if (M > 1L) stats::var(Q) else matrix(0, R, R)
  Tm   <- Ubar + (1 + 1 / M) * B

  # prepare_ltmle_data() divides the outcome by y_scale, so means scale linearly and
  # the covariance by the square.
  qbar <- qbar * y_scale
  Tm   <- Tm * y_scale^2
  names(qbar) <- labels
  dimnames(Tm) <- list(labels, labels)

  se <- sqrt(diag(Tm))

  list(
    estimates = tibble::tibble(
      intervention = labels,
      n_imp        = M,
      ltmle_effect = round(qbar, 3),
      ltmle_se     = round(se, 3),
      ltmle_ll     = round(qbar - 1.96 * se, 3),
      ltmle_ul     = round(qbar + 1.96 * se, 3)
    ),
    qbar = qbar,
    T    = Tm
  )
}


# Difference of every regime from a reference regime, using the pooled covariance.
#
# Var(m_j - m_ref) = T[j,j] + T[ref,ref] - 2 T[j,ref]. The old ltmle_contrasts() summed
# the two variances because fit_ltmle_imp() discarded the influence curves, which
# assumed independence between arms estimated on the very same subjects. The covariance
# is positive here, so those intervals were too wide -- in the opposite direction to
# ltmle's own warning that the IC variance is anticonservative under positivity
# violations. This version needs no such caveat.
ltmle_contrasts <- function(pooled, reference = NULL) {
  qbar   <- pooled$qbar
  Tm     <- pooled$T
  labels <- names(qbar)

  if (is.null(reference)) {
    n_nodes   <- length(strsplit(labels[1L], "-", fixed = TRUE)[[1L]])
    reference <- paste(rep(0, n_nodes), collapse = "-")
  }
  if (!reference %in% labels) {
    stop("ltmle_contrasts(): reference regime '", reference,
         "' is not among the fitted regimes", call. = FALSE)
  }

  others <- setdiff(labels, reference)
  est    <- qbar[others] - qbar[reference]
  v      <- diag(Tm)[others] + Tm[reference, reference] - 2 * Tm[others, reference]

  if (any(v <= 0)) {
    stop("ltmle_contrasts(): non-positive contrast variance for ",
         paste(others[v <= 0], collapse = ", "), call. = FALSE)
  }
  se <- sqrt(v)

  tibble::tibble(
    intervention = others,
    n_imp        = pooled$estimates$n_imp[1L],
    ltmle_effect = round(est, 3),
    ltmle_se     = round(se, 3),
    ltmle_ll     = round(est - 1.96 * se, 3),
    ltmle_ul     = round(est + 1.96 * se, 3)
  )
}
