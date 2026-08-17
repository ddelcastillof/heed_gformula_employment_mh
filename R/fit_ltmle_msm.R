# One ltmleMSM fit: all regimes against a single imputed dataset.
#
# WHY THIS REPLACES 16 SEPARATE ltmle() CALLS
#
# ltmle() is a thin wrapper over ltmleMSM(): GetMSMInputsForLtmle() turns a single
# `abar` into one regime with `working.msm = "Y ~ 1"` and `msm.weights = matrix(1,1,1)`,
# and LtmleFromInputs() returns plogis(beta). So looping over regimes means running
# 16 independent one-regime MSMs.
#
# That loop refits models it does not need to. In ltmle:::Estimate the per-node branch is
#
#   if (regime.index == first.regime || multiple.subs || multiple.Qstar) FitAndPredict()
#   else                                                                PredictOnly(newdata)
#
# with `multiple.subs <- is.matrix(subs)` (TRUE only when stratify = TRUE) and
# `multiple.Qstar <- is.matrix(Qstar.kplus1)`. For a g node Qstar.kplus1 is NULL, so a
# single g fit is shared across every regime -- correctly, since g is `A_k ~ history`
# and contains no reference to the regime being evaluated; only the prediction, with A
# set to the regime, differs. For a Q node Qstar.kplus1 becomes an n x num.regimes
# matrix of regime-specific updates, so Q genuinely refits per regime -- except at the
# first step of the recursion, where the target is the observed Y.
#
# Four waves, 16 regimes:
#
#                                   16 x ltmle()   1 x ltmleMSM()
#   g fits (4 A nodes)                  64                4
#   Q fits, first block (target = Y)    16                1
#   Q fits, remaining 3 blocks          48               48
#   total SuperLearner fits            128               53
#
# A SATURATED MSM IS NOT AN APPROXIMATION. The working MSM is a projection of the
# regime-specific means onto the design given in `summary.measures`. With as many free
# parameters as regimes (intercept + R-1 indicators) the projection is exact and
# plogis(X %*% beta) reproduces each regime's mean algebraically. Verified against 16
# ltmle() calls in tests/testthat/tests.R.

# Saturated-MSM inputs for a set of static regimes.
#
# `regimes` is the named list of static patterns from _targets.R; each element is a
# vector of length num.Anodes. ltmleMSM instead wants an n x num.Anodes x num.regimes
# array (every row of a slice is that regime's pattern, since these are static rules).
ltmle_msm_design <- function(regimes, n_obs, n_anodes) {
  R      <- length(regimes)
  labels <- names(regimes)

  if (R < 2L) stop("ltmle_msm_design(): need at least 2 regimes", call. = FALSE)
  if (is.null(labels) || anyDuplicated(labels)) {
    stop("ltmle_msm_design(): `regimes` must have unique names", call. = FALSE)
  }

  bad <- vapply(regimes, function(a) length(as.vector(a)) != n_anodes, logical(1))
  if (any(bad)) {
    stop("ltmle_msm_design(): regime(s) ", paste(labels[bad], collapse = ", "),
         " do not have ", n_anodes, " element(s)", call. = FALSE)
  }

  regime_arr <- array(NA_real_, dim = c(n_obs, n_anodes, R))
  for (j in seq_len(R)) {
    regime_arr[, , j] <- matrix(as.vector(regimes[[j]]),
                                nrow = n_obs, ncol = n_anodes, byrow = TRUE)
  }

  # Saturated design: an indicator per regime except the reference (the first).
  ind <- paste0("d", seq_len(R)[-1L])
  sm  <- array(0, dim = c(R, R - 1L, 1L), dimnames = list(NULL, ind, NULL))
  for (j in 2:R) sm[j, j - 1L, 1L] <- 1

  working.msm <- paste("Y ~", paste(ind, collapse = " + "))

  # Build the MSM design matrix from the same formula and frame ltmle will use, so the
  # columns line up with `beta` by name rather than by assumed position.
  X <- stats::model.matrix(
    stats::as.formula(paste("~", paste(ind, collapse = " + "))),
    data = as.data.frame(sm[, , 1L])
  )

  list(regimes = regime_arr, summary.measures = sm, working.msm = working.msm,
       X = X, labels = labels,
       # matrix(1, R, 1): GetMsmWeights() defaults to "empirical", which weights each
       # regime by how many people actually followed it and zeroes duplicates -- and
       # Estimate() then loops only over `regimes.with.positive.weight`. With 10 of 16
       # regimes supported by 56-371 rows that would quietly downweight exactly the
       # intermittent trajectories the study is about. The one-regime ltmle() path uses
       # matrix(1,1,1), so all-ones is what reproduces it.
       msm.weights = matrix(1, nrow = R, ncol = 1L))
}


# Per-regime means and their FULL covariance matrix from one ltmleMSM fit.
#
# summary.ltmleMSM() reports the beta scale: v <- apply(IC, 2, var) and
# std.dev <- sqrt(v / nrow(IC)), i.e. Cov(beta) = var(IC) / n. Each regime mean is
# m_j = plogis(X_j' beta), so by the delta method the influence curve of m_j is
# (IC %*% X_j) * m_j * (1 - m_j) -- exactly the transform LtmleFromInputs() applies in
# the one-regime case (`IC$tmle <- IC[, 1] * tmle * (1 - tmle)`). Taking var() of the
# whole transformed matrix gives the between-regime covariances too, which 16 separate
# calls cannot provide.
ltmle_msm_estimates <- function(fit, X, labels) {
  beta <- fit$beta

  if (!setequal(colnames(X), names(beta))) {
    stop("ltmle_msm_estimates(): MSM design columns (",
         paste(colnames(X), collapse = ", "), ") do not match beta (",
         paste(names(beta), collapse = ", "), ")", call. = FALSE)
  }
  X <- X[, names(beta), drop = FALSE]

  # variance.method = "ic" leaves this NULL. If it is ever set, summary.ltmleMSM would
  # use pmax(diag(variance.estimate), IC.variance) and the IC-only covariance below
  # would understate the variance -- refuse rather than silently disagree.
  if (!is.null(fit$variance.estimate)) {
    stop("ltmle_msm_estimates(): fit carries a non-IC variance.estimate; the ",
         "delta-method covariance here is IC-only", call. = FALSE)
  }

  eta <- as.vector(X %*% beta)
  m   <- stats::plogis(eta)
  IC  <- sweep(fit$IC %*% t(X), 2L, m * (1 - m), `*`)   # n x R
  V   <- stats::var(IC) / nrow(IC)

  names(m)    <- labels
  dimnames(V) <- list(labels, labels)

  list(estimate = m, cov = V)
}


# One targets branch: every regime against a single imputed dataset.
#
# Returns a list, not a tibble: the covariance matrix has to travel with the estimates
# so contrasts can be formed after pooling. The ltmle_sum_* targets therefore use
# `iteration = "list"`.
fit_ltmle_imp <- function(imp_idx, ltmle_data_list, regimes, sl_libs,
                          outcome, n_waves,
                          gbounds = c(1e-6, 1),
                          Yrange  = c(0, 1),
                          SL.cvControl = list(V = 3L)) {

  register_sl_wrappers()

  nodes <- ltmle_nodes(outcome, n_waves)
  data  <- ltmle_data_list[[imp_idx]]

  des <- ltmle_msm_design(regimes  = regimes,
                          n_obs    = nrow(data),
                          n_anodes = length(nodes$Anodes))

  fit <- ltmle::ltmleMSM(
    data             = data,
    Anodes           = nodes$Anodes,
    Lnodes           = nodes$Lnodes,
    Ynodes           = nodes$Ynodes,
    survivalOutcome  = FALSE,
    regimes          = des$regimes,
    summary.measures = des$summary.measures,
    working.msm      = des$working.msm,
    msm.weights      = des$msm.weights,
    # NULL resolves to the last Y node only in ltmle 1.3.0, which is the estimand here.
    # (The paper's section 4.3 describes 0.9-9-3, where the default differed.)
    final.Ynodes     = NULL,
    stratify         = FALSE,
    gbounds          = gbounds,
    Yrange           = Yrange,
    SL.library       = sl_libs,
    SL.cvControl     = SL.cvControl,
    estimate.time    = FALSE,
    variance.method  = "ic"
  )

  est <- ltmle_msm_estimates(fit, X = des$X, labels = des$labels)

  list(imp_idx      = imp_idx,
       intervention = des$labels,
       estimate     = est$estimate,
       cov          = est$cov)
}
