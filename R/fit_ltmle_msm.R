# One ltmleMSM fit: all regimes against a single imputed dataset.
# Reduces the number of Q fits to decrease processing time. It's the same as calling ltmle() but it shares the g fits across al models
# Four waves, 16 regimes:
#
#                                   16 x ltmle()   1 x ltmleMSM()
#   g fits (4 A nodes)                  64                4
#   Q fits, first block (target = Y)    16                1
#   Q fits, remaining 3 blocks          48               48
#   total SuperLearner fits            128               53
#
# Returns a list, not a tibble: the covariance matrix must travel with the estimates so
# contrasts can be formed after pooling.

fit_ltmle_imp <- function(imp_idx, ltmle_data_list, regimes, sl_libs,
                          outcome, n_waves,
                          gbounds = c(1e-6, 1),
                          Yrange  = c(0, 1),
                          SL.cvControl = list(V = 3L)) {

  register_sl_wrappers()

  nodes <- ltmle_nodes(outcome, n_waves)
  data  <- ltmle_data_list[[imp_idx]]

  n_obs    <- nrow(data)
  n_anodes <- length(nodes$Anodes)
  R        <- length(regimes)
  labels   <- names(regimes)

  if (R < 2L) stop("fit_ltmle_imp(): need at least 2 regimes", call. = FALSE)
  if (is.null(labels) || anyDuplicated(labels)) {
    stop("fit_ltmle_imp(): `regimes` must have unique names", call. = FALSE)
  }

  bad <- vapply(regimes, function(a) length(as.vector(a)) != n_anodes, logical(1))
  if (any(bad)) {
    stop("fit_ltmle_imp(): regime(s) ", paste(labels[bad], collapse = ", "),
         " do not have ", n_anodes, " element(s)", call. = FALSE)
  }

  # regimes arg in ltmleMSM accepts an array of the strategies
  regime_arr <- array(NA_real_, dim = c(n_obs, n_anodes, R))
  for (j in seq_len(R)) {
    regime_arr[, , j] <- matrix(as.vector(regimes[[j]]),
                                nrow = n_obs, ncol = n_anodes, byrow = TRUE)
  }

  # An indicator per regime except the reference.
  ind <- paste0("d", seq_len(R)[-1L])
  sm  <- array(0, dim = c(R, R - 1L, 1L), dimnames = list(NULL, ind, NULL))
  for (j in 2:R) sm[j, j - 1L, 1L] <- 1

  rhs         <- paste(ind, collapse = " + ")
  working.msm <- paste("Y ~", rhs)

  X <- stats::model.matrix(stats::as.formula(paste("~", rhs)),
                           data = as.data.frame(sm[, , 1L]))

  fit <- ltmle::ltmleMSM(
    data             = data,
    Anodes           = nodes$Anodes,
    Lnodes           = nodes$Lnodes,
    Ynodes           = nodes$Ynodes,
    survivalOutcome  = FALSE,
    regimes          = regime_arr,
    summary.measures = sm,
    working.msm      = working.msm,
    msm.weights      = matrix(1, nrow = R, ncol = 1L),
    final.Ynodes     = NULL,
    stratify         = FALSE,
    gbounds          = gbounds,
    Yrange           = Yrange,
    SL.library       = sl_libs,
    SL.cvControl     = SL.cvControl,
    estimate.time    = FALSE,
    variance.method  = "ic"
  )

  # Per-regime means and their FULL covariance matrix.
  beta <- fit$beta

  if (!setequal(colnames(X), names(beta))) {
    stop("fit_ltmle_imp(): MSM design columns (",
         paste(colnames(X), collapse = ", "), ") do not match beta (",
         paste(names(beta), collapse = ", "), ")", call. = FALSE)
  }
  X <- X[, names(beta), drop = FALSE]

  # variance.method = "ic" leave this NULL.
  if (!is.null(fit$variance.estimate)) {
    stop("fit_ltmle_imp(): fit carries a non-IC variance.estimate; the ",
         "delta-method covariance here is IC-only", call. = FALSE)
  }

  eta <- as.vector(X %*% beta)
  m   <- stats::plogis(eta)
  IC  <- sweep(fit$IC %*% t(X), 2L, m * (1 - m), `*`)   # n x R
  V   <- stats::var(IC) / nrow(IC)

  names(m)    <- labels
  dimnames(V) <- list(labels, labels)

  list(imp_idx      = imp_idx,
       intervention = labels,
       estimate     = m,
       cov          = V)
}
