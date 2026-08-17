# Custom SuperLearner learners for the LTMLE step.

## Registrer SuperLearner wrappers with a function
### Pending: add more learners

register_sl_wrappers <- function() {
  nms <- c("SL.xgboost2.ltmle", "SL.xgboost4.ltmle",
           "SL.gam.ltmle2", "SL.gam.ltmle3", "SL.gam.ltmle4", "SL.gam.ltmle5",
           "SL.rf.ltmle", "predict.SL.rf.ltmle", 
           "SL.nnet5", "SL.nnet10", "predict.SL.nnet.ltmle",
           "SL.poly2", "SL.poly3", ".poly_design", "predict.SL.poly.ltmle")
  src <- environment(register_sl_wrappers)
  for (nm in nms) assign(nm, get(nm, envir = src), envir = globalenv())
  invisible(nms)
}

## gam learner for LTMLE
SL.gam.ltmle <- function(Y, X, newX, family, deg.gam = 10, ...) {
  if (!requireNamespace("gam", quietly = TRUE)) stop("package 'gam' required")
  SuperLearner::SL.gam(Y = Y, X = X, newX = newX, family = family, deg.gam = deg.gam, ...)
}

SL.gam.ltmle2 <- function(...) SL.gam.ltmle(..., deg.gam = 2)
SL.gam.ltmle3 <- function(...) SL.gam.ltmle(..., deg.gam = 3)
SL.gam.ltmle4 <- function(...) SL.gam.ltmle(..., deg.gam = 4)
SL.gam.ltmle5 <- function(...) SL.gam.ltmle(..., deg.gam = 5)


## XGboost covers non-linearities and interactions, even in contexts with n>p
SL.xgboost.ltmle <- function(Y, X, newX, family, depth = 1, ntrees = 1000, ...) {
  Y <- as.numeric(Y)
  if (!all(Y %in% c(0, 1))) family <- gaussian()
  SuperLearner::SL.xgboost(Y = Y, 
                           X = X, 
                           newX = newX, 
                           family = family, 
                           max_depth = depth, 
                           ntrees = ntrees, 
                           ...)
}

SL.xgboost2.ltmle <- function(...) SL.xgboost.ltmle(..., depth = 2, ntrees = 500)
SL.xgboost4.ltmle <- function(...) SL.xgboost.ltmle(..., depth = 4, ntrees = 500)

SL.rf.ltmle <- function(Y, X, newX, family,
                       obsWeights = rep(1, length(Y)), id = NULL,
                       num.trees = 100, min.node.size = 50, ...) {
  if (!requireNamespace("ranger", quietly = TRUE)) stop("package 'ranger' required")

  Xm  <- data.matrix(X)
  NXm <- data.matrix(newX)
  colnames(Xm) <- colnames(NXm) <- make.names(colnames(Xm), unique = TRUE)

  fit <- ranger::ranger(
    x = Xm, y = Y,
    num.trees     = num.trees,
    min.node.size = min.node.size,
    case.weights  = obsWeights,
    num.threads   = 1,
    ...
  )
  pred <- stats::predict(fit, data = NXm)$predictions
  out  <- list(object = fit, xnames = colnames(Xm))
  class(out) <- "SL.rf.ltmle"
  list(pred = pred, fit = out)
}

predict.SL.rf.ltmle <- function(object, newdata, ...) {
  NXm <- data.matrix(newdata)
  colnames(NXm) <- object$xnames
  stats::predict(object$object, data = NXm)$predictions
}

SL.poly.ltmle <- function(Y, X, newX, family,
                         obsWeights = rep(1, length(Y)),
                         degree = 2, ...) {
  Xm  <- data.matrix(X)
  NXm <- data.matrix(newX)
  colnames(Xm) <- colnames(NXm) <- make.names(colnames(Xm), unique = TRUE)

  cont <- apply(Xm, 2, function(x) length(unique(x)) > 2)
  ctr  <- colMeans(Xm[, cont, drop = FALSE])
  scl  <- apply(Xm[, cont, drop = FALSE], 2, stats::sd); scl[scl < 1e-8] <- 1

  Xd  <- .poly_design(Xm,  cont, ctr, scl, degree)
  NXd <- .poly_design(NXm, cont, ctr, scl, degree)

  fam <- if (all(Y >= 0 & Y <= 1)) stats::quasibinomial() else stats::gaussian()
  fit <- stats::glm(Y ~ ., data = data.frame(Y = Y, Xd),
                    family = fam, weights = obsWeights)

  pred <- as.numeric(stats::predict(fit, newdata = NXd, type = "response"))
  out  <- list(object = fit, cont = cont, ctr = ctr, scl = scl, degree = degree)
  class(out) <- "SL.poly.ltmle"
  list(pred = pred, fit = out)
}

.poly_design <- function(M, cont, ctr, scl, degree) {
  Cm <- scale(M[, cont, drop = FALSE], center = ctr, scale = scl)
  pw <- do.call(cbind, lapply(seq(2, degree), function(d) {
    P <- Cm^d
    colnames(P) <- paste0(colnames(Cm), "_p", d)
    P
  }))
  as.data.frame(cbind(M[, !cont, drop = FALSE], Cm, pw))
}

predict.SL.poly.ltmle <- function(object, newdata, ...) {
  NXm <- data.matrix(newdata)
  colnames(NXm) <- make.names(colnames(NXm), unique = TRUE)
  NXd <- .poly_design(NXm, object$cont, object$ctr, object$scl, object$degree)
  as.numeric(stats::predict(object$object, newdata = NXd, type = "response"))
}

SL.poly2 <- function(...) SL.poly.ltmle(..., degree = 2)
SL.poly3 <- function(...) SL.poly.ltmle(..., degree = 3)

SL.nnet.ltmle <- function(Y, X, newX, family,
                         obsWeights = rep(1, length(Y)), id = NULL,
                         size = 20, decay = 0.02, maxit = 200,
                         n.restarts = 1, ...) {
  if (!requireNamespace("nnet", quietly = TRUE)) stop("package 'nnet' required")

  Xm  <- data.matrix(X)
  ctr <- colMeans(Xm)
  scl <- apply(Xm, 2, stats::sd); scl[scl < 1e-8] <- 1
  Xs  <- scale(Xm, center = ctr, scale = scl)
  NXs <- scale(data.matrix(newX), center = ctr, scale = scl)

  bounded <- all(Y >= 0 & Y <= 1)
  # Continuous Y needs scaling too: `decay` penalises the output weights, so an
  # outcome on the ~50-point SF-12 scale would be shrunk into a near-flat fit.
  y.ctr <- if (bounded) 0 else mean(Y)
  y.scl <- if (bounded) 1 else stats::sd(Y)
  if (y.scl < 1e-8) y.scl <- 1
  Yt <- (Y - y.ctr) / y.scl

  best <- NULL
  for (i in seq_len(n.restarts)) {
    fit <- try(nnet::nnet(
      x = Xs, y = Yt, weights = obsWeights,
      size = size, decay = decay, maxit = maxit,
      linout = !bounded, entropy = bounded, softmax = FALSE,
      rang = 0.7, trace = FALSE,
      MaxNWts = size * (ncol(Xs) + 2) + 100
    ), silent = TRUE)
    if (inherits(fit, "try-error")) next
    # Same data and decay across restarts, so $value is directly comparable.
    if (is.null(best) || fit$value < best$value) best <- fit
  }
  if (is.null(best)) stop("SL.nnet.ltmle: all nnet restarts failed")

  pred <- as.numeric(stats::predict(best, newdata = NXs, type = "raw")) * y.scl + y.ctr
  out  <- list(object = best, ctr = ctr, scl = scl, y.ctr = y.ctr, y.scl = y.scl)
  class(out) <- "SL.nnet.ltmle"
  list(pred = pred, fit = out)
}

predict.SL.nnet.ltmle <- function(object, newdata, ...) {
  NXs <- scale(data.matrix(newdata), center = object$ctr, scale = object$scl)
  as.numeric(stats::predict(object$object, newdata = NXs, type = "raw")) *
    object$y.scl + object$y.ctr
}

SL.nnet10 <- function(...) SL.nnet.ltmle(..., size = 5)
SL.nnet20 <- function(...) SL.nnet.ltmle(..., size = 10)