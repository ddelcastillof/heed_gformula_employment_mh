# Custom SuperLearner learners for the LTMLE step.

## Registrer SuperLearner wrappers with a function
### Pending: add more learners

register_sl_wrappers <- function() {
  nms <- c("SL.xgboost.ltmle")
  src <- environment(register_sl_wrappers)
  for (nm in nms) assign(nm, get(nm, envir = src), envir = globalenv())
  invisible(nms)
}

## XGboost covers non-linearities and interactions, even in contexts with n>p
SL.xgboost.ltmle <- function(Y, X, newX, family, ...) {
  if (is.factor(Y)) Y <- as.integer(Y) - 1L
  if (!all(Y %in% c(0, 1))) family <- gaussian()
  SuperLearner::SL.xgboost(Y = Y, X = X, newX = newX, family = family, ...)
}