run_mice <- function(wide_data, m = 5, maxit = 10, seed = 20260522) {
  method_list <- mice::make.method(wide_data)

  # Method assignment by variable group
  method_by_group <- c(
    # time-invariant baseline confounders (columns end in _base)
    sex_dv               = "logreg",
    hiqual_dv_fact       = "polr",
    race                 = "logreg",
    gor_dv_fact          = "polr",
    age_dv               = "norm",
    # SF-12 scores: baseline (_base) and outcome
    sf12mcs_dv           = "pmm",
    sf12pcs_dv           = "pmm",
    # time-varying confounders / exposure
    econ_dist_bin_fact   = "logreg",
    log_income           = "pmm",
    econ_emp_bin_fact    = "logreg",   # exposure
    # lagged confounders mcs and pcs
    pcs_lagged           = "pmm",
    mcs_lagged           = "pmm",
    # other lagged confounders
    dnc_fact_lagged      = "polyreg",
    home_owner_lagged    = "logreg",
    econ_benefits_lagged = "logreg",
    mastat_dv_lagged     = "logreg"
  )

  group   <- sub("_(\\d+|base)$", "", names(method_list))
  matched <- group %in% names(method_by_group)
  method_list[matched] <- method_by_group[group[matched]]

  pred_mat <- mice::make.predictorMatrix(wide_data)

  no_predict <- grep("^(gor_dv_fact|age_dv)_\\d+$", colnames(pred_mat), value = TRUE) # almost collinear with other covariates, not used on observed outcomes imputations
  pred_mat[, no_predict] <- 0

  mids <-  mice::mice(
           data            = wide_data,
           m               = m,
           maxit           = maxit,
           seed            = seed,
           method          = method_list,
           predictorMatrix = pred_mat
          )

  le <- mids$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_mice: no logged events.")
  } else {
    message("run_mice: ", nrow(le), " logged event(s) during imputation:")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }

  mids
}
