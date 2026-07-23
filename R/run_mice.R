run_mice <- function(wide_data, m = 5, maxit = 10, seed = 20260522) {
  method_list <- mice::make.method(wide_data)

  # Method assignment by variable group
  method_by_group <- c(
    # time-invariant baseline confounders (columns end in _base)
    sex_dv               = "logreg",
    hiqual_dv_fact       = "pmm",
    race                 = "logreg",
    age_dv               = "norm",
    # time-varying confounders (NOT lagged), also time-invariant
    gor_dv_fact          = "pmm",
    # SF-12 scores: baseline (_base) and outcome
    sf12mcs_dv           = "pmm",
    sf12pcs_dv           = "pmm",
    # mediators / exposure
    econ_dist_bin_fact   = "logreg",
    log_income           = "pmm",
    econ_emp_bin_fact    = "logreg",   # exposure
    # lagged confounders mcs and pcs
    pcs_lagged           = "pmm",
    mcs_lagged           = "pmm",
    # other lagged confounders
    dnc_fact_lagged      = "pmm",
    home_owner_lagged    = "logreg",
    econ_benefits_lagged = "logreg",
    mastat_dv_lagged     = "logreg"
  )

  group   <- sub("_(\\d+|base)$", "", names(method_list))
  matched <- group %in% names(method_by_group)
  method_list[matched] <- method_by_group[group[matched]]

  # gor_dv_fact has low variability in the sample. 
  # So a LOCF approach via passive imputation will be used for missing intermediate waves 
  # (respondents that return in posterior waves).

  gor_cols <- grep("^gor_dv_fact_\\d+$", names(method_list), value = TRUE)
  gor_cols <- gor_cols[order(as.integer(sub("^gor_dv_fact_", "", gor_cols)))]

  if (length(gor_cols) > 2L) {
    interior <- seq.int(2L, length(gor_cols) - 1L)
    if (anyNA(wide_data[[gor_cols[1L]]])) {
      warning("run_mice: ", gor_cols[1L], " has missing values, so the interior ",
              "gor_dv_fact waves keep their '", method_by_group[["gor_dv_fact"]], "' method")
    } else {
      method_list[gor_cols[interior]] <- paste0("~ I(", gor_cols[interior - 1L], ")")
    }
  }

  pred_mat <- mice::make.predictorMatrix(wide_data)

  collinear <- grep("^(age_dv|gor_dv_fact)(_\\d+|_base)$", colnames(pred_mat), value = TRUE)
  pred_mat[, collinear] <- 0

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
