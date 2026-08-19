## G-formula helper for effect modification: running the model in only one strata
## the function will use the same wide_data_mcs or pcs depending on the outcome

## Baseline columns that carry no information once the data is split. Subsetting on
## sex_dv_base == "Male" leaves a complete, constant column: mice removes it as a
## predictor and logs an event for every variable it touches, and
## make_counterfactual_method() still hands it a real method, because it probes with
## an all-NA row appended, so gFormulaImpute would then fit logreg against a single
## observed level. Dropping it up front avoids both. Age is unaffected: its strata
## are ranges, so age_dv_base still varies within each.
##
## j-subsetting a data.table returns a new object without the make_wide()/
## set_exposure() attributes, so they are saved and restamped, with the dropped
## names removed from the column-name attributes.
drop_constant_baseline <- function(stratum) {
  vec_attrs    <- c("baseline_vars", "time_lagged", "time_varying",
                    "outcome_vars", "mediators", "exposure_vars")
  scalar_attrs <- c("outcome_final", "outcome_baseline", "time_points")

  base_vars <- attr(stratum, "baseline_vars")
  const     <- base_vars[purrr::map_lgl(base_vars,
                                        \(v) data.table::uniqueN(stratum[[v]]) <= 1L)]
  if (length(const) == 0L) return(stratum)

  saved <- purrr::map(rlang::set_names(c(vec_attrs, scalar_attrs)),
                      \(a) attr(stratum, a))
  out   <- stratum[, setdiff(names(stratum), const), with = FALSE]

  for (a in vec_attrs)    attr(out, a) <- setdiff(saved[[a]], const)
  for (a in scalar_attrs) attr(out, a) <- saved[[a]]

  message(paste0("  dropped constant baseline column(s): ", toString(const)))
  out
}

build_data_em <- function(data, how_many, outcome, round_start, round_end, modifier,
                          age_cut = NULL) {

  library(data.table)

# verify data is DT
  if (!data.table::is.data.table(data)) {
    stop("data must be a data.table")
  }

  # verify round_start and round_end are valid
  if (round_start >= round_end) {
    stop("round_start must be less than round_end")
  } 

  # verify how_many (number of rounds) is valid
  if (how_many %in% c("three", "four", "five", "six")) {
    how_many <- rlang::arg_match(how_many, values = c("three", 
                                                      "four", 
                                                      "five", 
                                                      "six",
                                                      "seven",
                                                      "eight"))
  } else {
    stop("Function can only evaluate three to eight waves. Please specify the how_many argument")
  }

  # verify which outcome is being used
  if (outcome %in% c("MCS", "PCS")) {
    outcome <- rlang::arg_match(outcome, values = c("MCS", "PCS"))
  } else {
    stop("Function can only evaluate MCS or PCS outcomes. Please specify the outcome argument")
  }

  # verify the effect modifier up front
  if (modifier %in% c("sex", "age", "hiqual")) {
    modifier <- rlang::arg_match(modifier, values = c("sex", "age", "hiqual"))
  } else {
    stop("Invalid modifier specified. Please choose from 'sex', 'age', or 'hiqual'.")
  }

  # verify age_cut is an integer if modifier is age
  if (modifier == "age" && (!is.numeric(age_cut) || age_cut %% 1 != 0)) {
    stop("For age modifier, age_cut must be an integer.")
  }

  message(paste0("Building data for ", how_many, "-waves pipeline ", "(", round_start, "-", round_end, ")"))

  eligible_pidp <- intersect(
    data[wave == round_start & response == 1, pidp],
    data[wave == round_end & response == 1, pidp]
  )

  pop_DT <- data[pidp %in% eligible_pidp]

# long format variable shifting
  long_data <- pop_DT[, `:=`(
    t0 = t0 - 2L, 
    pcs_lagged = shift(sf12pcs_dv, type = "lag"),
    mcs_lagged = shift(sf12mcs_dv, type = "lag"),
# the other time-varying confounders should also be lagged
    dnc_fact_lagged = shift(dnc_fact, type = "lag"),
    home_owner_lagged = shift(home_owner, type = "lag"),
    econ_benefits_lagged = shift(econ_benefits, type = "lag"),
    mastat_dv_lagged = shift(mastat_dv, type = "lag")
    )][wave %in% round_start:round_end]

# keeping those with at least 2 observed outcomes and no baseline missing
  outcome_col <- if (outcome == "MCS") "sf12mcs_dv" else "sf12pcs_dv"
  base_col    <- paste0(outcome_col, "_base")
  n_waves     <- round_end - round_start + 1L

  no_mh_outcomes <- long_data[,
    .(n_missing    = sum(is.na(.SD[[1L]])),
      base_missing = all(is.na(.SD[[2L]]))),
    by = pidp,
    .SDcols = c(outcome_col, base_col)
  ][n_missing >= (n_waves - 1L) & base_missing == TRUE, .(pidp)]

  long_data <- long_data[!no_mh_outcomes, on = .(pidp)]

# matrix of possible intervention patterns according to number of waves included (0: no intervention, 1: intervention)
  n_intervention <- match(how_many, c("three", "four", "five", "six", "seven", "eight")) + 2L
  intervention_pattern <- asplit(as.matrix(do.call(CJ, rep(list(0:1), n_intervention))), 1)

  wide_data <- long_data
  
  if (outcome == "MCS") {
    wide_data <- wide_data |> 
      make_wide(pidp,
                t0,
                base_cols = c(gor_dv_fact_base,
                              sex_dv_base,
                              race_base,
                              hiqual_dv_fact_base,
                              age_dv_base,
                              sf12mcs_dv_base),
                outcome = sf12mcs_dv,
                mediators = c(log_income,
                              econ_dist_bin_fact),
                gor_dv_fact,
                pcs_lagged,
                dnc_fact_lagged,
                home_owner_lagged,
                econ_benefits_lagged,
                mastat_dv_lagged,
                econ_emp_bin_fact,
                waves = c(0:(round_end - round_start))
                ) |>
      data.table::as.data.table()
} else if (outcome == "PCS") {
    wide_data <- wide_data |>
      make_wide(pidp,
                t0,
                base_cols = c(gor_dv_fact_base,
                              sex_dv_base,
                              race_base,
                              hiqual_dv_fact_base,
                              age_dv_base,
                              sf12pcs_dv_base),
                outcome = sf12pcs_dv,
                mediators = c(log_income,
                              econ_dist_bin_fact),
                gor_dv_fact,
                mcs_lagged,
                dnc_fact_lagged,
                home_owner_lagged,
                econ_benefits_lagged,
                mastat_dv_lagged,
                econ_emp_bin_fact,
                waves = c(0:(round_end - round_start))
                ) |>
      data.table::as.data.table()
}
    wide_data <- set_exposure(wide_data, 
                              exposure = "econ_emp_bin_fact")
  
  ## splitting data across effect modifier strata (sex, age, highest education).
  strata <- switch(modifier,
    sex = list(
      male   = wide_data[sex_dv_base == "Male"],
      female = wide_data[sex_dv_base == "Female"]
    ),
    age = list(
      age_younger = wide_data[age_dv_base <= age_cut],
      age_older   = wide_data[age_dv_base >  age_cut]
    ),
    hiqual = list(
      hiqual_high   = wide_data[hiqual_dv_fact_base == "High"],
      hiqual_medium = wide_data[hiqual_dv_fact_base == "Medium"],
      hiqual_low    = wide_data[hiqual_dv_fact_base == "Low"]
    )
  )

  n_strata <- purrr::map_int(strata, nrow)

  ## the modifier column is constant inside its own stratum: drop it here, once,
  ## so the mice run and the counterfactual imputation downstream both see the
  ## same columns without either having to re-derive the stratification
  strata <- purrr::map(strata, drop_constant_baseline)

  message(paste0("Strata sizes (", modifier,
                 if (modifier == "age") paste0(", cut at ", age_cut) else "",
                 "): ",
                 paste(names(n_strata), n_strata, sep = " = ", collapse = ", "),
                 " of ", nrow(wide_data), " rows"))

  c(strata, list(intervention_pattern = intervention_pattern))
}

## Step 1: impute WITHIN the stratum to preserve congeniality with final counterfactual imputation + ATE estimation
run_mice_strata <- function(stratum, strata_label, stratum_label, m, maxit, seed) {
  message(paste0("Running MICE imputation for ", strata_label, " = ", stratum_label,
                 " (", nrow(stratum), " rows)"))
  run_mice(wide_data = stratum, m = m, maxit = maxit, seed = seed)
}

## Step 2-4: counterfactual imputation, model fits and synthetic pooling for one stratum. 
run_em_stratum <- function(mids_stratum, stratum, intervention_pattern,
                           strata_label, stratum_label, M = 50,
                           cache_path = NULL, imps_path = NULL) {

  library(magrittr)

  # cache_path defaults to NULL so targets owns invalidation: a file that exists
  # short-circuits the target's own hash and serves stale results after an upstream
  # change. Pass a path only when driving this from a script.
  if (!is.null(cache_path) && file.exists(cache_path)) {
    message(paste0("Loading cached: ", basename(cache_path)))
    return(readRDS(cache_path))
  }

  regimes       <- intervention_pattern
  trt_vars      <- attr(stratum, "exposure_vars")
  outcome_final <- attr(stratum, "outcome_final")

  # fail before the expensive impute, not after
  if (is.null(regimes)) {
    stop("intervention_pattern is NULL: extract the intervention object from the ",
         "build_data_em() list")
  }
  if (is.null(trt_vars) || is.null(outcome_final)) {
    stop("stratum lacks 'exposure_vars'/'outcome_final' attributes: pass a stratum ",
         "from the build_data_em() list, not a plain subset")
  }
  if (length(regimes[[1]]) != length(trt_vars)) {
    stop("regime length (", length(regimes[[1]]), ") != number of treatment ",
         "columns (", length(trt_vars), "): ", paste(trt_vars, collapse = ", "))
  }
  # gFormulaImpute matches the predictor matrix to mids$data by name, so a mids
  # imputed from a different column set silently misaligns the DAG. Row count is
  # checked as well because two strata of the same modifier share a schema
  # exactly — names alone would pass a hiqual_low mids against a hiqual_high
  # stratum and label the results with the wrong group.
  if (!identical(names(mids_stratum$data), names(stratum)) ||
      nrow(mids_stratum$data) != nrow(stratum)) {
    stop("mids_stratum was not imputed from this stratum: ",
         nrow(mids_stratum$data), " x ", ncol(mids_stratum$data), " vs ",
         nrow(stratum), " x ", ncol(stratum),
         ". run_mice_strata() must have been given this exact stratum")
  }

  message(paste0("Running gFormulaMI — ", strata_label, " = ", stratum_label))

  predictor_matrix <- make_counterfactual_matrix(stratum)
  method_vector    <- make_counterfactual_method(stratum)

  # gFormulaImpute hardcodes maxit = 1 and draws columns left to right, so an arrow
  # pointing rightward reads that parent's iteration-0 hot-deck seed instead of its
  # drawn value. Dropping a constant baseline column preserves the column order, but
  # the invariant is cheap to assert here and expensive to discover downstream.
  rightward <- which(predictor_matrix == 1 & upper.tri(predictor_matrix), arr.ind = TRUE)
  if (nrow(rightward) > 0L) {
    stop("predictor matrix is not lower-triangular, so ", nrow(rightward),
         " arrow(s) read hot-deck seeds: ",
         toString(paste0(rownames(predictor_matrix)[rightward[, "row"]], " <- ",
                         colnames(predictor_matrix)[rightward[, "col"]])))
  }

  # gFormulaImpute calls mice() once per regime and keeps none of the loggedEvents.
  # Trace the call to collect them: stratifying is exactly what makes a small
  # stratum's models degenerate, and the pooled run never showed it.
  log_env <- rlang::env(calls = list())

  collect_logged <- function(res) {
    if (is.null(res$iteration) || res$iteration == 0L) return(invisible(NULL))
    log_env$calls <- c(log_env$calls, list(res$loggedEvents))
    invisible(NULL)
  }

  invisible(suppressMessages(trace(
    "mice",
    exit  = bquote(.(collect_logged)(returnValue())),
    print = FALSE,
    where = asNamespace("mice")
  )))
  on.exit(suppressMessages(untrace("mice", where = asNamespace("mice"))),
          add = TRUE)

  imps <- withCallingHandlers(
    gFormulaMI::gFormulaImpute(
      data            = mids_stratum,
      M               = M,
      trtVars         = trt_vars,
      trtRegimes      = regimes,
      method          = method_vector,
      predictorMatrix = predictor_matrix,
      silent          = TRUE
    ),
    warning = function(w) {
      if (grepl("^Number of logged events", conditionMessage(w))) {
        rlang::cnd_muffle(w)
      }
    }
  )

  if (!is.null(mids_stratum$m) && mids_stratum$m != M) {
    message("run_em_stratum: gFormulaImpute reset M to ", mids_stratum$m,
            " (requested ", M, "): M is bound to the number of imputations in ",
            "mids_stratum. Change m in run_mice_strata() to change it.")
  }

  n_mice_calls  <- length(log_env$calls)
  logged_events <- purrr::keep(log_env$calls, \(le) !is.null(le) && nrow(le) > 0)

  if (length(logged_events) == 0) {
    message("run_em_stratum: no logged events in ", n_mice_calls, " mice() call(s).")
  } else {
    tally <- logged_events |>
      purrr::list_rbind() |>
      dplyr::count(dep, meth, out, sort = TRUE)

    message("run_em_stratum: ", length(logged_events), " of ", n_mice_calls,
            " mice() call(s) logged events for ", strata_label, " = ", stratum_label,
            ". dep | meth | out (n calls):")
    purrr::pwalk(tally, \(dep, meth, out, n)
                 message("  ", dep, " | ", meth, " | ", out, "  (", n, ")"))
  }

  # Step 3: fit marginal and difference models across synthetic datasets
  fits_marginal <- imps %$% lm(reformulate("0 + factor(regime)", outcome_final))
  fits_diff     <- imps %$% lm(reformulate("factor(regime)",     outcome_final))

  # imps is M x 2^n_waves stacked datasets. Writing it out is opt-in: within a
  # stratum it is smaller than the pooled run, but still the largest object here.
  if (!is.null(imps_path)) saveRDS(imps, imps_path)

  rm(imps); gc()

  regime_labels <- tibble::tibble(
    intervention = purrr::map_chr(regimes, paste, collapse = "-")
  )

  # `term` is kept rather than dropped: in the difference fit row 1 is the intercept
  # (the mean under the reference regime) and rows 2+ are contrasts against it, so
  # the intervention label on row 1 does not mean the same thing as on the others.
  pool_and_label <- function(fits) {
    gFormulaMI::syntheticPool(fits) |>
      tibble::as_tibble(rownames = "term") |>
      dplyr::transmute(
        term,
        mi_effect = Estimate,
        mi_se     = sqrt(Total),
        mi_ll     = `95% CI L`,
        mi_ul     = `95% CI U`
      ) |>
      dplyr::bind_cols(regime_labels) |>
      dplyr::mutate(strata_var = strata_label, stratum = stratum_label)
  }

  # Step 4: pool results; release fit lists immediately after
  res <- list(
    marginal = pool_and_label(fits_marginal),
    diff     = pool_and_label(fits_diff)
  )

  rm(fits_marginal, fits_diff); gc()

  if (!is.null(cache_path)) saveRDS(res, cache_path)
  res
}
