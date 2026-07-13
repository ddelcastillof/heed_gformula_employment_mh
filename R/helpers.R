
# Helpers

## G-formula helper for effect modification: running the model in only one strata
### @ pending: add error tracing from mids imputation inside gFormulaMI

run_em_stratum <- function(mids_full, col, val, predictor_matrix,
                           outcome_var, strata_label, stratum_label,
                           trt_vars, regimes, regime_labels, imps_path, cache_path,
                           datasets) {

  if (file.exists(cache_path)) {
    message(paste0("Loading cached: ", basename(cache_path)))
    return(readRDS(cache_path))
  }

  message(paste0("Running gFormulaMI — ", strata_label, " = ", stratum_label))

  # Step 1: filter to stratum (50 imputations x stratum n)
  mids_s <- dplyr::filter(mids_full, .data[[col]] == val)

  # Step 2: synthetic counterfactuals
  imps <- gFormulaImpute(
    mids_s,
    trtVars         = trt_vars,
    trtRegimes      = regimes,
    predictorMatrix = predictor_matrix,
    M               = datasets,
    silent          = TRUE
  )

  # filtered mids no longer needed
  rm(mids_s); gc()

  out_col <- paste0(outcome_var, "_2")

  # Step 3: fit marginal and difference models across synthetic datasets
  fits_marginal <- imps %$% lm(reformulate("0 + factor(regime)", out_col))
  fits_diff     <- imps %$% lm(reformulate("factor(regime)",     out_col))

  # saving imps before dropping to release memory
  saveRDS(imps, imps_path)

  # surface any logged imputation events before dropping the object
  if (!is.null(imps$loggedEvents) && nrow(imps$loggedEvents) > 0) {
    message(paste0("  [loggedEvents] ", strata_label, " = ", stratum_label,
                   " (", nrow(imps$loggedEvents), " events):"))
    print(imps$loggedEvents)
  }

  rm(imps); gc()

  pool_and_label <- function(fits) {
    syntheticPool(fits) |>
      as_tibble(rownames = "row") |>
      transmute(
        mi_effect = Estimate,
        mi_se     = sqrt(Total),
        mi_ll     = `95% CI L`,
        mi_ul     = `95% CI U`
      ) |>
      bind_cols(regime_labels) |>
      mutate(strata_var = strata_label, stratum = stratum_label)
  }

  # Step 4: pool results; release fit lists immediately after
  res <- list(
    marginal = pool_and_label(fits_marginal),
    diff     = pool_and_label(fits_diff)
  )

  rm(fits_marginal, fits_diff); gc()

  saveRDS(res, cache_path)
  res
}

## Make wide helper for pivoting dataset in a G-formula via MI compatible format

make_wide <- function(df, id_col, time_col, base_cols, outcome, ..., static = FALSE, waves = NULL, roles = NULL) {
  
  require(rlang)
  require(dplyr)
  require(tidyr)

  outcome_name <- as_name(ensym(outcome))
  
  t_conf <- enquos(...)
  
  if (!is.null(waves)) {
    df <- df |> dplyr::filter({{time_col}} %in% waves)
  }

  t_max <- max(df |> dplyr::pull({{time_col}}))
  t_min <- min(df |> dplyr::pull({{time_col}}))

  df_out <- df |>
    dplyr::select({{id_col}}, {{time_col}}, {{base_cols}}, !!!t_conf, {{outcome}}) |>
    tidyr::pivot_wider(
      id_cols = c({{id_col}}, {{base_cols}}),
      names_from = {{time_col}},
      values_from = -c({{id_col}}, {{time_col}}, {{base_cols}})) |>
    dplyr::select(
#      {{id_col}},
      {{base_cols}},
      ends_with("0"),
      ends_with("1"),
      ends_with("2"),
      ends_with("3"),
      ends_with("4"),
      ends_with("5"),
      ends_with("6"),
      ends_with("7"),
      ends_with("8"),
      ends_with("9")
    )

  if (static) {
      intermediate_cols <- paste0(outcome_name, "_", (t_min + 1):(t_max - 1))
      df_out <- df_out |> dplyr::select(-any_of(intermediate_cols))
    }

  attr(df_out, "n_tvars") <- length(t_conf)
  attr(df_out, "n_base") <- length(df[1,] |> dplyr::select({{base_cols}}))
  attr(df_out, "baseline_vars") <- names(dplyr::select(df_out, {{base_cols}}))
  attr(df_out, "time_lagged")   <- names(dplyr::select(df_out, contains("lagged")))
  attr(df_out, "outcome_var")   <- paste0(outcome_name, "_", t_max)
  attr(df_out, "outcome_baseline") <- paste0(outcome_name, "_base")
  attr(df_out, "time_points") <- length(unique(df |> dplyr::pull({{time_col}})))
  attr(df_out, "intermediate_vars") <- names(dplyr::select(df_out, !contains("lagged"), -c({{base_cols}})))
  attr(df_out, "dag_roles") <- roles # extra vars like age/region/income/distress

  
  return(df_out)
}

## Helper to tag the exposure column(s) so downstream code (predictor matrix) knows which nodes are treatment.
set_exposure <- function(df, exposure) {
  require(rlang)
  exposure <- as_name(ensym(exposure))
  exposure_vars <- grep(paste0("^", exposure, "_\\d+$"), names(df), value = TRUE)
  if (length(exposure_vars) == 0L) stop("no exposure columns matched '", exposure, "_<wave>'")
  attr(df, "exposure_vars") <- exposure_vars
  return(df)
}

## function to replicate DAG arrows (counterfactual imputation for gFormulaMI)

make_counterfactual_matrix <- function(return_vals) {
  baseline_vars <- attr(return_vals, "baseline_vars")
  time_lagged <- attr(return_vals, "time_lagged")
  outcome_var <- attr(return_vals, "outcome_var")
  outcome_baseline <- attr(return_vals, "outcome_baseline")
  time_points <- attr(return_vals, "time_points")
  intermediate_vars <- attr(return_vals, "intermediate_vars")
  exposure <- attr(return_vals, "exposure_vars")
  roles <- attr(return_vals, "dag_roles")

  n_vars <- length(return_vals) + 1

  p_mat <- matrix(0, ncol = n_vars, nrow = n_vars)

  nodes <- c(colnames(return_vals), "regime")
  dimnames(p_mat) <- list(nodes, nodes)

  # outcome_var attr is the final measurement (e.g. sf12mcs_dv_2); strip its
  # wave index to rebuild the per-wave outcome column names
  outcome_stem <- sub("_\\d+$", "", outcome_var)

  if (is.null(roles)) {
    message("make_counterfactual_matrix: data has no 'dag_roles' attribute ",
            "(pass `roles` to make_wide()); age/region/income/distress ",
            "columns fall back to backward-only predictor rows")
    roles <- list()
  }

  pick <- function(...) intersect(c(...), nodes)
  at_wave <- function(vars, t) vars[endsWith(vars, paste0("_", t))]
  # role stem + wave -> column name; a NULL stem yields character(0), so
  # missing roles drop out of every arrow assignment
  role_at <- function(stem, t) pick(paste0(stem, "_", t))

  # baseline outcome (MH T0) is predicted by all time-invariant confounders
  p_mat[outcome_baseline, setdiff(baseline_vars, outcome_baseline)] <- 1

  for (t in 0:(time_points - 1)) {
    lag_t <- at_wave(time_lagged, t)
    emp_t <- at_wave(exposure, t)
    inc_t <- role_at(roles$income, t)
    dis_t <- role_at(roles$distress, t)
    mh_t  <- role_at(outcome_stem, t)

    age_prev <- role_at(roles$age, t - 1)
    gor_prev <- role_at(roles$region, t - 1)
    lag_prev <- at_wave(time_lagged, t - 1)
    emp_prev <- at_wave(exposure, t - 1)
    inc_prev <- role_at(roles$income, t - 1)
    dis_prev <- role_at(roles$distress, t - 1)
    mh_prev  <- if (t == 0) outcome_baseline else role_at(outcome_stem, t - 1)

    # Age chain; age is otherwise an exogenous root node in the DAG
    p_mat[age_t, age_prev] <- 1

    # region has no DAG node: treat as time-invariant confounder with a chain
    p_mat[gor_t, c(baseline_vars, gor_prev)] <- 1

    # Var T(t) <- Inv, Age T(t), Var chain, Emp/Inc/Dis/MH T(t-1) (MH0 -> Var1)
    p_mat[lag_t, c(baseline_vars, age_t, gor_t, lag_prev,
                   emp_prev, inc_prev, dis_prev, mh_prev)] <- 1

    # Emp T(t) <- Inv, Age T(t), Var T(t), Emp/Inc/Dis T(t-1),
    # MH T(t-1) (treatment-confounder feedback; MH0 -> Emp1)
    p_mat[emp_t, c(baseline_vars, age_t, gor_t, lag_t,
                   emp_prev, inc_prev, dis_prev, mh_prev)] <- 1

    # Inc T(t) <- Inv, Age T(t), Var T(t), Emp T(t), Inc chain, MH T(t-1)
    p_mat[inc_t, c(baseline_vars, age_t, gor_t, lag_t, emp_t,
                   inc_prev, mh_prev)] <- 1

    # Dis T(t) <- Inv, Age T(t), Var T(t), Emp T(t), Inc T(t), Dis chain
    p_mat[dis_t, c(baseline_vars, age_t, gor_t, lag_t, emp_t, inc_t,
                   dis_prev)] <- 1

    # MH T(t) <- Inv, Age T(t), Var T(t), Emp T(t), Inc T(t), Dis T(t), MH chain
    p_mat[mh_t, c(baseline_vars, age_t, gor_t, lag_t, emp_t, inc_t, dis_t,
                  mh_prev)] <- 1

    # any time-varying column without a DAG role above gets a conservative
    # backward-only row so it is never synthesized from an empty model
    leftover_t <- setdiff(at_wave(intermediate_vars, t),
                          c(age_t, gor_t, emp_t, inc_t, dis_t, mh_t))
    if (length(leftover_t) > 0) {
      p_mat[leftover_t, c(baseline_vars, lag_prev, emp_prev, inc_prev,
                          dis_prev, mh_prev)] <- 1
    }
  }

  # regime node is predicted by all other variables (i.e., all confounders and the outcome)
  p_mat["regime", 1:(n_vars - 1)] <- 1

  # gFormulaMI synthesizes columns left to right in a single sweep (maxit = 1),
  # so a variable can only condition on columns synthesized before it or on
  # the treatment columns (pre-filled by the regime, never imputed); drop
  # arrows that point forward in the column order
  ord <- stats::setNames(seq_along(nodes), nodes)
  for (r in setdiff(nodes, "regime")) {
    preds <- nodes[p_mat[r, ] == 1]
    fwd <- setdiff(preds[ord[preds] > ord[r]], exposure)
    p_mat[r, fwd] <- 0
  }

  return(p_mat)
}