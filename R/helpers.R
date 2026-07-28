# --------------
# Helpers
# --------------

## Make wide helper for pivoting dataset in a G-formula via MI compatible format

# mediators sits after ... so it can only be matched by full name, never positionally
make_wide <- function(df, id_col, time_col, base_cols, outcome, ..., mediators = NULL, waves = NULL) {
  
  library(rlang)
  library(dplyr)
  library(tidyr)
  library(stringr)

  outcome_name <- as_name(ensym(outcome))
  
  t_conf <- enquos(...)
  
  if (!is.null(waves)) {
    df <- df |> dplyr::filter({{time_col}} %in% waves)
  }

  t_max <- max(df |> dplyr::pull({{time_col}}))
  t_min <- min(df |> dplyr::pull({{time_col}}))

  df_out <- df |>
    dplyr::select({{id_col}}, {{time_col}}, {{base_cols}}, !!!t_conf, {{mediators}}, {{outcome}}) |>
    tidyr::pivot_wider(
      id_cols = c({{id_col}}, {{base_cols}}),
      names_from = {{time_col}},
      values_from = -c({{id_col}}, {{time_col}}, {{base_cols}})) |>
    dplyr::select(
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

  attr(df_out, "baseline_vars") <- names(dplyr::select(df_out, {{base_cols}}))
  attr(df_out, "time_lagged")   <- names(dplyr::select(df_out, contains("lagged")))

  # Everything passed through ... that is not a lagged variable: same-wave
  # time-varying confounders such as age_dv and gor_dv_fact. Without this they
  # match no node class in make_counterfactual_matrix() and are silently left
  # with no arrows at all. The exposure also arrives via ... and is separated
  # out downstream, once set_exposure() has tagged it.
  tv_names <- setdiff(names(dplyr::select(df, !!!t_conf)),
                      str_subset(names(dplyr::select(df, !!!t_conf)), "lagged"))

  attr(df_out, "time_varying") <- if (length(tv_names)) {
    str_subset(names(df_out), str_glue("^(", str_flatten(str_escape(tv_names), "|"), ")_\\d+$"))
  } else {
    character(0)
  }

  attr(df_out, "outcome_vars") <- str_subset(names(df_out), str_glue("^", str_escape(outcome_name), "_\\d+$"))
  attr(df_out, "outcome_final") <- as.character(str_glue(outcome_name, "_", t_max))
  attr(df_out, "outcome_baseline") <- as.character(str_glue(outcome_name, "_base"))
  attr(df_out, "time_points") <- length(unique(df |> dplyr::pull({{time_col}})))
  med_names <- names(dplyr::select(df, {{mediators}}))

  attr(df_out, "mediators") <- if (length(med_names)) {
    str_subset(names(df_out), str_glue("^(", str_flatten(str_escape(med_names), "|"), ")_\\d+$"))
  } else {
    character(0)
  }

  
  return(df_out)
}

## Helper to tag the exposure column(s) so downstream code (predictor matrix) knows which nodes are treatment.
set_exposure <- function(df, exposure) {
  require(rlang)
  require(stringr)
  exposure <- as_name(ensym(exposure))
  exposure_vars <- str_subset(names(df), str_glue("^", str_escape(exposure), "_\\d+$"))
  if (length(exposure_vars) == 0L) stop("no exposure columns matched '", exposure, "_<wave>'")
  attr(df, "exposure_vars") <- exposure_vars
  return(df)
}

## function to replicate DAG arrows (counterfactual imputation for gFormulaMI):
## expands a node-level arrows table across however many waves the data carries

make_counterfactual_matrix <- function(return_vals, arrows = NULL) {
  library(stringr)

  ## DAG arrows
  if (is.null(arrows)) {
    arrows <- tibble::tribble(
      ~from,                ~to,                  ~lag,
      "inv",                "var",                0,
      "inv",                "exp",                0,
      "inv",                "log_income",         0,
      "inv",                "econ_dist_bin_fact", 0,
      "inv",                "outcome",            0,
      "tv",                 "exp",                0,
      "tv",                 "log_income",         0,
      "tv",                 "econ_dist_bin_fact", 0,
      "tv",                 "outcome",            0,
      "var",                "exp",                0,
      "var",                "log_income",         0,
      "var",                "econ_dist_bin_fact", 0,
      "var",                "outcome",            0,
      "exp",                "log_income",         0,
      "exp",                "econ_dist_bin_fact", 0,
      "exp",                "outcome",            0,
      "log_income",         "econ_dist_bin_fact", 0,
      "log_income",         "outcome",            0,
      "econ_dist_bin_fact", "outcome",            0,
      "tv",                 "tv",                 1,
      "var",                "var",                1,
      "exp",                "var",                1,
      "log_income",         "var",                1,
      "econ_dist_bin_fact", "var",                1,
      "outcome",            "var",                1,
      "exp",                "exp",                1,
      "log_income",         "exp",                1,
      "econ_dist_bin_fact", "exp",                1,
      "outcome",            "exp",                1,
      "log_income",         "log_income",         1,
      "econ_dist_bin_fact", "econ_dist_bin_fact", 1,
      "outcome",            "outcome",            1,
      "outcome",            "log_income",         1,
    )
  }

  baseline_vars <- attr(return_vals, "baseline_vars")
  time_lagged <- attr(return_vals, "time_lagged")
  outcome_vars <- attr(return_vals, "outcome_vars")
  outcome_baseline <- attr(return_vals, "outcome_baseline")
  time_points <- attr(return_vals, "time_points")
  mediators <- attr(return_vals, "mediators")
  exposure <- attr(return_vals, "exposure_vars")

  # same-wave time-varying confounders (age_dv, gor_dv_fact): tagged by
  # make_wide() from ..., minus the exposure, which arrives the same way but is
  # its own node. NULL for wide data built before this attribute existed.
  time_varying <- setdiff(attr(return_vals, "time_varying"), exposure)

  n_vars <- length(return_vals) + 1

  p_mat <- matrix(0, ncol = n_vars, nrow = n_vars)

  nodes <- c(colnames(return_vals), "regime")
  dimnames(p_mat) <- list(nodes, nodes)

  med_stems <- unique(str_remove(mediators, "_\\d+$"))
  unresolved <- setdiff(unique(c(arrows$from, arrows$to)),
                        c("inv", "tv", "var", "exp", "outcome", med_stems))
  if (length(unresolved)) {
    stop("arrow nodes not resolvable from attributes: ", toString(unresolved),
         " (mediator stems available: ", toString(med_stems), ")")
  }

  # baseline columns that are the wave-0 value of a time-varying confounder
  # (age_dv_base, gor_dv_fact_base, age_dv_sq_base).
  tv_stems <- unique(str_remove(time_varying, "_\\d+$"))
  tv_base  <- intersect(paste0(tv_stems, "_base"), baseline_vars)

  inv <- setdiff(baseline_vars, c(outcome_baseline, tv_base))

  # columns of a DAG node at wave t
  cols_of <- function(node, t) {
    switch(node,
      inv     = inv,
      tv      = str_subset(time_varying, str_glue("_", t, "$")),
      var     = str_subset(time_lagged,  str_glue("_", t, "$")),
      exp     = str_subset(exposure,     str_glue("_", t, "$")),
      outcome = str_subset(outcome_vars, str_glue("_", t, "$")),
      str_subset(mediators, str_glue("^", str_escape(node), "_", t, "$"))
    )
  }

  # gFormulaMI draws counterfactuals using Monte-Carlo like drawing instead of FCS
  # (maxit = 1 harcoded inside the package), so a lower-triangular predictor matrix
  # across baseline vars allows gFormulaImpute to impute time-invariant vars by the rule
  # of chain probabilities.
  tinv <- baseline_vars[order(match(baseline_vars, colnames(return_vals)))]
  for (i in seq_along(tinv)[-1]) p_mat[tinv[i], tinv[seq_len(i - 1)]] <- 1

  # expand each arrow across waves
  for (t in 0:(time_points - 1)) {
    for (i in seq_len(nrow(arrows))) {
      self_lag <- arrows$from[i] == arrows$to[i] && arrows$lag[i] == 1
      if (self_lag && t > 0) {
        stems <- intersect(
          str_remove(cols_of(arrows$to[i], t),       str_glue("_", t, "$")),
          str_remove(cols_of(arrows$from[i], t - 1), str_glue("_", t - 1, "$"))
        )
        if (length(stems)) {
          p_mat[cbind(paste0(stems, "_", t), paste0(stems, "_", t - 1))] <- 1
        }
      } else {
        parents <- if (arrows$lag[i] == 0 || arrows$from[i] == "inv") {
          cols_of(arrows$from[i], t)
        } else if (t == 0) {
          if (arrows$from[i] == "outcome") outcome_baseline else character(0)
        } else {
          cols_of(arrows$from[i], t - 1)
        }
        p_mat[cols_of(arrows$to[i], t), parents] <- 1
      }
    }
  }
 
  if (length(tv_base)) {
    t_first <- 0
    for (stem in tv_stems) {
      base_col <- paste0(stem, "_base")
      first_col <- str_subset(time_varying, str_glue("^", str_escape(stem), "_", t_first, "$"))
      if (base_col %in% tv_base && length(first_col)) p_mat[first_col, base_col] <- 1
    }
    for (t in 0:(time_points - 1)) {
      var_t <- cols_of("var", t)
      if (length(var_t)) p_mat[var_t, tv_base] <- 1
    }
  }

  # regime node is predicted by all other variables (i.e., all confounders and the outcome)
  p_mat["regime", 1:(n_vars - 1)] <- 1

  return(p_mat)
}

## imputation methods for the gFormuaMI imputation. Derived from the methods used in observed data.

make_counterfactual_method <- function(return_vals) {
  probe <- rbind(as.data.frame(return_vals), NA)

  method <- mice::make.method(probe,
                              defaultMethod = c("pmm", "logreg", "pmm", "pmm"))

  # treatment columns are assigned from the regime rather than imputed, and are
  # complete in the observed block, so gFormulaMI expects them blank
  exposure <- attr(return_vals, "exposure_vars")
  if (!is.null(exposure)) method[exposure] <- ""

  return(method)
}