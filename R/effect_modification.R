# Effect modification by Sex, Age group and Education (three-wave model).
#
# Ported from reports/20_effect_mod.qmd into pipeline functions. The analysis is
# the three-wave g-formula run *separately within strata*, so run_em_stratum()
# (R/helpers.R) does the heavy lifting; the functions here build the stratified
# data, the two predictor matrices, orchestrate the per-stratum runs, test for
# interaction, and draw the forest plots.
#
# Machine-specific readRDS/saveRDS caching from the notebook is intentionally
# dropped: {targets} memoises each target, and run_em_stratum()'s mandatory
# cache_path/imps_path are routed to tempfile() so nothing persists outside the
# targets store.

# ---- Strata definitions -----------------------------------------------------
# One row per (modifier, stratum). `label` is the modifier name used by the
# interaction test; `col`/`val` select the stratum in the imputed data.
em_strata_spec <- function() {
  list(
    list(col = "sex_dv_base", val = "Female",          label = "Sex",       stratum = "Female"),
    list(col = "sex_dv_base", val = "Male",            label = "Sex",       stratum = "Male"),
    list(col = "age_group",   val = "Younger (25-40)", label = "Age group", stratum = "Younger (25-40)"),
    list(col = "age_group",   val = "Older (41-65)",   label = "Age group", stratum = "Older (41-65)"),
    list(col = "edu_group",   val = "Higher",          label = "Education", stratum = "Higher"),
    list(col = "edu_group",   val = "Medium",          label = "Education", stratum = "Medium"),
    list(col = "edu_group",   val = "Lower",           label = "Education", stratum = "Lower")
  )
}

# ---- 1. Long data with strata + lags (shared by MCS and PCS) -----------------
# Same eligibility as the three-wave model (observed at waves 2 and 5), plus the
# baseline-derived modifiers age_group / edu_group. Returns a data.table.
build_em_long <- function(data) {
  if (!data.table::is.data.table(data)) stop("data must be a data.table")

  eligible_pidp <- intersect(
    data[wave == 2 & response == 1, pidp],
    data[wave == 5 & response == 1, pidp]
  )
  pop_DT <- data[pidp %in% eligible_pidp]

  long_data <- pop_DT[, `:=`(
    t0                   = t0 - 2L,
    pcs_lagged           = data.table::shift(sf12pcs_dv,    type = "lag"),
    mcs_lagged           = data.table::shift(sf12mcs_dv,    type = "lag"),
    lagged_dnc           = data.table::shift(dnc,           type = "lag"),
    lagged_home_owner    = data.table::shift(home_owner,    type = "lag"),
    lagged_econ_benefits = data.table::shift(econ_benefits, type = "lag"),
    lagged_mastat_dv     = data.table::shift(mastat_dv,     type = "lag")
  )][wave %in% 3:5]

  # drop people with >=2 missing outcomes AND a fully missing baseline MCS
  no_mh_outcomes <- long_data[,
    .(n_missing = sum(is.na(sf12mcs_dv)), base_missing = all(is.na(sf12mcs_dv_base))),
    by = pidp
  ][n_missing >= 2 & base_missing == TRUE, .(pidp)]
  long_data <- long_data[!no_mh_outcomes, on = .(pidp)]

  # baseline-derived, time-invariant modifiers
  long_data[, age_group := factor(
    data.table::fifelse(age_dv_base <= 40, "Younger (25-40)", "Older (41-65)")
  )]
  # preproc_data() already collapses hiqual codes into hiqual_dv_fact_base
  # (High/Medium/Low); relabel to the stratum names used by em_strata_spec()
  long_data[, edu_group := factor(data.table::fcase(
    hiqual_dv_fact_base == "High",   "Higher",
    hiqual_dv_fact_base == "Medium", "Medium",
    hiqual_dv_fact_base == "Low",    "Lower",
    default = NA_character_
  ), levels = c("Higher", "Medium", "Lower"))]

  long_data[]
}

# ---- 2. Wide format for one outcome -----------------------------------------
# Age and region are baseline confounders (age_dv_base / gor_dv_fact_base),
# matching build_data(); age_group / edu_group ride along in base_cols so they
# survive into the imputed object for later stratum filtering.
make_em_wide <- function(long_data, outcome = c("MCS", "PCS")) {
  outcome <- rlang::arg_match(outcome)
  ld <- dplyr::mutate(long_data, econ_emp_bin = haven::as_factor(econ_emp_bin))

  if (outcome == "MCS") {
    wide <- make_wide(
      ld, pidp, t0,
      base_cols = c(sex_dv_base, race_base, hiqual_dv_fact_base,
                    age_dv_base, gor_dv_fact_base,
                    age_group, edu_group, sf12mcs_dv_base),
      outcome = sf12mcs_dv,
      pcs_lagged, econ_emp_bin, log_income, lagged_dnc, lagged_home_owner,
      lagged_econ_benefits, lagged_mastat_dv,
      econ_dist,
      static = FALSE, waves = c(0, 1, 2)
    )
  } else {
    wide <- make_wide(
      ld, pidp, t0,
      base_cols = c(sex_dv_base, race_base, hiqual_dv_fact_base,
                    age_dv_base, gor_dv_fact_base,
                    age_group, edu_group, sf12pcs_dv_base),
      outcome = sf12pcs_dv,
      mcs_lagged, econ_emp_bin, log_income, lagged_dnc, lagged_home_owner,
      lagged_econ_benefits, lagged_mastat_dv,
      econ_dist,
      static = FALSE, waves = c(0, 1, 2)
    )
  }
  wide
}

# ---- 3. MICE for the stratified analysis ------------------------------------
# Age and region enter as baseline vars (age_dv_base / gor_dv_fact_base), as in
# build_data(). gor_dv_fact_base is imputed and used as a predictor like any
# other baseline confounder; age_dv_base has both its row and column zeroed
# alongside the strata variables because age_group is derived from it before
# imputation (imputing one but not the other would make the strata inconsistent).
run_em_mice <- function(wide_data, outcome = c("MCS", "PCS"),
                        m = 50, maxit = 15, seed = 20260410) {
  outcome <- rlang::arg_match(outcome)
  pred_mat <- mice::make.predictorMatrix(wide_data)

  for (v in c("age_dv_base", "age_group", "edu_group")) {
    pred_mat[v, ] <- 0
    pred_mat[, v] <- 0
  }

  mids <- mice::mice(
    wide_data,
    predictorMatrix = pred_mat,
    defaultMethod   = c("norm", "logreg", "pmm", "pmm"),
    m               = m,
    maxit           = maxit,
    seed            = seed
  )

  le <- mids$loggedEvents
  if (is.null(le) || nrow(le) == 0) {
    message("run_em_mice (", outcome, "): no logged events.")
  } else {
    message("run_em_mice (", outcome, "): ", nrow(le), " logged event(s):")
    message(paste(utils::capture.output(print(le)), collapse = "\n"))
  }
  mids
}

# ---- 4. gFormulaMI counterfactual predictor matrix --------------------------
# Encodes the three-wave DAG (same structure as 16_three_waves_final), then zeroes
# the strata columns so they never enter the sequential imputation. `ovar` swaps
# the whole block between sf12mcs_dv and sf12pcs_dv.
make_em_predictor_matrix <- function(wide_data, outcome = c("MCS", "PCS")) {
  outcome <- rlang::arg_match(outcome)
  ovar    <- if (outcome == "MCS") "sf12mcs_dv" else "sf12pcs_dv"

  # --- banded starting matrix (base cols predict everything; short-lag band) ---
  n_tvars <- attr(wide_data, "n_tvars")
  n_base  <- attr(wide_data, "n_base")
  n_vars  <- length(wide_data) + 1L
  p <- matrix(0L, n_vars, n_vars,
              dimnames = list(c(colnames(wide_data), "regime"),
                              c(colnames(wide_data), "regime")))
  p[(n_base + 1):n_vars, 1:n_base] <- 1L
  for (n in 1:(n_vars - n_base)) {
    p[n_base + n, max(1L, n_base + n - n_tvars):(n_base + n - 1L)] <- 1L
  }
  p[n_vars, 1:(n_vars - 1L)] <- 1L

  # --- DAG-specific overrides (mirror reports/20_effect_mod.qmd) --------------
  o0 <- paste0(ovar, "_0"); o1 <- paste0(ovar, "_1"); o2 <- paste0(ovar, "_2")
  ob <- paste0(ovar, "_base")

  p[, c("age_group", "edu_group")] <- 0L        # strata never predict

  p[, o0] <- 0L                                 # intermediates don't predict intermediates
  p[, o1] <- 0L
  p[o1, o0] <- 1L                               # each score predicted by its own immediate lag
  p[o2, o1] <- 1L
  p[o1, ob] <- 0L                               # ...not by baseline score
  p[o2, ob] <- 0L

  p[o0, c("econ_emp_bin_1", "log_income_1")] <- 1L  # score_t -> employment/income_{t+1}
  p[o1, c("econ_emp_bin_2", "log_income_2")] <- 1L

  p["econ_dist_0", c("econ_emp_bin_1", "log_income_1")] <- 0L  # distress does not propagate
  p["econ_dist_1", c("econ_emp_bin_2", "log_income_2")] <- 0L

  p["econ_emp_bin_1", o0] <- 1L                 # score_t -> employment_{t+1}
  p["econ_emp_bin_2", o1] <- 1L

  p["regime", ] <- 1L                           # regime predicted by all confounders/outcome
  p["age_group", "regime"] <- 0L                # strata are not imputed from regime
  p["edu_group", "regime"] <- 0L

  p
}

# ---- 5. Stratified run + combine for one outcome -----------------------------
# Runs run_em_stratum() over every stratum and stacks the marginal / difference
# tables, tagged with the outcome label. datasets = M for gFormulaImpute.
run_effect_modification <- function(wide_mids, predictor_matrix,
                                    outcome = c("MCS", "PCS"), datasets = 200) {
  outcome     <- rlang::arg_match(outcome)
  ovar        <- if (outcome == "MCS") "sf12mcs_dv" else "sf12pcs_dv"
  outcome_lab <- if (outcome == "MCS") "Mental Component Score (MCS)"
                 else                  "Physical Component Score (PCS)"

  trt_vars <- c("econ_emp_bin_0", "econ_emp_bin_1", "econ_emp_bin_2")
  regimes  <- asplit(as.matrix(data.table::CJ(0:1, 0:1, 0:1)), 1)
  regime_labels <- tibble::tibble(
    intervention = purrr::map_chr(regimes, ~ paste(.x, collapse = "-"))
  )

  results <- purrr::map(em_strata_spec(), ~ run_em_stratum(
    mids_full        = wide_mids,
    col              = .x$col,
    val              = .x$val,
    predictor_matrix = predictor_matrix,
    outcome_var      = ovar,
    strata_label     = .x$label,
    stratum_label    = .x$stratum,
    trt_vars         = trt_vars,
    regimes          = regimes,
    regime_labels    = regime_labels,
    cache_path       = tempfile(fileext = ".rds"),  # {targets} owns persistence
    imps_path        = tempfile(fileext = ".rds"),
    datasets         = datasets
  ))

  list(
    marginal = dplyr::mutate(purrr::map_df(results, "marginal"), outcome = outcome_lab),
    diff     = dplyr::mutate(purrr::map_df(results, "diff"),     outcome = outcome_lab)
  )
}

# ---- 6. Interaction tests ----------------------------------------------------
# Global (k-1) df Wald chi2 on stratum-specific mean differences, all contrasts
# against the reference (first) stratum; Sigma carries the shared-reference
# covariance. All pairwise deltas + CIs are returned for display. p_int is the
# single global test repeated on each row within the modifier group.
compute_interaction_test <- function(diff_df, strata_var_filter, outcome_label) {
  filtered <- diff_df |>
    dplyr::filter(strata_var == strata_var_filter, outcome == outcome_label) |>
    dplyr::arrange(stratum)

  strata  <- unique(filtered$stratum)
  effects <- filtered$mi_effect
  ses     <- filtered$mi_se
  lls     <- filtered$mi_ll
  uls     <- filtered$mi_ul
  k       <- length(strata)

  c_vec       <- effects[1] - effects[-1]
  Sigma       <- matrix(ses[1]^2, nrow = k - 1, ncol = k - 1)
  diag(Sigma) <- ses[1]^2 + ses[-1]^2
  chi2_stat   <- as.numeric(t(c_vec) %*% solve(Sigma) %*% c_vec)
  df_test     <- k - 1L
  p_int       <- pchisq(chi2_stat, df = df_test, lower.tail = FALSE)

  pairs <- utils::combn(seq_len(k), 2, simplify = FALSE)
  purrr::map_df(pairs, function(idx) {
    i <- idx[1]; j <- idx[2]
    delta    <- effects[i] - effects[j]
    se_delta <- sqrt(ses[i]^2 + ses[j]^2)
    tibble::tibble(
      outcome    = outcome_label,
      strata_var = strata_var_filter,
      stratum_a  = strata[i], stratum_b = strata[j],
      cate_a = effects[i], cate_a_ll = lls[i], cate_a_ul = uls[i],
      cate_b = effects[j], cate_b_ll = lls[j], cate_b_ul = uls[j],
      delta = delta, ci_l = delta - 1.96 * se_delta, ci_u = delta + 1.96 * se_delta,
      df_test = df_test, chi2_stat = chi2_stat, p_int = p_int
    )
  })
}

# Orchestrates compute_interaction_test() over every strategy x modifier x
# outcome, then BH-adjusts the distinct global tests within each outcome family.
test_interaction <- function(mcs_diff, pcs_diff) {
  all_diff <- dplyr::bind_rows(mcs_diff, pcs_diff)

  ref_label        <- paste(rep("0", max(stringr::str_count(all_diff$intervention, "-")) + 1L),
                            collapse = "-")
  treat_strategies <- setdiff(unique(all_diff$intervention), ref_label)
  effect_modifiers <- c("Sex", "Age group", "Education")
  outcome_labels   <- c("Mental Component Score (MCS)", "Physical Component Score (PCS)")

  interaction_results <- dplyr::bind_rows(purrr::map(treat_strategies, function(strat) {
    strat_diff <- dplyr::filter(all_diff, intervention == strat)
    dplyr::bind_rows(purrr::map(outcome_labels, function(out) {
      dplyr::bind_rows(purrr::map(effect_modifiers,
                                  ~ compute_interaction_test(strat_diff, .x, out)))
    })) |> dplyr::mutate(intervention = strat)
  }))

  # BH over the distinct global tests (strategies x modifiers) within each outcome
  bh_adj <- interaction_results |>
    dplyr::distinct(outcome, intervention, strata_var, p_int) |>
    dplyr::group_by(outcome) |>
    dplyr::mutate(q_int = p.adjust(p_int, method = "BH")) |>
    dplyr::ungroup()

  interaction_results |>
    dplyr::left_join(bh_adj, by = c("outcome", "intervention", "strata_var", "p_int"))
}

# ---- 7. Effect-modification forest plots ------------------------------------
# Two figures: (a) the always-unemployed vs always-employed contrast across
# strata; (b) all non-reference regimes by stratum. Writes PNGs to fig_dir and
# returns the plot objects plus their paths (for a format = "file" target).
make_em_graphs <- function(em_mcs, em_pcs, fig_dir = here::here("figs")) {
  all_diff <- dplyr::bind_rows(em_mcs$diff, em_pcs$diff)

  n_waves   <- max(stringr::str_count(all_diff$intervention, "-")) + 1L
  ref_label <- paste(rep("0", n_waves), collapse = "-")
  all_unemp <- paste(rep("1", n_waves), collapse = "-")
  pal       <- unname(colorBlindness::paletteMartin)

  # (a) always-unemployed vs always-employed contrast
  graph_contrast <- all_diff |>
    dplyr::filter(intervention == all_unemp) |>
    dplyr::mutate(
      strata_var = factor(strata_var, levels = c("Sex", "Age group", "Education")),
      stratum    = forcats::fct_rev(factor(stratum))
    ) |>
    ggplot2::ggplot(ggplot2::aes(mi_effect, stratum,
                                 xmin = mi_ll, xmax = mi_ul, colour = strata_var)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_errorbar(width = 0.25) +
    ggplot2::facet_grid(strata_var ~ outcome, scales = "free_y", space = "free_y") +
    ggplot2::scale_colour_manual(values = pal[1:3], guide = "none") +
    ggplot2::labs(x = "Estimated mean difference (always unemployed vs always employed)", y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0))

  # (b) all non-reference regimes by stratum
  strata_levels <- c("Female", "Male", "Younger (25-40)", "Older (41-65)",
                     "Higher", "Medium", "Lower")
  graph_full <- all_diff |>
    dplyr::filter(intervention != ref_label) |>
    dplyr::mutate(
      n_unemp      = factor(stringr::str_count(intervention, "1")),
      intervention = stringr::str_replace_all(intervention, c("0" = "E", "1" = "U")),
      strata_var   = factor(strata_var, levels = c("Sex", "Age group", "Education"))
    ) |>
    ggplot2::ggplot(ggplot2::aes(mi_effect, intervention,
                                 xmin = mi_ll, xmax = mi_ul,
                                 colour = stratum, shape = stratum)) +
    ggplot2::geom_point(size = 1.8, position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::geom_errorbar(width = 0.25, position = ggplot2::position_dodge(width = 0.6)) +
    ggplot2::facet_grid(strata_var ~ outcome, scales = "free_x") +
    ggplot2::scale_colour_manual(values = stats::setNames(pal[1:7], strata_levels), name = "Stratum") +
    ggplot2::scale_shape_manual(
      values = stats::setNames(c(16L, 17L, 16L, 17L, 16L, 17L, 18L), strata_levels),
      name = "Stratum"
    ) +
    ggplot2::labs(x = "Estimated mean difference vs always employed (E-E-E)",
                  y = "Intervention strategy") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   strip.text.y = ggplot2::element_text(angle = 0))

  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  files <- c(
    contrast = file.path(fig_dir, "graph_em_always_unemployed.png"),
    full     = file.path(fig_dir, "graph_em_full_regimes.png")
  )
  ggplot2::ggsave(files[["contrast"]], graph_contrast, dpi = 300, width = 9,  height = 5)
  ggplot2::ggsave(files[["full"]],     graph_full,     dpi = 300, width = 10, height = 8)

  list(contrast = graph_contrast, full = graph_full, files = files)
}
