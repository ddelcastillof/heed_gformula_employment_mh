# Plots for the LTMLE arm.
#
# TMLE is the sensitivity analysis for gFormulaMI, so both arms intervene on
# econ_emp_bin_fact and the regimes mean the same thing in both figures. These
# are kept separate from make_graphs() only because the pooled columns differ
# (ltmle_effect / ltmle_ll / ltmle_ul rather than mi_*) and because the
# contrasts have to be formed before pooling rather than by a second model fit.

prep_ltmle_results <- function(pooled, outcome_label, codes = c("E", "U")) {
  library(dplyr)
  library(stringr)

  pooled |>
    mutate(
      outcome      = outcome_label,
      n_exposed    = factor(str_count(intervention, "1")),
      intervention = str_replace_all(
        intervention,
        setNames(codes, c("0", "1"))
      )
    )
}


ltmle_strategy_plot <- function(df, x_lab, legend_lab) {
  library(ggplot2)
  library(colorBlindness)

  # read top-to-bottom from never-exposed to always-exposed
  lvls <- df |>
    dplyr::distinct(intervention, n_exposed) |>
    dplyr::arrange(dplyr::desc(n_exposed), dplyr::desc(intervention)) |>
    dplyr::pull(intervention)

  df <- dplyr::mutate(df, intervention = factor(intervention, levels = lvls))

  ggplot(df, aes(ltmle_effect, intervention,
                 xmin = ltmle_ll, xmax = ltmle_ul, colour = n_exposed)) +
    geom_point(size = 2, position = position_dodge(width = 0.7)) +
    geom_errorbar(width = 0.3, position = position_dodge(width = 0.7)) +
    scale_colour_manual(
      values = unname(paletteMartin),
      name   = legend_lab
    ) +
    facet_wrap(~outcome, nrow = 1, scales = "free_x") +
    labs(x = x_lab, y = "Intervention strategy") +
    theme_bw()
}


# Difference of every regime from a reference regime, formed within each
# imputation and returned in the shape pool_ltmle() consumes.
#
# The point estimates are exact. The variances are NOT: fit_ltmle_imp() discards
# the influence curves, so Cov(regime, reference) is unavailable and the two
# variances are simply summed. Regimes are estimated on the same subjects, so
# that covariance is expected to be positive and the resulting intervals are
# therefore conservative -- which cuts against ltmle's own warning that the IC
# variance is anticonservative under positivity violations. Treat the widths as
# approximate either way.
ltmle_contrasts <- function(ltmle_summaries, reference = NULL) {
  library(dplyr)

  if (is.null(reference)) {
    n_nodes   <- length(strsplit(ltmle_summaries$intervention[1], "-", fixed = TRUE)[[1]])
    reference <- paste(rep(0, n_nodes), collapse = "-")
  }
  if (!reference %in% ltmle_summaries$intervention) {
    stop("ltmle_contrasts(): reference regime '", reference,
         "' is not among the fitted regimes", call. = FALSE)
  }

  ref <- ltmle_summaries |>
    filter(intervention == reference) |>
    select(imp_idx, ref_estimate = estimate, ref_variance = variance)

  ltmle_summaries |>
    filter(intervention != reference) |>
    inner_join(ref, by = "imp_idx") |>
    mutate(
      estimate = estimate - ref_estimate,
      variance = variance + ref_variance
    ) |>
    select(intervention, imp_idx, estimate, variance)
}


# ltmle_sum_* are the per-imputation summary tibbles produced by fit_ltmle_imp(),
# not the pooled tables: the contrasts have to be formed before pooling.
make_ltmle_graphs <- function(ltmle_sum_mcs, ltmle_sum_pcs,
                              mcs_label = "Mental Component Score (MCS)",
                              pcs_label = "Physical Component Score (PCS)",
                              exposure_label = "unemployed",
                              reference = NULL,
                              save_dir  = NULL,
                              wave_label = NULL) {
  library(dplyr)
  library(ggplot2)

  legend_lab <- paste0("Number of ", exposure_label, " periods")

  marginal_df <- bind_rows(
    prep_ltmle_results(pool_ltmle(ltmle_sum_mcs), mcs_label),
    prep_ltmle_results(pool_ltmle(ltmle_sum_pcs), pcs_label)
  )

  diff_df <- bind_rows(
    prep_ltmle_results(pool_ltmle(ltmle_contrasts(ltmle_sum_mcs, reference)), mcs_label),
    prep_ltmle_results(pool_ltmle(ltmle_contrasts(ltmle_sum_pcs, reference)), pcs_label)
  )

  marginal <- ltmle_strategy_plot(marginal_df, "Estimated marginal mean (TMLE)", legend_lab)
  diff     <- ltmle_strategy_plot(
    diff_df,
    paste0("Estimated mean difference (TMLE)\n(reference: never ", exposure_label, ")"),
    legend_lab
  )

  if (!is.null(save_dir)) {
    suffix        <- if (!is.null(wave_label)) paste0("_", wave_label) else ""
    marginal_file <- paste0("graph_ltmle_marginal", suffix, ".png")
    diff_file     <- paste0("graph_ltmle_diff", suffix, ".png")
    ggsave(file.path(save_dir, marginal_file), marginal,
           dpi = 300, width = 8, height = 12)
    ggsave(file.path(save_dir, diff_file), diff,
           dpi = 300, width = 8, height = 12)
  }

  list(marginal = marginal, diff = diff)
}
