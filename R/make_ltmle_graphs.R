# Plots for the LTMLE arm.

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

  lvls <- sort(unique(df$intervention))

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


# ltmle_contrasts() now lives in R/pool_ltmle.R: with one ltmleMSM fit per imputation the
# pooled covariance is available, so contrasts are formed after pooling rather than
# within each imputation. Algebraically identical, and it keeps Cov(regime, reference).
make_ltmle_graphs <- function(ltmle_pooled_mcs, ltmle_pooled_pcs,
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
    prep_ltmle_results(ltmle_pooled_mcs$estimates, mcs_label),
    prep_ltmle_results(ltmle_pooled_pcs$estimates, pcs_label)
  )

  diff_df <- bind_rows(
    prep_ltmle_results(ltmle_contrasts(ltmle_pooled_mcs, reference), mcs_label),
    prep_ltmle_results(ltmle_contrasts(ltmle_pooled_pcs, reference), pcs_label)
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
