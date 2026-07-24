prep_gform_results <- function(results, outcome_label) {
  library(dplyr)
  library(stringr)

  results |>
    mutate(
      outcome      = outcome_label,
      n_unemp      = factor(str_count(intervention, "1")),
      intervention = str_replace_all(intervention, c("0" = "E", "1" = "U"))
    )
}


gform_strategy_plot <- function(df, x_lab) {
  library(ggplot2)
  library(colorBlindness)

  ggplot(df, aes(mi_effect, intervention, xmin = mi_ll, xmax = mi_ul, colour = n_unemp)) +
    geom_point(size = 2, position = position_dodge(width = 0.7)) +
    geom_errorbar(width = 0.3, position = position_dodge(width = 0.7)) +
    scale_colour_manual(
      values = unname(paletteMartin),
      name   = "Number of unemployed periods"
    ) +
    facet_wrap(~outcome, nrow = 1, scales = "free_x") +
    labs(x = x_lab, y = "Intervention strategy") +
    theme_bw()
}


make_graphs <- function(gform_mcs, gform_pcs,
                        gform_mcs_ate, gform_pcs_ate,
                        mcs_label = "Mental Component Score (MCS)",
                        pcs_label = "Physical Component Score (PCS)",
                        save_dir  = NULL) {
  library(dplyr)
  library(stringr)
  library(ggplot2)

  # Marginal means: every strategy, both outcomes stacked for faceting.
  marginal_df <- bind_rows(
    prep_gform_results(gform_mcs$results, mcs_label),
    prep_gform_results(gform_pcs$results, pcs_label)
  )

  diff_df <- bind_rows(
    prep_gform_results(gform_mcs_ate$results, mcs_label),
    prep_gform_results(gform_pcs_ate$results, pcs_label)
  ) |>
    filter(!str_detect(intervention, "^E(-E)*$"))

  marginal <- gform_strategy_plot(marginal_df, "Estimated marginal mean")
  diff     <- gform_strategy_plot(
    diff_df,
    "Estimated mean difference\n(reference: always employed)"
  )

  if (!is.null(save_dir)) {
    ggsave(file.path(save_dir, "graph_marginal.png"), marginal,
           dpi = 300, width = 8, height = 12)
    ggsave(file.path(save_dir, "graph_diff.png"), diff,
           dpi = 300, width = 8, height = 12)
  }

  list(marginal = marginal, diff = diff)
}
