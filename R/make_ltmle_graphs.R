# Plots for the LTMLE arm.

make_ltmle_graphs <- function(ltmle_pooled_mcs, ltmle_pooled_pcs,
                              mcs_label = "Mental Component Score (MCS)",
                              pcs_label = "Physical Component Score (PCS)",
                              codes = c("E", "U"),
                              reference = NULL,
                              save_dir  = NULL,
                              wave_label = NULL) {
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(colorBlindness)

  legend_lab <- paste("Number of unemployed periods")

  # The two panels share every geom, scale and facet; they differ only in the data
  # they read and their x label. Hold that difference as data and draw them in one pass.
  panels <- list(
    marginal = list(
      x_lab = "Estimated marginal mean (TMLE)",
      data  = list(ltmle_pooled_mcs$estimates, ltmle_pooled_pcs$estimates)
    ),
    diff = list(
      x_lab = paste("Estimated mean difference (TMLE) (reference: never unemployed)"),
      data  = list(ltmle_contrasts(ltmle_pooled_mcs, reference),
                   ltmle_contrasts(ltmle_pooled_pcs, reference))
    )
  )

  plots <- lapply(panels, function(p) {
    # Regime labels arrive as "0-1-1-0". Recode to "E-U-U-E" for the axis; the count
    # of exposed periods drives the colour scale.
    df <- bind_rows(Map(
      \(d, lab) mutate(
        d,
        outcome      = lab,
        n_exposed    = factor(str_count(intervention, "1")),
        intervention = str_replace_all(intervention, setNames(codes, c("0", "1")))
      ),
      p$data, list(mcs_label, pcs_label)
    ))

    # Level order set after binding both outcomes, so the facets share one y axis.
    df <- mutate(df, intervention = factor(intervention,
                                           levels = sort(unique(intervention))))

    ggplot(df, aes(ltmle_effect, intervention,
                   xmin = ltmle_ll, xmax = ltmle_ul, colour = n_exposed)) +
      geom_point(size = 2, position = position_dodge(width = 0.7)) +
      geom_errorbar(width = 0.3, position = position_dodge(width = 0.7)) +
      scale_colour_manual(
        values = unname(paletteMartin),
        name   = legend_lab
      ) +
      facet_wrap(~outcome, nrow = 1, scales = "free_x") +
      labs(x = p$x_lab, y = "Intervention strategy") +
      theme_bw()
  })

  if (!is.null(save_dir)) {
    suffix <- if (!is.null(wave_label)) paste0("_", wave_label) else ""
    for (nm in names(plots)) {
      ggsave(file.path(save_dir, paste0("graph_ltmle_", nm, suffix, ".png")),
             plots[[nm]], dpi = 300, width = 8, height = 12)
    }
  }

  plots
}
