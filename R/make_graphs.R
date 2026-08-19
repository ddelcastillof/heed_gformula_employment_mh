# Plots for the gFormulaMI arm.

make_graphs <- function(gform_mcs, gform_pcs,
                        gform_mcs_ate, gform_pcs_ate,
                        mcs_label = "Mental Component Score (MCS)",
                        pcs_label = "Physical Component Score (PCS)",
                        save_dir  = NULL,
                        wave_label = NULL) {
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(colorBlindness)

  # The two panels share every geom, scale and facet; they differ only in the data they
  # read, their x label, and whether the reference strategy stays in. Hold that as data
  # and draw them in one pass.
  panels <- list(
    marginal = list(
      x_lab    = "Estimated marginal mean",
      data     = list(gform_mcs$results, gform_pcs$results),
      drop_ref = FALSE
    ),
    diff = list(
      x_lab    = "Estimated mean difference\n(reference: always employed)",
      data     = list(gform_mcs_ate$results, gform_pcs_ate$results),
      drop_ref = TRUE
    )
  )

  plots <- lapply(panels, function(p) {
    # Regime labels arrive as "0-1-1-0". Recode to "E-U-U-E" for the axis; the count of
    # unemployed periods drives the colour scale.
    df <- bind_rows(Map(
      \(d, lab) mutate(
        d,
        outcome      = lab,
        n_unemp      = factor(str_count(intervention, "1")),
        intervention = str_replace_all(intervention, c("0" = "E", "1" = "U"))
      ),
      p$data, list(mcs_label, pcs_label)
    ))

    # Recode first: the all-employed reference reads "E-E-E" only after the swap, and
    # its contrast against itself is a row of zeros.
    if (p$drop_ref) df <- filter(df, !str_detect(intervention, "^E(-E)*$"))

    ggplot(df, aes(mi_effect, intervention,
                   xmin = mi_ll, xmax = mi_ul, colour = n_unemp)) +
      geom_point(size = 2, position = position_dodge(width = 0.7)) +
      geom_errorbar(width = 0.3, position = position_dodge(width = 0.7)) +
      scale_colour_manual(
        values = unname(paletteMartin),
        name   = "Number of unemployed periods"
      ) +
      facet_wrap(~outcome, nrow = 1, scales = "free_x") +
      labs(x = p$x_lab, y = "Intervention strategy") +
      theme_bw()
  })

  if (!is.null(save_dir)) {
    suffix <- if (!is.null(wave_label)) paste0("_", wave_label) else ""
    for (nm in names(plots)) {
      ggsave(file.path(save_dir, paste0("graph_", nm, suffix, ".png")),
             plots[[nm]], dpi = 300, width = 8, height = 12)
    }
  }

  plots
}
