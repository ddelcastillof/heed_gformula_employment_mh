make_graphs <- function(comparison_mcs, comparison_pcs, fig_dir = here::here("figs")) {

  required <- c("intervention", "mi_effect", "mi_ll", "mi_ul")
  check_cols <- function(df, what) {
    missing <- setdiff(required, names(df))
    if (length(missing)) {
      stop("make_graphs: ", what, " is missing column(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    df
  }

  # ---- 1. Stack MCS + PCS into one long table, tagged by outcome -------------
  mcs_lab <- "Mental Component Score (MCS)"
  pcs_lab <- "Physical Component Score (PCS)"

  marginal_df <- dplyr::bind_rows(
    dplyr::mutate(check_cols(comparison_mcs$marginal, "comparison_mcs$marginal"), outcome = mcs_lab),
    dplyr::mutate(check_cols(comparison_pcs$marginal, "comparison_pcs$marginal"), outcome = pcs_lab)
  )
  diff_df <- dplyr::bind_rows(
    dplyr::mutate(check_cols(comparison_mcs$ate, "comparison_mcs$ate"), outcome = mcs_lab),
    dplyr::mutate(check_cols(comparison_pcs$ate, "comparison_pcs$ate"), outcome = pcs_lab)
  )

  # ---- 2. Wave count is inferred from the label, never hard-coded ------------
  # "0-0-1" has two dashes -> three waves. Drives the reference regime, the
  # figure sizing, and the axis caption for any 3/4/5-wave run.
  n_waves       <- max(stringr::str_count(marginal_df$intervention, "-")) + 1L
  ref_label_raw <- paste(rep("0", n_waves), collapse = "-")  # "0-0-0"  (always employed)
  ref_label_eu  <- paste(rep("E", n_waves), collapse = "-")  # "E-E-E"

  # ---- 3. Shared regime aesthetics ------------------------------------------
  # n_unemp counts unemployed periods (the "1"s); the label is then recoded to
  # E/U for readability; ends_unemp flags trajectories that finish unemployed.
  # Order matters: n_unemp is read off the raw label before it is overwritten.
  prep <- function(df) {
    dplyr::mutate(
      df,
      n_unemp      = factor(stringr::str_count(intervention, "1")),
      intervention = stringr::str_replace_all(intervention, c("0" = "E", "1" = "U")),
      ends_unemp   = stringr::str_ends(intervention, "U")
    )
  }

  forest <- function(df, x_lab) {
    ggplot2::ggplot(df, ggplot2::aes(mi_effect, intervention,
                                     xmin = mi_ll, xmax = mi_ul,
                                     colour = n_unemp, shape = ends_unemp)) +
      ggplot2::geom_point(size = 2, position = ggplot2::position_dodge(width = 0.7)) +
      ggplot2::geom_errorbar(width = 0.3, position = ggplot2::position_dodge(width = 0.7)) +
      ggplot2::facet_wrap(~outcome, nrow = 1, scales = "free_x") +
      ggplot2::scale_colour_manual(values = unname(colorBlindness::paletteMartin),
                                   name = "Number of unemployed periods") +
      ggplot2::scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 18), guide = "none") +
      ggplot2::labs(x = x_lab, y = "Intervention strategy") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "right")
  }

  # ---- 4. The three figures --------------------------------------------------
  marginal_plot <- forest(prep(marginal_df), "Estimated marginal mean")

  diff_plot <- forest(
    prep(dplyr::filter(diff_df, intervention != ref_label_raw)),
    paste0("Estimated mean difference\n(reference: always employed, ", ref_label_eu, ")")
  ) +
    ggplot2::expand_limits(x = 0)  # keep the null line in view for every wave count

  combined_plot <- cowplot::plot_grid(
    marginal_plot, diff_plot,
    ncol = 1, align = "v", labels = "AUTO"
  )

  # ---- 5. Persist PNGs; height scales with the number of regimes (2^n_waves) --
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  n_regimes <- dplyr::n_distinct(marginal_df$intervention)
  panel_h   <- max(6, 0.35 * n_regimes)
  stub      <- file.path(fig_dir, paste0("graph", n_waves, "waves_"))
  files <- c(
    marginal = paste0(stub, "marginal.png"),
    diff     = paste0(stub, "diff.png"),
    combined = paste0(stub, "combined.png")
  )
  ggplot2::ggsave(files[["marginal"]], marginal_plot, dpi = 300, width = 8,  height = panel_h)
  ggplot2::ggsave(files[["diff"]],     diff_plot,     dpi = 300, width = 8,  height = panel_h)
  ggplot2::ggsave(files[["combined"]], combined_plot, dpi = 300, width = 10, height = 2 * panel_h)

  list(
    marginal = marginal_plot,
    diff     = diff_plot,
    combined = combined_plot,
    files    = files,
    n_waves  = n_waves
  )
}
