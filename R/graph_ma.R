# Final meta-analysis figure: the pooled estimates from meta_analysis(), with the
# marginal means and the contrasts side by side.
#
# The two estimands do not behave the same way once the wave-sets are pooled -- a
# level shift common to every strategy cancels inside a contrast but not inside a
# marginal mean -- so the point shape carries the heterogeneity test: a filled
# point pooled cleanly, a hollow one did not and should be read per wave-set
# instead of as a single number.

make_ma_graph <- function(ma_mcs, ma_mcs_ate,
                          ma_pcs = NULL, ma_pcs_ate = NULL,
                          mcs_label  = "Mental Component Score (MCS)",
                          pcs_label  = "Physical Component Score (PCS)",
                          het_alpha  = 0.05,
                          save_dir   = NULL,
                          wave_label = NULL,
                          width = 9, height = 8) {
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(colorBlindness)

  estimand_levels <- c("Marginal mean", "Mean difference\n(ref: always employed)")

  # meta_analysis() returns list(results = , fits = ); a bare tibble is fine too.
  as_results <- function(x) if (is.data.frame(x)) x else x$results

  # One tidy frame per (outcome, estimand) cell that was actually supplied.
  cells <- list(
    list(ma = ma_mcs,     outcome = mcs_label, estimand = estimand_levels[1]),
    list(ma = ma_mcs_ate, outcome = mcs_label, estimand = estimand_levels[2]),
    list(ma = ma_pcs,     outcome = pcs_label, estimand = estimand_levels[1]),
    list(ma = ma_pcs_ate, outcome = pcs_label, estimand = estimand_levels[2])
  )
  cells <- Filter(\(c) !is.null(c$ma), cells)
  if (!length(cells)) {
    stop("make_ma_graph(): no meta_analysis() results supplied", call. = FALSE)
  }

  df <- bind_rows(lapply(cells, \(c) {
    r <- as_results(c$ma)
    needed <- c("intervention", "mi_effect", "mi_ll", "mi_ul", "p_het")
    missing <- setdiff(needed, names(r))
    if (length(missing)) {
      stop("make_ma_graph(): ", c$outcome, " / ", trimws(c$estimand),
           " is missing column(s) ", paste(missing, collapse = ", "),
           " -- expected a meta_analysis() result", call. = FALSE)
    }
    mutate(r, outcome = c$outcome, estimand = c$estimand)
  }))

  # Regimes arrive as "0-1-1-0". Recode to "E-U-U-E" for the axis; the count of
  # unemployed periods drives the colour, as it does in make_graphs().
  df <- df |>
    mutate(
      n_unemp      = factor(str_count(intervention, "1")),
      intervention = str_replace_all(intervention, c("0" = "E", "1" = "U")),
      estimand     = factor(estimand, levels = estimand_levels),
      outcome      = factor(outcome, levels = unique(c(mcs_label, pcs_label))),
      pooled_ok    = p_het >= het_alpha
    )

  # Recode first: the all-employed strategy reads "E-E-E-E" only after the swap.
  # On the contrast estimand that row is the model intercept, not a difference.
  df <- filter(df, !(estimand == estimand_levels[2] &
                       str_detect(intervention, "^E(-E)*$")))

  df <- mutate(df, intervention = factor(intervention,
                                         levels = rev(sort(unique(intervention)))))

  # The null line belongs to the contrast panel only, so carry it as data rather
  # than as a bare geom_vline that would stripe both columns.
  null_line <- distinct(filter(df, estimand == estimand_levels[2]),
                        outcome, estimand) |>
    mutate(x = 0)

  n_het <- sum(!df$pooled_ok)
  model <- if ("effects" %in% names(df)) unique(as.character(df$effects)) else NULL
  subtitle <- paste0(
    "Pooled across wave-sets by meta_analysis()",
    if (length(model) == 1L) paste0(" (", model, " effects)") else ""
  )
  # str_wrap, because ggplot clips a long subtitle at the panel edge rather than
  # reflowing it.
  caption <- if (n_het > 0) {
    str_wrap(paste0(
      "Hollow points: the two wave-sets disagreed at p < ", het_alpha,
      " (", n_het, " of ", nrow(df), " estimates), so the pooled value there ",
      "averages over a real difference and should be read per wave-set instead."
    ), width = 110)
  } else {
    NULL
  }

  g <- ggplot(df, aes(mi_effect, intervention,
                      xmin = mi_ll, xmax = mi_ul, colour = n_unemp)) +
    geom_vline(data = null_line, aes(xintercept = x),
               linetype = 2, colour = "grey55", inherit.aes = FALSE) +
    geom_errorbar(width = 0, linewidth = 0.55) +
    geom_point(aes(shape = pooled_ok), size = 2.4, fill = "white", stroke = 0.9) +
    scale_colour_manual(values = unname(paletteMartin),
                        name = "Number of unemployed periods") +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21),
                       guide = "none") +
    facet_grid(outcome ~ estimand, scales = "free_x") +
    labs(x = "Points on the outcome scale", y = "Intervention strategy",
         subtitle = subtitle, caption = caption) +
    theme_bw(base_size = 12) +
    theme(
      legend.position    = "bottom",
      plot.subtitle      = element_text(colour = "grey35", size = 9.5),
      plot.caption       = element_text(colour = "grey35", size = 8.5,
                                        hjust = 0, margin = margin(t = 10)),
      plot.caption.position = "plot",
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "grey92"),
      strip.background   = element_rect(fill = "grey95", colour = "grey70")
    )

  if (!is.null(save_dir)) {
    suffix <- if (!is.null(wave_label)) paste0("_", wave_label) else ""
    ggsave(file.path(save_dir, paste0("graph_ma", suffix, ".png")),
           g, dpi = 300, width = width, height = height)
  }

  g
}
