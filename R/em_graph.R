# Effect-modification forest plot: every stratum's contrasts against the always-employed
# regime, both wave windows in one figure and not pooled. Rows are the modifiers, columns
# the outcomes, and within a regime the strata of a modifier sit side by side, each as a
# pair: its first window filled, its second open. The colours are the Okabe-Ito set of the
# conference figures (R/conference/fig_meta_pooled.R), extended by the two Okabe-Ito hues
# that figure did not need.

make_em_graph <- function(em_results, stratum_labels,
                          modifier_labels = c(sex = "Sex", race = "Race", hiqual = "Education"),
                          # names are the window ids in em_results; NULL keeps the raw ids
                          window_labels = NULL,
                          colours = stats::setNames(
                            grDevices::palette.colors(palette = "Okabe-Ito", names = TRUE)[
                              c("black", "reddishpurple", "skyblue", "vermillion",
                                "bluishgreen", "blue", "orange")],
                            c("male", "female", "white", "nonwhite", "high", "medium", "low")),
                          min_df     = 5,
                          mcs_label  = "Mental Component Score (MCS)",
                          pcs_label  = "Physical Component Score (PCS)",
                          save_dir   = NULL,
                          width = 10, height = 14) {
  library(dplyr)
  library(stringr)
  library(ggplot2)

  needed  <- c("outcome", "window", "modifier", "stratum", "estimand", "term",
               "intervention", "mi_effect", "mi_df", "mi_ll", "mi_ul")
  missing <- setdiff(needed, names(em_results))
  if (length(missing)) {
    stop("make_em_graph(): em_results is missing column(s) ", toString(missing),
         " -- expected the row-bound effect_modification(\"gform\") outputs", call. = FALSE)
  }

  # Only the contrasts. Row 1 of the diff fit is the (Intercept), the mean under the
  # reference regime, and the marginal means sit on another scale altogether.
  df <- filter(em_results, estimand == "diff", term != "(Intercept)")
  if (!nrow(df)) {
    stop("make_em_graph(): em_results holds no diff contrasts to draw", call. = FALSE)
  }

  # A stratum without a label would fall out of the legend, one without a colour out of
  # the plot, and a modifier or outcome without a label would be drawn as NA.
  outcome_labels <- c(mcs = mcs_label, pcs = pcs_label)
  need_entry <- function(have, used, arg, what) {
    gap <- setdiff(unique(used), names(have))
    if (length(gap)) {
      stop("make_em_graph(): ", arg, " has no entry for ", what, " ", toString(gap),
           call. = FALSE)
    }
  }
  need_entry(stratum_labels,  df$stratum,  "stratum_labels",  "stratum")
  need_entry(colours,         df$stratum,  "colours",         "stratum")
  need_entry(modifier_labels, df$modifier, "modifier_labels", "modifier")
  need_entry(outcome_labels,  df$outcome,  "mcs_label/pcs_label", "outcome")

  # window_labels fixes the window order too: the first is drawn filled, the second open.
  if (is.null(window_labels)) window_labels <- setNames(nm = unique(df$window))
  need_entry(window_labels,   df$window,   "window_labels",   "window")
  windows <- intersect(names(window_labels), df$window)
  if (length(windows) > 2) {
    stop("make_em_graph(): ", length(windows), " windows to draw, but a stratum's windows ",
         "are told apart only as filled and open, so at most two fit", call. = FALSE)
  }

  # stratum_labels fixes the order of the legend and of the strata within a regime. The
  # shape follows a stratum's place within its modifier, so the first-listed stratum of
  # every modifier (its reference in em_spec) is a circle. The shapes take a fill, which
  # tells a stratum's two windows apart.
  shape_pool <- c(21, 24, 22, 23, 25)
  strata <- distinct(df, modifier, stratum) |>
    mutate(label = unname(stratum_labels[stratum]),
           order = match(stratum, names(stratum_labels))) |>
    arrange(order) |>
    mutate(shape = shape_pool[row_number()], .by = modifier)
  if (anyNA(strata$shape)) {
    stop("make_em_graph(): a modifier has more than ", length(shape_pool),
         " strata, and there is one shape per stratum", call. = FALSE)
  }
  stratum_colours <- setNames(unname(colours[strata$stratum]), strata$label)
  stratum_shapes  <- setNames(strata$shape, strata$label)

  # A CI is drawn only when syntheticPool's df reaches min_df. A total variance near zero
  # gives a df far below 1, and then an interval of +-Inf or +-1e6 that would set the
  # whole x axis; the fallback for a non-positive total leaves no interval at all.
  # min_df = 0 draws every finite interval, and then only a missing one is left out.
  no_ci <- if (min_df > 0) {
    paste0("pooling df below ", min_df, ", or no interval")
  } else {
    "no interval"
  }
  df <- df |>
    mutate(
      regime   = str_replace_all(intervention, c("0" = "E", "1" = "U")),
      ci_drawn = !is.na(mi_df) & mi_df >= min_df & is.finite(mi_ll) & is.finite(mi_ul),
      mi_ll    = if_else(ci_drawn, mi_ll, NA_real_),
      mi_ul    = if_else(ci_drawn, mi_ul, NA_real_),
      fill     = if_else(window == windows[1], unname(colours[stratum]), "white")
    )

  hidden <- filter(df, !ci_drawn)
  if (nrow(hidden)) {
    message("make_em_graph(): no 95% CI drawn for ", nrow(hidden), " contrast(s) (",
            no_ci, "), shown as a marker without its bar: ",
            toString(sprintf("%s/%s/%s/%s %s (df %s)", hidden$outcome, hidden$window,
                             hidden$modifier, hidden$stratum, hidden$regime,
                             signif(hidden$mi_df, 2))))
  }

  df <- df |>
    mutate(
      outcome  = factor(unname(outcome_labels[outcome]), levels = unname(outcome_labels)),
      modifier = factor(unname(modifier_labels[modifier]), levels = unname(modifier_labels)),
      stratum  = factor(unname(stratum_labels[stratum]), levels = strata$label),
      window   = factor(unname(window_labels[window]), levels = unname(window_labels[windows])),
      # sorted, so the always-unemployed regime tops the axis as in the original figure
      regime   = factor(regime, levels = sort(unique(regime)))
    )

  # Within a regime the groups run stratum-major, so each stratum's windows sit as a pair.
  dodge <- position_dodge(width = 0.85, orientation = "y", reverse = TRUE)

  # the reference regime of the diff fit: every wave employed
  n_waves <- str_count(as.character(df$regime[1]), "-") + 1L
  ref     <- paste(rep("E", n_waves), collapse = "-")

  g <- ggplot(df, aes(mi_effect, regime, xmin = mi_ll, xmax = mi_ul,
                      colour = stratum, shape = stratum,
                      group = interaction(window, stratum))) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey55") +
    geom_errorbar(width = 0.25, position = dodge, na.rm = TRUE) +
    # alpha is held at 1: it only carries the window legend, filled against open
    geom_point(aes(fill = fill, alpha = window), size = 1.3, stroke = 0.6, position = dodge) +
    facet_grid(modifier ~ outcome, scales = "free_x") +
    scale_colour_manual(values = stratum_colours, name = "Stratum") +
    scale_shape_manual(values = stratum_shapes, name = "Stratum") +
    scale_fill_identity() +
    scale_alpha_manual(values = rep(1, length(windows)), name = "Window") +
    # colour and shape merge into one legend; its keys are filled once, on the colour
    # guide, since the merge keeps a single override.aes and warns about the other
    guides(colour = guide_legend(order = 1, nrow = 1,
                                 override.aes = list(fill = unname(stratum_colours))),
           shape  = guide_legend(order = 1, nrow = 1),
           alpha  = guide_legend(order = 2, override.aes = list(
                      shape = 21, colour = "black", size = 2,
                      fill = c("black", "white")[seq_along(windows)]))) +
    labs(x = paste0("Estimated mean difference vs always employed (", ref, ")"),
         y = "Intervention strategy",
         caption = if (!all(df$ci_drawn)) {
           paste0("Markers without a bar: no 95% CI drawn (", no_ci, ").")
         }) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.y    = element_text(angle = 0),
          plot.caption    = element_text(colour = "grey35", hjust = 0))

  if (!is.null(save_dir)) {
    ggsave(file.path(save_dir, "graph_em.png"), g, dpi = 300, width = width, height = height)
  }

  g
}
