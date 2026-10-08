# Effect-modification forest plot: every stratum's contrasts against the always-employed
# regime, one figure per wave window. Rows are the modifiers, columns the outcomes, and
# within a regime the strata of a modifier sit side by side. The colours are the Okabe-Ito
# set of the conference figures (R/conference/fig_meta_pooled.R), extended by the two
# Okabe-Ito hues that figure did not need.

make_em_graph <- function(em_results, stratum_labels,
                          modifier_labels = c(sex = "Sex", race = "Race", hiqual = "Education"),
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

  # stratum_labels fixes the order of the legend and of the strata within a regime. The
  # shape follows a stratum's place within its modifier, so the first-listed stratum of
  # every modifier (its reference in em_spec) is a circle. The shapes take a fill: a
  # contrast drawn without its CI is hollow.
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
  df <- df |>
    mutate(
      regime   = str_replace_all(intervention, c("0" = "E", "1" = "U")),
      ci_drawn = !is.na(mi_df) & mi_df >= min_df & is.finite(mi_ll) & is.finite(mi_ul),
      mi_ll    = if_else(ci_drawn, mi_ll, NA_real_),
      mi_ul    = if_else(ci_drawn, mi_ul, NA_real_),
      fill     = if_else(ci_drawn, unname(colours[stratum]), "white")
    )

  hidden <- filter(df, !ci_drawn)
  if (nrow(hidden)) {
    message("make_em_graph(): no 95% CI drawn for ", nrow(hidden), " contrast(s) with ",
            "mi_df < ", min_df, " or no interval, shown as hollow markers: ",
            toString(sprintf("%s/%s/%s/%s %s (df %s)", hidden$outcome, hidden$window,
                             hidden$modifier, hidden$stratum, hidden$regime,
                             signif(hidden$mi_df, 2))))
  }

  df <- df |>
    mutate(
      outcome  = factor(unname(outcome_labels[outcome]), levels = unname(outcome_labels)),
      modifier = factor(unname(modifier_labels[modifier]), levels = unname(modifier_labels)),
      stratum  = factor(unname(stratum_labels[stratum]), levels = strata$label),
      # sorted, so the always-unemployed regime tops the axis as in the original figure
      regime   = factor(regime, levels = sort(unique(regime)))
    )

  dodge <- position_dodge(width = 0.7, reverse = TRUE)

  plots <- lapply(setNames(nm = unique(df$window)), \(w) {
    d <- filter(df, window == w)

    # the reference regime of the diff fit: every wave employed
    n_waves <- str_count(as.character(d$regime[1]), "-") + 1L
    ref     <- paste(rep("E", n_waves), collapse = "-")

    ggplot(d, aes(mi_effect, regime, xmin = mi_ll, xmax = mi_ul,
                  colour = stratum, shape = stratum, group = stratum)) +
      geom_vline(xintercept = 0, linetype = 2, colour = "grey55") +
      geom_errorbar(width = 0.25, position = dodge, na.rm = TRUE) +
      geom_point(aes(fill = fill), size = 1.8, stroke = 0.7, position = dodge) +
      facet_grid(modifier ~ outcome, scales = "free_x") +
      scale_colour_manual(values = stratum_colours, name = "Stratum") +
      scale_shape_manual(values = stratum_shapes, name = "Stratum") +
      scale_fill_identity() +
      # colour and shape merge into one legend; its keys are filled once, on the colour
      # guide, since the merge keeps a single override.aes and warns about the other
      guides(colour = guide_legend(nrow = 1, override.aes = list(fill = unname(stratum_colours))),
             shape  = guide_legend(nrow = 1)) +
      labs(x = paste0("Estimated mean difference vs always employed (", ref, ")"),
           y = "Intervention strategy",
           caption = if (!all(d$ci_drawn)) {
             paste0("Hollow markers: no 95% CI drawn (pooling df below ", min_df,
                    ", or no interval).")
           }) +
      theme_bw() +
      theme(legend.position = "bottom",
            strip.text.y    = element_text(angle = 0),
            plot.caption    = element_text(colour = "grey35", hjust = 0))
  })

  if (!is.null(save_dir)) {
    for (w in names(plots)) {
      ggsave(file.path(save_dir, paste0("graph_em_", w, ".png")),
             plots[[w]], dpi = 300, width = width, height = height)
    }
  }

  plots
}
