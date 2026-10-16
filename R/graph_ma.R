# Final meta-analysis figure: the pooled estimates from meta_analysis(), with the
# marginal means and the contrasts side by side. Pass `windows` to draw the two
# per-window gFormulaMI estimates in grey above each pooled one.

make_ma_graph <- function(ma_mcs, ma_mcs_ate,
                          ma_pcs = NULL, ma_pcs_ate = NULL,
                          # Named by window label, each holding the run_gform()
                          # results that meta_analysis() pooled, keyed by cell:
                          # list("Waves 3-6" = list(mcs = , mcs_ate = , pcs = , pcs_ate = ), ...)
                          windows    = NULL,
                          mcs_label  = "Mental Component Score (MCS)",
                          pcs_label  = "Physical Component Score (PCS)",
                          het_alpha  = 0.05,
                          save_dir   = NULL,
                          wave_label = NULL,
                          width = 9, height = 8) {
  library(dplyr)
  library(stringr)
  library(ggplot2)

  estimand_levels <- c("Marginal mean", "Mean difference")
  pooled_label    <- "Pooled"

  count_cols  <- unname(grDevices::palette.colors(palette = "Okabe-Ito", names = TRUE)[
    c("black", "reddishpurple", "skyblue", "vermillion", "bluishgreen")])
  window_cols <- c("grey35", "grey62")

  if (length(windows) && (length(windows) > length(window_cols) ||
                          is.null(names(windows)) || !all(nzchar(names(windows))))) {
    stop("make_ma_graph(): `windows` must hold at most two results lists, ",
         "named by window label", call. = FALSE)
  }

  # meta_analysis() returns list(results = , fits = ); a bare tibble is fine too.
  as_results <- function(x) if (is.data.frame(x)) x else x$results

  # One tidy frame per (outcome, estimand) cell that was actually supplied. `key`
  # names the matching run_gform() result inside each element of `windows`.
  cells <- list(
    list(ma = ma_mcs,     key = "mcs",     outcome = mcs_label, estimand = estimand_levels[1]),
    list(ma = ma_mcs_ate, key = "mcs_ate", outcome = mcs_label, estimand = estimand_levels[2]),
    list(ma = ma_pcs,     key = "pcs",     outcome = pcs_label, estimand = estimand_levels[1]),
    list(ma = ma_pcs_ate, key = "pcs_ate", outcome = pcs_label, estimand = estimand_levels[2])
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
    mutate(r, outcome = c$outcome, estimand = c$estimand, source = pooled_label)
  }))

  # The window estimates behind each supplied cell. Bound ahead of the pooled rows
  # so the pooled marks draw last, on top.
  win <- bind_rows(lapply(names(windows), \(w) bind_rows(lapply(cells, \(c) {
    if (is.null(windows[[w]][[c$key]])) {
      stop("make_ma_graph(): windows$`", w, "` has no `", c$key, "` result",
           call. = FALSE)
    }
    r <- as_results(windows[[w]][[c$key]])
    missing <- setdiff(c("intervention", "mi_effect", "mi_ll", "mi_ul"), names(r))
    if (length(missing)) {
      stop("make_ma_graph(): windows$`", w, "`$", c$key, " is missing column(s) ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    mutate(r, outcome = c$outcome, estimand = c$estimand, source = w)
  }))))
  df <- bind_rows(win, df)

  # Regimes arrive as "0-1-1-0". Recode to "E-U-U-E" for the axis; the count of
  # unemployed periods drives the pooled estimate's colour, each window its own grey.
  df <- df |>
    mutate(
      n_unemp      = factor(str_count(intervention, "1")),
      intervention = str_replace_all(intervention, c("0" = "E", "1" = "U")),
      estimand     = factor(estimand, levels = estimand_levels),
      outcome      = factor(outcome, levels = unique(c(mcs_label, pcs_label))),
      source       = factor(source, levels = c(names(windows), pooled_label)),
      col_key      = if_else(source == pooled_label, as.character(n_unemp),
                             as.character(source))
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

  model <- if ("effects" %in% names(df)) unique(as.character(df$effects)) else NULL

  # Per-source marks: each window a small circle or triangle on a thin interval, the
  # pooled estimate a larger point on the full-width line.
  win_cols <- setNames(window_cols[seq_along(windows)], names(windows))
  sources  <- levels(df$source)
  shapes   <- setNames(c(rep_len(c(16, 17), length(windows)), 16), sources)
  sizes    <- setNames(c(rep(1.1, length(windows)), 1.9), sources)
  widths   <- setNames(c(rep(0.35, length(windows)), 0.55), sources)

  # Within a strategy the estimates stack top to bottom in legend order.
  dodge <- position_dodge(width = 0.8, orientation = "y", reverse = TRUE)

  g <- ggplot(df, aes(mi_effect, intervention, xmin = mi_ll, xmax = mi_ul,
                      colour = col_key, group = source)) +
    geom_vline(data = null_line, aes(xintercept = x),
               linetype = 2, colour = "grey55", inherit.aes = FALSE) +
    geom_errorbar(aes(linewidth = source), width = 0, position = dodge) +
    geom_point(aes(shape = source, size = source), stroke = 0.9, position = dodge) +
    scale_colour_manual(
      values = c(setNames(count_cols[seq_along(levels(df$n_unemp))], levels(df$n_unemp)),
                 win_cols),
      breaks = levels(df$n_unemp),
      name   = "Number of unemployed periods",
      guide  = guide_legend(order = 1)) +
    scale_shape_manual(
      values = shapes, name = "Estimate",
      guide  = if (length(windows)) {
        guide_legend(order = 2, override.aes = list(colour = unname(c(win_cols, "black")),
                                                    size   = unname(sizes)))
      } else "none") +
    scale_size_manual(values = sizes, guide = "none") +
    scale_linewidth_manual(values = widths, guide = "none") +
    facet_grid(outcome ~ estimand, scales = "free_x") +
    # free_x gives each column its own copy of this scale, so the functions see that
    # column's range: marginal means sit far above 0, contrasts reach 0 via the null line.
    # The marginal limits round out to whole points rather than a fixed range, since
    # the windows spread wider than the pooled estimates.
    scale_x_continuous(
      limits = \(l) if (l[1] > 0) c(floor(l[1]), ceiling(l[2])) else l,
      breaks = scales::breaks_width(1),
      oob    = scales::oob_keep
    ) +
    labs(x = "Points on the outcome scale", y = "Intervention strategy") +
    theme_bw(base_size = 12) +
    theme(
      legend.position    = "bottom",
      # Two legends side by side overrun the 9 in width, so stack them.
      legend.box         = "vertical",
      legend.spacing.y   = unit(2, "pt"),
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
