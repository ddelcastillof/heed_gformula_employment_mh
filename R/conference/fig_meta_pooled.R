# ==============================================================================
# Conference figure: pooled meta-analysis estimates across the two wave windows.
# ==============================================================================

suppressMessages({
  library(targets); library(here); library(dplyr); library(tidyr)
  library(stringr); library(ggplot2)
})
here::i_am("g_formula.Rproj")
source(here("R", "meta_analysis.R"))

# ==============================================================================
# CONFIG
# ==============================================================================

OUTCOME      <- "MCS"
EFFECTS      <- "fixed"
PANELS       <- c("marginal", "contrast")
SHOW_WINDOWS <- FALSE

## --- how to mark heterogeneity ------------------------------------------
FLAG_HET   <- FALSE
HET_ALPHA  <- 0.05
SHOW_NOTE  <- FALSE

## --- ordering and filtering ---------------------------------------------
ORDER_BY <- "label"
KEEP_STRATEGIES <- NULL

## --- look ----------------------------------------------------------------
BASE_SIZE  <- 12
POINT_SIZE <- 1.5
LINE_WIDTH <- 0.5
PALETTE    <- unname(palette.colors(palette = "Okabe-Ito", names = TRUE)[
  c("black", "reddishpurple", "skyblue", "vermillion", "bluishgreen")
])
WINDOW_COLS <- c("Waves 3-6" = "grey45", "Waves 7-10" = "grey72")

## --- legend footprint ------------------------------------------------------
LEGEND_POS   <- "right"  
LEGEND_ROWS  <- 2        
LEGEND_KEY   <- 0.75      
LEGEND_REL   <- 0.75
LEGEND_WRAP  <- 18       
LEGEND_GAP   <- 12       # gap between panel edge and legend box

## --- text -----------------------------------------------------------------
TITLE    <- NULL
SUBTITLE <- NULL
X_LAB    <- NULL
Y_LAB    <- "Intervention strategy"
LEGEND_TITLE <- "Number of unemployed periods"

## --- output ---------------------------------------------------------------
SAVE      <- TRUE
OUT_DIR   <- here::here("figs")
OUT_NAME  <- NULL
OUT_W     <- 8
OUT_H     <- 6
OUT_DPI   <- 300

# ==============================================================================
# END CONFIG
# ==============================================================================

stopifnot(OUTCOME %in% c("MCS", "PCS"),
          EFFECTS %in% c("fixed", "random"),
          length(PANELS) >= 1, all(PANELS %in% c("marginal", "contrast")),
          ORDER_BY %in% c("label", "effect"))

o <- tolower(OUTCOME)
targets_needed <- c(
  marginal_early = sprintf("gform_%s_four", o),
  marginal_late  = sprintf("gform_%s_four_w7_w10", o),
  contrast_early = sprintf("gform_%s_ate_four", o),
  contrast_late  = sprintf("gform_%s_ate_four_w7_w10", o)
)
gf <- lapply(targets_needed, tar_read_raw)

pool <- function(early, late) {
  suppressMessages(meta_analysis(early, late,
                                 labels  = c("Waves 3-6", "Waves 7-10"),
                                 effects = EFFECTS))
}
ma <- list(
  marginal = pool(gf$marginal_early, gf$marginal_late),
  contrast = pool(gf$contrast_early, gf$contrast_late)
)

panel_labs <- c(
  marginal = "Marginal mean",
  contrast = "Mean difference"
)

# ---- assemble one long frame -----------------------------------------------
take <- function(d, panel, source) {
  r <- tibble::as_tibble(if (is.data.frame(d)) d else d$results)
  if (!"p_het" %in% names(r)) r$p_het <- NA_real_
  transmute(r, intervention, mi_effect, mi_ll, mi_ul, p_het,
            panel = panel, source = source)
}

df <- bind_rows(lapply(PANELS, function(p) {
  rows <- take(ma[[p]], p, "Pooled")
  if (SHOW_WINDOWS) {
    rows <- bind_rows(
      take(gf[[paste0(p, "_early")]], p, "Waves 3-6"),
      take(gf[[paste0(p, "_late")]],  p, "Waves 7-10"),
      rows
    )
  }
  rows
}))

df <- df |>
  mutate(
    n_unemp      = factor(str_count(intervention, "1")),
    intervention = str_replace_all(intervention, c("0" = "E", "1" = "U")),
    panel        = factor(panel, levels = PANELS, labels = panel_labs[PANELS]),
    source       = factor(source, levels = c("Waves 3-6", "Waves 7-10", "Pooled")),
    pooled_ok    = is.na(p_het) | p_het >= HET_ALPHA
  )

df <- filter(df, !(panel == panel_labs["contrast"] &
                     str_detect(intervention, "^E(-E)*$")))

if (!is.null(KEEP_STRATEGIES)) df <- filter(df, intervention %in% KEEP_STRATEGIES)
if (!nrow(df)) stop("No rows left to plot -- check KEEP_STRATEGIES.")

# ---- strategy ordering ------------------------------------------------------
lev <- if (ORDER_BY == "effect") {
  key <- df |>
    filter(source == "Pooled",
           panel == panel_labs[if ("contrast" %in% PANELS) "contrast" else PANELS[1]]) |>
    arrange(desc(mi_effect))
  c(setdiff(unique(df$intervention), key$intervention), key$intervention)
} else {
  rev(sort(unique(df$intervention)))
}
df <- mutate(df, intervention = factor(intervention, levels = lev))

# ---- labels -----------------------------------------------------------------
subtitle <- SUBTITLE
x_lab    <- X_LAB    %||% paste(OUTCOME, "points")
caption  <- if (FLAG_HET && SHOW_NOTE && any(!df$pooled_ok)) {
  str_wrap(paste0(
    "Reference for the contrasts are the always employed trajectory\n(E-E-E-E)"
  ), width = 110)
} else NULL

# ---- the null line belongs to the contrast panel only -----------------------
null_line <- df |>
  filter(panel == panel_labs["contrast"]) |>
  distinct(panel) |>
  mutate(x = 0)

# ---- build ------------------------------------------------------------------
dodge <- if (SHOW_WINDOWS) position_dodge(width = 0.75) else position_identity()

g <- ggplot(df, aes(mi_effect, intervention, xmin = mi_ll, xmax = mi_ul))

if (nrow(null_line)) {
  g <- g + geom_vline(data = null_line, aes(xintercept = x),
                      linetype = 2, colour = "grey55", inherit.aes = FALSE)
}

if (SHOW_WINDOWS) {
  src_cols <- c(WINDOW_COLS, Pooled = PALETTE[2])
  g <- g +
    geom_errorbar(aes(colour = source), width = 0,
                  linewidth = LINE_WIDTH, position = dodge) +
    geom_point(aes(colour = source, size = source == "Pooled",
                   shape = if (FLAG_HET) pooled_ok else TRUE),
               fill = "white", stroke = 0.9, position = dodge) +
    scale_colour_manual(values = src_cols, name = NULL,
                        limits = names(src_cols)) +
    scale_size_manual(values = c(`FALSE` = POINT_SIZE * 0.7,
                                 `TRUE`  = POINT_SIZE),
                      guide = "none")
} else {
  g <- g +
    geom_errorbar(aes(colour = n_unemp), width = 0,
                  linewidth = LINE_WIDTH, position = dodge) +
    geom_point(aes(colour = n_unemp,
                   shape = if (FLAG_HET) pooled_ok else TRUE),
               size = POINT_SIZE, fill = "white", stroke = 0.9,
               position = dodge) +
    scale_colour_manual(
      values = PALETTE,
      name   = if (LEGEND_POS == "right") str_wrap(LEGEND_TITLE, LEGEND_WRAP)
               else LEGEND_TITLE)
}

g <- g +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
  facet_wrap(~panel, nrow = 1, scales = "free_x") +
  guides(colour = guide_legend(
    nrow = if (LEGEND_POS == "bottom") LEGEND_ROWS else NULL)) +
  labs(title = TITLE, subtitle = subtitle, caption = "Reference for the contrasts are the always employed trajectory (E-E-E-E) \nResults from waves 3-6 and 7-10 were pooled with a fixed-effect meta-analysis. \nThe graph shows the pooled estimates",
       x = x_lab, y = Y_LAB) +
  theme_bw(base_size = BASE_SIZE) +
  theme(
    legend.position       = LEGEND_POS,
    legend.title          = element_text(size = rel(LEGEND_REL)),
    legend.text           = element_text(size = rel(LEGEND_REL)),
    legend.title.position = "top",
    legend.key.size       = unit(LEGEND_KEY, "lines"),
    legend.key.spacing.x  = unit(4, "pt"),
    legend.key.spacing.y  = unit(1, "pt"),
    legend.margin         = margin(0, 0, 0, 0),
    legend.box.spacing    = unit(LEGEND_GAP, "pt"),
    plot.subtitle         = element_text(colour = "grey35",
                                         size = rel(0.8)),
    plot.caption          = element_text(colour = "grey35", size = rel(0.7),
                                         hjust = 0, margin = margin(t = 10)),
    plot.caption.position = "plot",
    axis.title.x          = element_text(size = rel(0.8)),
    axis.text.x           = element_text(size = rel(0.8)),
    axis.title.y          = element_text(size = rel(0.8)),
    axis.text.y           = element_text(size = rel(0.8)),
    panel.grid.minor      = element_blank(),
    panel.grid.major.y    = element_line(colour = "grey92"),
    strip.background      = element_rect(fill = "grey95", colour = "grey70")
  )

print(g)

if (SAVE) {
  out <- OUT_NAME %||% sprintf("conf_meta_%s_%s.png", o, EFFECTS)
  ggsave(file.path(OUT_DIR, out), g,
         width = OUT_W, height = OUT_H, dpi = OUT_DPI)
  message("wrote ", file.path(OUT_DIR, out))
}
