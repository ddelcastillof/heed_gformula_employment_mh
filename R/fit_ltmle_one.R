# One ltmle fit: one regime x one imputed dataset.
#
# Qform and gform are left to ltmle. Its defaults regress every node on all the
# columns that precede it, and prepare_ltmle_data() already hands it the columns
# in ltmle_nodes() order, so the defaults are the fully adjusted models.

fit_ltmle_one <- function(regime_label, imp_idx, ltmle_data_list,
                          regimes, sl_libs,
                          outcome, n_waves,
                          gbounds = c(0.01, 1),
                          Yrange  = c(0, 1)) {

  nodes <- ltmle_nodes(outcome, n_waves)

  if (!regime_label %in% names(regimes)) {
    stop("fit_ltmle_one(): regime '", regime_label, "' is not in regimes", call. = FALSE)
  }
  abar <- regimes[[regime_label]]

  if (is.array(abar) && length(dim(abar)) == 1L) abar <- as.vector(abar)

  if (length(abar) != length(nodes$Anodes)) {
    stop("fit_ltmle_one(): regime '", regime_label, "' has ", length(abar),
         " element(s) but the ", n_waves, "-wave data has ",
         length(nodes$Anodes), " A node(s)", call. = FALSE)
  }

  ltmle::ltmle(
    data            = ltmle_data_list[[imp_idx]],
    Anodes          = nodes$Anodes,
    Lnodes          = nodes$Lnodes,
    Ynodes          = nodes$Ynodes,
    survivalOutcome = FALSE,
    gbounds         = gbounds,
    abar            = abar,
    SL.library      = sl_libs,
    estimate.time   = FALSE,
    variance.method = "ic",
    Yrange          = Yrange
  )
}

# One targets branch: every regime against a single imputed dataset.
#
# Branching over imputations rather than over regime x imputation keeps the job
# count at m per outcome instead of m * 2^n_waves, and the fits are summarised
# here so only a 2^n_waves-row tibble crosses back to the targets store.
fit_ltmle_imp <- function(imp_idx, ltmle_data_list, regimes, sl_libs,
                          outcome, n_waves) {

  regime_labels <- names(regimes)

  fits <- purrr::map(regime_labels, function(regime_label) {
    fit_ltmle_one(regime_label    = regime_label,
                  imp_idx         = imp_idx,
                  ltmle_data_list = ltmle_data_list,
                  regimes         = regimes,
                  sl_libs         = sl_libs,
                  outcome         = outcome,
                  n_waves         = n_waves)
  })

  summarise_ltmle_fits(fits, regime_labels, imp_idx = imp_idx)
}
