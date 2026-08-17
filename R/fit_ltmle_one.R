# One ltmle fit: one regime x one imputed dataset.
fit_ltmle_one <- function(regime_label, imp_idx, ltmle_data_list,
                          regimes, sl_libs,
                          outcome, n_waves,
                          gbounds = c(1e-6, 1),
                          Yrange  = c(0, 1),
                          SL.cvControl = list(V = 3L)) {

  register_sl_wrappers()
  
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
    SL.cvControl    = SL.cvControl,
    estimate.time   = FALSE,
    variance.method = "ic",
    Yrange          = Yrange
  )
}

# One targets branch: every regime against a single imputed dataset.

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
