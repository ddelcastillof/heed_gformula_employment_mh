pacman::p_load(testthat,
               here,
               ggplot2,
               purrr,
               colorBlindness)

i_am("tests/testthat/tests.R")

# does build_data runs without errors
test_that("build_data runs without errors", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)
  
  pop_data <- import_data(force = TRUE) |> clean_data() |> preproc_data()
  
  message("Minimal analytical scenario: testing three waves (3-5) for the three outcomes")
  message("Testing build_data with three waves")

  expect_error(wide_data <- build_data(data = pop_data, 
                                       round_start = 3, 
                                       round_end = 5, 
                                       how_many = "three",
                                       outcome = "MCS"
                                       ), NA)
  
  message("Testing build_data for outcome PCS")

  expect_error(wide_data <- build_data(data = pop_data, 
                                       round_start = 3, 
                                       round_end = 5, 
                                       how_many = "three",
                                       outcome = "PCS"
                                       ), NA)
  
  message("Is wide_data a DT object?")
  
  expect_true(data.table::is.data.table(wide_data$data))
})

## does run_mice runs without errors

test_that("run_mice runs without errors", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)
  
  pop_data <- import_data(force = FALSE) |> clean_data() |> preproc_data()
  
  message("Testing run_mice with three waves and outcome MCS")
  
  wide_data <- build_data(data = pop_data, 
                          round_start = 3, 
                          round_end = 5, 
                          how_many = "three",
                          outcome = "MCS"
                          )
  
  expect_error(wide_mids <- run_mice(wide_data$data,
                                     m     = 2,
                                     maxit = 2,
                                     seed  = 42), NA)
  
  message("Is wide_mids a mids object")

  expect_true(inherits(wide_mids, "mids"))
})

## age_dv and gor_dv_fact are imputed passively at interior waves, with the first and last wave complete
test_that("interior age_dv and gor_dv_fact waves are imputed passively", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  make_wave_data <- function(n_waves, n = 60) {
    regions <- c("London", "Wales", "Scotland")
    df <- data.frame(sf12mcs_dv_base = rnorm(n))
    for (t in seq_len(n_waves) - 1L) {
      df[[paste0("age_dv_", t)]]      <- 30L + t
      df[[paste0("gor_dv_fact_", t)]] <- factor(rep_len(regions, n), levels = regions)
      df[[paste0("log_income_", t)]]  <- rnorm(n)
    }
    for (t in seq_len(n_waves - 2L)) {
      df[[paste0("age_dv_", t)]][1:10]      <- NA_integer_
      df[[paste0("gor_dv_fact_", t)]][1:10] <- NA
    }
    df$log_income_0[1:5] <- NA_real_
    df
  }

  set.seed(42)

  for (n_waves in 3:5) {
    message("Testing passive age_dv and gor_dv_fact imputation with ", n_waves, " waves")

    interior <- seq_len(n_waves - 2L)
    mids     <- run_mice(make_wave_data(n_waves), m = 1, maxit = 1, seed = 42)

    expect_equal(unname(mids$method[paste0("age_dv_", interior)]),
                 paste0("~ I(age_dv_0 + ", interior, "L)"))
    expect_equal(unname(mids$method[paste0("gor_dv_fact_", interior)]),
                 paste0("~ I(gor_dv_fact_", interior - 1L, ")"))

    cmp <- mice::complete(mids, 1)
    for (t in interior) {
      expect_equal(cmp[[paste0("age_dv_", t)]], cmp$age_dv_0 + t)
      # last observation carried forward, chaining through consecutive gaps
      expect_equal(cmp[[paste0("gor_dv_fact_", t)]], cmp[[paste0("gor_dv_fact_", t - 1L)]])
    }
  }
})

## minimal reproducible error: does gformulami crashes due to subscript out of bounds (misspelled var)?
test_that("run_gformula runs without errors", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  data <- import_data(force = TRUE) |> clean_data() |> preproc_data()

  wide_data <- data |> build_data(round_start = 3, 
                 round_end   = 5, 
                 how_many    = "three",
                 outcome     = "MCS"
                 )
# just to check variable and attributes names
  wide_mids <- run_mice(wide_data$data,
                        m     = 50,
                        maxit = 1,
                        seed  = 42)

  g_formula_results <- run_gform(wide_mids,
                        wide_data$data,
                        M = 50,
                        estimand = "factor(regime)",
                        nSim = 2*nrow(wide_data$data),
                        intervention_pattern = wide_data$intervention_pattern
                        )
  
  graph3waves_marginal <- g_formula_results$results |> mutate(outcome = "MCS") |>
  mutate(
    n_unemp = map_int(intervention, ~ length(str_extract_all(.x, "1", simplify = TRUE))) |> factor(),
    intervention = str_replace_all(intervention, c("0" = "E", "1" = "U"))
  ) |>
  ggplot(aes(mi_effect, intervention, xmin = mi_ll, xmax = mi_ul, colour = n_unemp)) +
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.7)) +
  scale_colour_manual(values = unname(paletteMartin), name = "Number of unemployed periods") +
  facet_wrap(~outcome, nrow = 1, scales = "free_x") +
  labs(x = "Estimate", y = "Intervention strategy") +
  theme_bw()
  
  graph3waves_diff <- g_formula_results$results |> mutate(outcome = "MCS") |>
  mutate(
    n_unemp = map_int(intervention, ~ length(str_extract_all(.x, "1", simplify = TRUE))) |> factor(),
    intervention = str_replace_all(intervention, c("0" = "E", "1" = "U"))
  ) |>
  filter(intervention != "E-E-E-E") |>
  ggplot(aes(mi_effect, intervention, xmin = mi_ll, xmax = mi_ul, colour = n_unemp)) +
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.3, position = position_dodge(width = 0.7)) +
  scale_colour_manual(values = unname(paletteMartin), name = "Number of unemployed periods") +
  facet_wrap(~outcome, nrow = 1, scales = "free_x") +
  labs(x = "Estimate", y = "Intervention strategy") +
  theme_bw()
  
  print(graph3waves_marginal)
  print(graph3waves_diff)
})

# synthetic wide data carrying the attributes set by make_wide()/set_exposure(),
# so the matrix structure can be tested without importing source data
stems_var <- c("pcs_lagged",
               "dnc_fact_lagged",
               "home_owner_lagged",
               "econ_benefits_lagged",
               "mastat_dv_lagged")

# column order mirrors build_data(): baselines with the outcome trailing, then per
# wave the time-varying confounder, the lagged confounders, the exposure, the
# mediators and finally the outcome
base_cols_synth <- c("gor_dv_fact_base", "sex_dv_base", "sf12mcs_dv_base")

make_synthetic_wide <- function(waves = 0:2) {
  cols <- c(base_cols_synth,
            unlist(lapply(waves, function(t) c(paste0("gor_dv_fact_", t),
                                               paste0(stems_var, "_", t),
                                               paste0("econ_emp_bin_fact_", t),
                                               paste0("log_income_", t),
                                               paste0("econ_dist_bin_fact_", t),
                                               paste0("sf12mcs_dv_", t)))))
  df <- as.data.frame(matrix(0, 2, length(cols), dimnames = list(NULL, cols)))
  attr(df, "baseline_vars")    <- base_cols_synth
  attr(df, "time_lagged")      <- grep("lagged", cols, value = TRUE)
  attr(df, "time_varying")     <- paste0("gor_dv_fact_", waves)
  attr(df, "outcome_vars")     <- paste0("sf12mcs_dv_", waves)
  attr(df, "outcome_baseline") <- "sf12mcs_dv_base"
  attr(df, "time_points")      <- length(waves)
  attr(df, "mediators")        <- c(paste0("log_income_", waves),
                                    paste0("econ_dist_bin_fact_", waves))
  attr(df, "exposure_vars")    <- paste0("econ_emp_bin_fact_", waves)
  df
}

p_mat <- make_counterfactual_matrix(make_synthetic_wide())

test_that("lagged vars predict only their corresponding stem at the next wave", {
  for (t in 1:2) {
    block <- p_mat[paste0(stems_var, "_", t), paste0(stems_var, "_", t - 1)]
    expect_equal(unname(diag(block)), rep(1, length(stems_var)))
    expect_equal(sum(block) - sum(diag(block)), 0)
  }
})

test_that("cross-group lag-1 arrows into lagged vars are preserved", {
  for (t in 1:2) {
    parents <- c(paste0("econ_emp_bin_fact_", t - 1),
                 paste0("log_income_", t - 1),
                 paste0("econ_dist_bin_fact_", t - 1),
                 paste0("sf12mcs_dv_", t - 1))
    block <- p_mat[paste0(stems_var, "_", t), parents]
    expect_true(all(block == 1))
  }
})

test_that("single-column self-lag arrows are preserved", {
  expect_equal(p_mat["econ_emp_bin_fact_2", "econ_emp_bin_fact_1"], 1)
  expect_equal(p_mat["log_income_2", "log_income_1"], 1)
  expect_equal(p_mat["econ_dist_bin_fact_2", "econ_dist_bin_fact_1"], 1)
  expect_equal(p_mat["sf12mcs_dv_2", "sf12mcs_dv_1"], 1)
})

test_that("t0 outcome is predicted by the baseline outcome", {
  expect_equal(p_mat["sf12mcs_dv_0", "sf12mcs_dv_base"], 1)
})

test_that("regime row is predicted by all other variables", {
  expect_true(all(p_mat["regime", setdiff(colnames(p_mat), "regime")] == 1))
  expect_equal(p_mat["regime", "regime"], 0)
})

# gFormulaImpute() runs mice with maxit = 1 and draws columns left to right, so a
# variable listed to the right of its own parent reads that parent's iteration-0
# hot-deck seed instead of its drawn value. The matrix must therefore be strictly
# lower-triangular. This is the invariant that breaks silently whenever a select()
# in make_wide() or a base_cols order in build_data() is rearranged.
test_that("no arrow points at a variable that has not been drawn yet", {
  offenders <- which(p_mat == 1 & upper.tri(p_mat, diag = TRUE), arr.ind = TRUE)
  expect_equal(
    nrow(offenders), 0,
    info = paste(rownames(p_mat)[offenders[, "row"]], "<-",
                 colnames(p_mat)[offenders[, "col"]], collapse = "; ")
  )
})

# with empty predictor rows mice draws each baseline from its own marginal, which
# discards the observed joint (region and ethnicity come out unrelated)
test_that("every time-invariant baseline but the first is chained on the previous ones", {
  baselines <- attr(make_synthetic_wide(), "baseline_vars")
  for (i in seq_along(baselines)[-1]) {
    expect_equal(unname(p_mat[baselines[i], baselines[seq_len(i - 1)]]),
                 rep(1, i - 1))
  }
  expect_equal(sum(p_mat[baselines[1], ]), 0)
})

test_that("counterfactual methods cover every column and stay in the observed support", {
  wide <- make_synthetic_wide()
  typed <- as.data.frame(lapply(names(wide), function(nm) {
    if (grepl("^(gor_dv_fact|hiqual)", nm)) factor(c("a", "b"), levels = c("a", "b", "c"))
    else if (grepl("fact|lagged|sex", nm)) factor(c("a", "b"))
    else c(1, 2)
  }), col.names = names(wide))
  attributes(typed) <- c(attributes(typed), attributes(wide)[c("exposure_vars")])

  method <- make_counterfactual_method(typed)

  expect_equal(length(method), ncol(typed))
  expect_equal(names(method), names(typed))
  # treatment is assigned from the regime, never imputed
  expect_true(all(method[attr(wide, "exposure_vars")] == ""))
  # pmm rather than gFormulaMI's default "norm", which is unbounded
  expect_equal(unname(method["sf12mcs_dv_2"]), "pmm")
  expect_equal(unname(method["log_income_0"]), "pmm")
  expect_equal(unname(method["sex_dv_base"]), "logreg")
  expect_equal(unname(method["gor_dv_fact_base"]), "polyreg")
})

# visual inspection of the DAG
make_counterfactual_matrix(wide_data$data) |> as.data.frame() |> 
  openxlsx::write.xlsx(here::here("tests", "counterfactual_matrix.xlsx"), 
                       overwrite = TRUE, 
                       rowNames = TRUE, 
                       colNames = TRUE)

# testing effect modification functions
test_that("effect modification functions run without errors", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)
  
  pop_data <- import_data(force = TRUE) |> clean_data() |> preproc_data()

  message("Testing build_data_em with three waves and outcome MCS, modifier sex")

  wide_data_em <- build_data_em(data = pop_data, 
                                              round_start = 3, 
                                              round_end = 5, 
                                              how_many = "three",
                                              outcome = "MCS",
                                              modifier = "sex"
                                              )
})

#### Testing LTMLE functions
test_that("TMLE functions run without errors", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)
  
  pop_data <- import_data(force = TRUE) |> clean_data() |> preproc_data()

  message("Testing build_data with three waves and outcome MCS")

  wide_data <- build_data(data = pop_data, 
                          round_start = 3, 
                          round_end = 5, 
                          how_many = "three",
                          outcome = "MCS"
                          )
  
  # test flight: small m, the pipeline runs m = 50
  mice_m <- 5L

  wide_mids <- run_mice(wide_data$data,
                        m     = mice_m,
                        maxit = 1,
                        seed  = 42) # just to check if it works with mids objects

  ## ltmle configs
  sl_libs <- c("SL.mean", "SL.glm", "SL.gam")

  # one A node per wave, so abar vectors are length n_waves. round_start = 3 and
  # round_end = 5 above give three waves; _targets.R must generate its regimes
  # the same way per wave-set, the length-4 list only fits the four-wave rows.
  n_waves <- 5L - 3L + 1L
  outcome <- "MCS"

  # as.vector(), not unname(): asplit() returns 1-D arrays and ltmle's abar
  # handling requires a plain vector
  regimes <- asplit(as.matrix(expand.grid(rep(list(0:1), n_waves))), 1)
  regimes <- setNames(lapply(regimes, as.vector),
                      vapply(regimes, paste, character(1), collapse = "-"))
  expect_length(regimes, 2^n_waves)

  ltmle_data <- prepare_ltmle_data(wide_mids, outcome = outcome, n_waves = n_waves)
  nodes      <- ltmle_nodes(outcome, n_waves)

  expect_length(ltmle_data, mice_m)
  # column order IS the ltmle contract: A/L/Y are matched positionally
  expect_identical(names(ltmle_data[[1]]), nodes$cols)
  expect_true(all(vapply(ltmle_data[[1]][nodes$Anodes],
                         \(a) all(a %in% c(0L, 1L)), logical(1))))
  expect_true(all(vapply(ltmle_data[[1]][nodes$Ynodes],
                         \(y) all(y >= 0 & y <= 1), logical(1))))
  expect_false(anyNA(ltmle_data[[1]]))

  # test flight grid: the two extreme regimes x two imputations. The full run is
  # names(regimes) x seq_len(mice_m), i.e. 2^n_waves * m fits.
  tmle_imp_idx <- seq_len(2L)
  grid <- expand.grid(
    regime_label = c(paste(rep(0, n_waves), collapse = "-"),
                     paste(rep(1, n_waves), collapse = "-")),
    imp_idx      = tmle_imp_idx,
    stringsAsFactors = FALSE
  )

  message("Fitting ", nrow(grid), " ltmle models (", n_waves, " waves, ",
          length(nodes$Anodes), " A nodes)")

  ltmle_one <- purrr::pmap(grid, function(regime_label, imp_idx) {
    fit_ltmle_one(regime_label    = regime_label,
                  imp_idx         = imp_idx,
                  ltmle_data_list = ltmle_data,
                  regimes         = regimes,
                  sl_libs         = sl_libs,
                  outcome         = outcome,
                  n_waves         = n_waves)
  })

  expect_length(ltmle_one, nrow(grid))
  expect_true(all(vapply(ltmle_one, inherits, logical(1), "ltmle")))

  # fits are summarised before pooling, so targets never stores the ltmle objects
  ltmle_sum <- summarise_ltmle_fits(ltmle_one, grid$regime_label, grid$imp_idx)

  expect_equal(nrow(ltmle_sum), nrow(grid))
  expect_true(all(ltmle_sum$variance > 0))

  pooled <- pool_ltmle(ltmle_sum)

  expect_setequal(pooled$intervention, unique(grid$regime_label))
  expect_true(all(is.finite(pooled$ltmle_effect)))
  expect_true(all(pooled$ltmle_se > 0))
  expect_true(all(pooled$ltmle_ll <= pooled$ltmle_effect &
                    pooled$ltmle_effect <= pooled$ltmle_ul))
  # pool_ltmle rescales by 100, so estimates are back on the SF-12 scale
  expect_true(all(pooled$ltmle_effect > 0 & pooled$ltmle_effect < 100))

  print(pooled)

  # the shape _targets.R actually branches on: one imputation, every regime in
  # the list, summarised in-branch. Two regimes here only to keep the flight short.
  imp_summary <- fit_ltmle_imp(imp_idx         = 1L,
                               ltmle_data_list = ltmle_data,
                               regimes         = regimes[1:2],
                               sl_libs         = sl_libs,
                               outcome         = outcome,
                               n_waves         = n_waves)

  expect_equal(nrow(imp_summary), 2L)
  expect_setequal(imp_summary$intervention, names(regimes)[1:2])
  expect_true(all(imp_summary$imp_idx == 1L))
  expect_error(pool_ltmle(imp_summary), NA)

  # a regime whose length does not match the A nodes must fail loudly
  expect_error(
    fit_ltmle_one("bad", 1L, ltmle_data, list(bad = rep(0, n_waves + 1L)),
                  sl_libs, outcome, n_waves),
    "A node"
  )
})