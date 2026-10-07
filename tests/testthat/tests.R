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

  # Regression: every other test starts at wave 3, which is the one window where a
  # fixed t0 offset happens to be right. A later window must index from 0 just the same.
  message("Testing build_data on a window that does not start at wave 3 (7-10)")

  expect_error(wide_late <- build_data(data = pop_data,
                                       round_start = 7,
                                       round_end = 10,
                                       how_many = "four",
                                       outcome = "MCS"
                                       ), NA)

  expect_gt(nrow(wide_late$data), 0L)
  expect_identical(attr(wide_late$data, "exposure_vars"),
                   paste0("econ_emp_bin_fact_", 0:3))
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

# testing effect modification: one function, three steps (split, gform, contrast)
test_that("effect_modification: split and gform run end to end on toy data", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  # ---- toy long panel: 300 persons x 3 waves, one row per person-wave ----
  # Every not-employed wave so far lowers MCS by about 4 points, about 6 for men, so the
  # contrasts sit far from null. The data seed is picked so that the two sex strata differ
  # in size: a mids from the wrong stratum is told apart by its row count.
  set.seed(6)
  n   <- 300L
  per <- rep(seq_len(n), each = 3L)          # row -> person
  N   <- length(per)

  # person level, constant across a person's waves
  sex      <- factor(sample(c("Female", "Male"), n, replace = TRUE),
                     levels = c("Female", "Male"))
  race     <- factor(sample(c("White", "Non-white"), n, replace = TRUE, prob = c(0.8, 0.2)),
                     levels = c("White", "Non-white"))
  hiqual   <- factor(sample(c("High", "Medium", "Low"), n, replace = TRUE),
                     levels = c("High", "Medium", "Low"))
  hiqual[sample(n, 10L)] <- NA
  region   <- factor(sample(c("North", "Midlands", "South"), n, replace = TRUE))
  age      <- sample(25:65, n, replace = TRUE)
  mcs_base <- rnorm(n, 50, 8)

  # person-wave level
  not_emp <- factor(rbinom(N, 1L, 0.3), levels = 0:1)
  n_off   <- stats::ave(as.integer(not_emp == "1"), per, FUN = cumsum)
  mcs     <- mcs_base[per] - n_off * (4 + 2 * (sex[per] == "Male")) + rnorm(N, 0, 5)
  log_inc <- rnorm(N, 7.5, 0.6)
  mcs[sample(N, round(0.1 * N))]     <- NA
  log_inc[sample(N, round(0.1 * N))] <- NA

  long <- data.table::data.table(
    pidp                 = per,
    t0                   = rep(0:2, times = n),
    gor_dv_fact_base     = region[per],
    sex_dv_base          = sex[per],
    race_base            = race[per],
    hiqual_dv_fact_base  = hiqual[per],
    age_dv_base          = age[per],
    age_dv_sq_base       = (age[per] - 45)^2,
    sf12mcs_dv_base      = mcs_base[per],
    gor_dv_fact          = region[per],
    pcs_lagged           = rnorm(N, 50, 8),
    dnc_fact_lagged      = factor(sample(c("Zero", "One", "2+"), N, replace = TRUE),
                                  levels = c("Zero", "One", "2+")),
    home_owner_lagged    = factor(sample(c("Renter", "Owner"), N, replace = TRUE),
                                  levels = c("Renter", "Owner")),
    econ_benefits_lagged = factor(sample(c("No benefits", "Benefits"), N, replace = TRUE,
                                         prob = c(0.8, 0.2)),
                                  levels = c("No benefits", "Benefits")),
    mastat_dv_lagged     = factor(sample(c("Not partnered", "Partnered"), N, replace = TRUE),
                                  levels = c("Not partnered", "Partnered")),
    econ_emp_bin_fact    = not_emp,
    log_income           = log_inc,
    econ_dist_bin_fact   = factor(rbinom(N, 1L, 0.3), levels = 0:1),
    sf12mcs_dv           = mcs
  )

  # ---- through the real make_wide() and set_exposure(), as build_data()'s MCS branch ----
  wide_data <- long |>
    make_wide(pidp,
              t0,
              base_cols = c(gor_dv_fact_base,
                            sex_dv_base,
                            race_base,
                            hiqual_dv_fact_base,
                            age_dv_base,
                            age_dv_sq_base,
                            sf12mcs_dv_base),
              outcome = sf12mcs_dv,
              mediators = c(log_income,
                            econ_dist_bin_fact),
              gor_dv_fact,
              pcs_lagged,
              dnc_fact_lagged,
              home_owner_lagged,
              econ_benefits_lagged,
              mastat_dv_lagged,
              econ_emp_bin_fact,
              waves = c(0:2)
              ) |>
    data.table::as.data.table()
  wide_data <- set_exposure(wide_data, exposure = "econ_emp_bin_fact")

  intervention_pattern <- asplit(as.matrix(do.call(data.table::CJ, rep(list(0:1), 3L))), 1)

  # ---- split ----
  n_high <- sum(wide_data$hiqual_dv_fact_base == "High", na.rm = TRUE)

  # 10 persons have no hiqual: they belong to no stratum of it, and the call says so
  suppressMessages(expect_message(
    high <- effect_modification("split", wide_data = wide_data,
                                column = "hiqual_dv_fact_base", level = "High"),
    "dropped 10 row\\(s\\) with missing hiqual_dv_fact_base"
  ))

  expect_s3_class(high, "data.table")
  expect_equal(nrow(high), n_high)
  expect_equal(high$sf12mcs_dv_base,
               wide_data$sf12mcs_dv_base[which(wide_data$hiqual_dv_fact_base == "High")])

  # the constant modifier column goes, and nothing else does
  dropped <- setdiff(names(wide_data), names(high))
  expect_identical(dropped, "hiqual_dv_fact_base")
  expect_identical(names(high), setdiff(names(wide_data), dropped))

  # the make_wide()/set_exposure() attributes are restamped without it, and only without it
  expect_false("hiqual_dv_fact_base" %in% attr(high, "baseline_vars"))
  expect_identical(attr(high, "baseline_vars"),
                   setdiff(attr(wide_data, "baseline_vars"), dropped))
  for (a in c("time_lagged", "time_varying", "outcome_vars", "mediators", "exposure_vars",
              "outcome_final", "outcome_baseline", "time_points")) {
    expect_identical(attr(high, a), attr(wide_data, a), info = a)
  }

  expect_error(effect_modification("split", wide_data = wide_data,
                                   column = "hiqual_dv_fact_base", level = "Very high"),
               "Very high")

  # the split is stored as an rds target and read back by run_mice() and the gform step, so
  # every attribute has to survive the round trip: only the data.table self-reference
  # pointer is rebuilt on read
  pipeline_attrs <- c("baseline_vars", "time_lagged", "time_varying", "outcome_vars",
                      "mediators", "exposure_vars", "outcome_final", "outcome_baseline",
                      "time_points")
  rds <- tempfile(fileext = ".rds")
  saveRDS(high, rds)
  high_back <- readRDS(rds)
  unlink(rds)

  without_selfref <- \(x) {
    a <- attributes(x)
    a <- a[setdiff(names(a), ".internal.selfref")]
    a[sort(names(a))]
  }
  expect_true(all(pipeline_attrs %in% names(attributes(high_back))))
  expect_identical(without_selfref(high_back), without_selfref(high))
  expect_identical(as.data.frame(high_back), as.data.frame(high))

  # ---- gform on the Male stratum ----
  male   <- suppressMessages(effect_modification("split", wide_data = wide_data,
                                                 column = "sex_dv_base", level = "Male"))
  female <- suppressMessages(effect_modification("split", wide_data = wide_data,
                                                 column = "sex_dv_base", level = "Female"))
  expect_false(nrow(male) == nrow(female))

  invisible(utils::capture.output(
    mids_male <- suppressMessages(run_mice(male, m = 5, maxit = 2, seed = 1))
  ))
  labels <- list(outcome = "mcs", window = "toy", modifier = "sex", stratum = "male")

  # gFormulaImpute calls mice() once per imputation (M = 5), not once per regime (8)
  set.seed(11)
  suppressMessages(expect_message(
    res <- effect_modification("gform", stratum = male, mids = mids_male,
                               intervention_pattern = intervention_pattern, M = 5,
                               nSim = 2L * nrow(male), labels = labels),
    "5 mice\\(\\) call\\(s\\)"
  ))

  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2L * 8L)
  expect_named(res, c("outcome", "window", "modifier", "stratum", "stratum_n", "estimand",
                      "term", "intervention", "mi_effect", "mi_se", "mi_df", "mi_ll",
                      "mi_ul"))
  expect_equal(res$estimand, rep(c("marginal", "diff"), each = 8L))
  expect_equal(res$term[res$estimand == "marginal"], paste0("factor(regime)", 1:8))
  expect_equal(res$term[res$estimand == "diff"],
               c("(Intercept)", paste0("factor(regime)", 2:8)))
  expect_equal(res$intervention,
               rep(purrr::map_chr(intervention_pattern, paste, collapse = "-"), times = 2L))
  for (nm in names(labels)) expect_equal(unique(res[[nm]]), labels[[nm]], info = nm)
  expect_true(all(is.finite(res$mi_effect)))
  # plain columns: the coefficient names must not ride along into the tibble
  expect_null(names(res$mi_effect))
  # all-unemployed against all-employed: the toy data lower MCS, far from null
  expect_lt(res$mi_effect[res$estimand == "diff" & res$intervention == "1-1-1"], 0)

  # the stratum's own n, as an integer. `stratum` is also a label here, so this fails if
  # nrow(stratum) is read off the label column instead of the data
  expect_type(res$stratum_n, "integer")
  expect_identical(res$stratum_n, rep(nrow(male), 16L))

  # a mids imputed from the other stratum of the same modifier is refused
  invisible(utils::capture.output(
    mids_female <- suppressMessages(run_mice(female, m = 2, maxit = 1, seed = 1))
  ))
  expect_error(
    effect_modification("gform", stratum = male, mids = mids_female,
                        intervention_pattern = intervention_pattern, M = 5,
                        nSim = 2L * nrow(male), labels = labels),
    "not imputed from this"
  )

  # a mids with one imputation has no between-imputation variance to pool, so it is
  # refused up front, before gFormulaImpute() does any work
  invisible(utils::capture.output(
    mids_one <- suppressMessages(run_mice(male, m = 1, maxit = 1, seed = 1))
  ))
  impute_called <- FALSE
  testthat::with_mocked_bindings(
    expect_error(
      effect_modification("gform", stratum = male, mids = mids_one,
                          intervention_pattern = intervention_pattern, M = 5,
                          nSim = 2L * nrow(male), labels = labels),
      "m = 1.*at least 2"
    ),
    gFormulaImpute = function(...) {
      impute_called <<- TRUE
      stop("gFormulaImpute must not be reached")
    },
    .package = "gFormulaMI"
  )
  expect_false(impute_called)

  # ---- pooling on imputations built by hand: no RNG, every number is known ----
  # gFormulaImpute() is replaced by a mids of M = 4 synthetic datasets, 8 regimes x 6 rows.
  # In dataset j the outcome of regime r is the fixed mean mu[j, r] plus the same residual
  # pattern `e` (it sums to 0) every time, so the regime mean in dataset j is exactly
  # mu[j, r] and the residual variance is the same in all datasets.
  e   <- c(-2, -1, 0, 0, 1, 2)
  reg <- factor(rep(1:8, each = length(e)))
  of  <- attr(male, "outcome_final")

  hand_imps <- function(mu) {
    orig <- data.frame(.imp = 0L, .id = seq_along(reg), regime = reg)
    orig[[of]] <- NA_real_
    sets <- lapply(seq_len(nrow(mu)), \(j) {
      d <- data.frame(.imp = j, .id = seq_along(reg), regime = reg)
      d[[of]] <- rep(mu[j, ], each = length(e)) + e
      d
    })
    mice::as.mids(do.call(rbind, c(list(orig), sets)))
  }

  # the gform step with gFormulaImpute() (and, if given, syntheticPool()) replaced
  gform_with <- function(impute, pool = gFormulaMI::syntheticPool, nSim = 2L * nrow(male)) {
    testthat::with_mocked_bindings(
      suppressMessages(
        effect_modification("gform", stratum = male, mids = mids_male,
                            intervention_pattern = intervention_pattern, M = 5,
                            nSim = nSim, labels = labels)
      ),
      gFormulaImpute = impute,
      syntheticPool  = pool,
      .package = "gFormulaMI"
    )
  }

  # regime 5 is regime 1 plus 10 in every dataset, so its contrast against regime 1 is the
  # same in every dataset: its between-imputation variance is 0 in the difference fit
  mu_bad <- rbind(c(10, 12, 31, 38, 20, 61, 70, 79),
                  c(14, 25, 29, 47, 24, 55, 68, 90),
                  c( 7, 21, 38, 41, 17, 66, 77, 85),
                  c(13, 18, 35, 52, 23, 58, 73, 82))
  stopifnot(all(mu_bad[, 5] - mu_bad[, 1] == 10))
  # the same, except that regime 5 now moves independently of regime 1
  mu_ok <- mu_bad
  mu_ok[, 5] <- c(20, 28, 14, 29)

  # built here, outside the gform step, because it traces mice()
  imps_bad <- hand_imps(mu_bad)
  imps_ok  <- hand_imps(mu_ok)

  # every total variance is positive: both fits go through the real syntheticPool(), and the
  # result columns are its table, marginal fit first: Estimate, sqrt(Total), df and the CI
  real_pool <- gFormulaMI::syntheticPool
  seen      <- list()
  res_ok <- gform_with(
    \(...) imps_ok,
    pool = \(fits) {
      seen[[length(seen) + 1L]] <<- fits
      real_pool(fits)
    }
  )
  expect_length(seen, 2L)
  pooled <- do.call(rbind, lapply(seen, real_pool))
  expect_equal(res_ok$mi_effect, unname(pooled[, "Estimate"]))
  expect_equal(res_ok$mi_se,     unname(sqrt(pooled[, "Total"])))
  expect_equal(res_ok$mi_df,     unname(pooled[, "df"]))
  expect_equal(res_ok$mi_ll,     unname(pooled[, "95% CI L"]))
  expect_equal(res_ok$mi_ul,     unname(pooled[, "95% CI U"]))
  expect_false(anyNA(res_ok))

  # one bad term: the real syntheticPool() stops on the diff fit, so the inline rule takes
  # over for it. The warning names the stratum, the estimand, the term and the original
  # error, and only that term is blanked
  expect_warning(
    res_bad <- gform_with(\(...) imps_bad),
    paste0("^effect_modification: \\[mcs/toy/sex/male\\] diff: total variance <= 0 for ",
           "term\\(s\\) factor\\(regime\\)5, so mi_se, mi_df, mi_ll and mi_ul are NA\\. ",
           ".*\\(syntheticPool: Some parameters have estimated total variances")
  )
  is_bad <- res_bad$estimand == "diff" & res_bad$term == "factor(regime)5"
  expect_equal(sum(is_bad), 1L)
  expect_true(all(is.na(res_bad[is_bad, c("mi_se", "mi_df", "mi_ll", "mi_ul")])))
  expect_equal(res_bad$mi_effect[is_bad], 10)            # the contrast itself survives
  expect_true(all(is.finite(res_bad$mi_effect)))
  expect_false(anyNA(res_bad[!is_bad, ]))
  # and the good terms are what the real syntheticPool() gives for them: mu_ok differs from
  # mu_bad only by a constant shift of regime 5 in each dataset, which changes nothing but
  # the terms for regime 5 (the residuals, and so every variance, stay the same), and the
  # real function succeeds on mu_ok. So it is the reference for all the other terms
  not5 <- res_bad$term != "factor(regime)5"
  for (col in c("mi_effect", "mi_se", "mi_df", "mi_ll", "mi_ul")) {
    expect_equal(res_bad[[col]][not5], res_ok[[col]][not5], info = col)
  }

  # every imputation identical: the between-imputation variance is 0, so no total is > 0.
  # Every se, df and CI is NA, the estimates stay, and nSim reaches gFormulaImpute().
  # Both fits warn, once each. A call of expect_warning() claims the warning it matches;
  # the other one goes to suppressWarnings(), whichever way the testthat edition treats it.
  same <- data.frame(regime = reg)
  same[[of]] <- 10 * as.integer(reg) + rep(e, times = 8L)
  degenerate <- mice::mice(same, m = 3, maxit = 0, printFlag = FALSE)
  seen_nsim  <- NULL
  impute_degenerate <- \(data, M, trtVars, trtRegimes, nSim, ...) {
    seen_nsim <<- nSim
    degenerate
  }
  suppressWarnings(expect_warning(
    res_deg <- gform_with(impute_degenerate, nSim = 77L),
    "\\[mcs/toy/sex/male\\] marginal: total variance <= 0 for term\\(s\\) factor\\(regime\\)1,"
  ))
  suppressWarnings(expect_warning(
    gform_with(impute_degenerate, nSim = 77L),
    "\\[mcs/toy/sex/male\\] diff: total variance <= 0 for term\\(s\\) \\(Intercept\\),"
  ))
  expect_identical(seen_nsim, 77L)
  expect_equal(nrow(res_deg), 16L)
  expect_true(all(is.finite(res_deg$mi_effect)))
  expect_true(all(is.na(res_deg[c("mi_se", "mi_df", "mi_ll", "mi_ul")])))

  # a syntheticPool() failure that is not a bad total is not the fallback's to absorb:
  # here every total is positive, so the original error comes through
  expect_error(
    gform_with(\(...) imps_ok, pool = \(fits) stop("boom")),
    "boom"
  )

  # ---- back to the real run: its mi_df follows the same rules, whichever way it pooled ----
  # mi_df goes NA exactly where mi_se does, and the CI is the estimate +/- t(mi_df) * mi_se.
  # (Last, because a missing mi_df makes the qt() call an error rather than a failure)
  ok <- !is.na(res$mi_se)
  expect_true(any(ok))
  expect_identical(is.na(res$mi_df), is.na(res$mi_se))
  expect_true(all(res$mi_df[ok] > 0))
  half <- (stats::qt(0.975, res$mi_df) * res$mi_se)[ok]
  expect_equal(res$mi_ul[ok] - res$mi_effect[ok], half)
  expect_equal(res$mi_effect[ok] - res$mi_ll[ok], half)
})

test_that("effect_modification: contrast reproduces hand-computed delta and Q", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  # one contrast per stratum: regime 8 ("1-1-1") against the reference regime. Infinite
  # df make the t reference the normal one, so the expected values are the z-based ones.
  # stratum_n rides along, as it does in the "gform" output
  contrasts <- tibble::tribble(
    ~modifier, ~stratum, ~mi_effect, ~mi_se,
    "sex",     "male",   -2,         0.3,
    "sex",     "female", -3,         0.4,
    "hiqual",  "high",   -1,         0.5,
    "hiqual",  "medium", -2,         0.5,
    "hiqual",  "low",    -4,         1.0
  ) |>
    dplyr::mutate(outcome = "mcs", window = "toy", estimand = "diff",
                  term = "factor(regime)8", intervention = "1-1-1",
                  stratum_n = 100L, mi_df = Inf)

  # rows that must not reach delta or Q, given wild values: the intercept of the diff fit
  # (the mean under the reference regime) and the marginal means
  ignored <- dplyr::bind_rows(
    dplyr::mutate(contrasts, term = "(Intercept)", mi_effect = 50, mi_se = 0.01),
    dplyr::mutate(contrasts, estimand = "marginal", mi_effect = 999, mi_se = 0.001)
  )
  results   <- dplyr::bind_rows(ignored, contrasts)
  reference <- c(sex = "male", hiqual = "high")

  res <- effect_modification("contrast", results = results, reference = reference)

  expect_named(res, c("delta", "q"))
  # stratum_n is neither carried nor allowed to turn into .x/.y columns
  expect_named(res$delta, c("outcome", "window", "modifier", "intervention", "stratum",
                            "reference", "delta", "delta_se", "delta_df", "delta_ll",
                            "delta_ul", "delta_p"))
  expect_named(res$q, c("outcome", "window", "modifier", "intervention", "k", "q", "q_df",
                        "q_p", "min_mi_df"))

  # delta: one row per non-reference stratum
  expected_delta <- tibble::tribble(
    ~modifier, ~stratum, ~reference, ~delta, ~delta_se,    ~delta_ll,     ~delta_ul,     ~delta_p,
    "hiqual",  "low",    "high",     -3,     1.1180339887, -5.1913063514, -0.8086936486, 0.0072903581,
    "hiqual",  "medium", "high",     -1,     0.7071067812, -2.3859038243, 0.3859038243,  0.1572992071,
    "sex",     "female", "male",     -1,     0.5,          -1.9799819923, -0.0200180077, 0.0455002639
  )
  got_delta <- dplyr::arrange(res$delta, modifier, stratum)

  expect_equal(nrow(got_delta), 3L)
  expect_equal(got_delta$modifier,     expected_delta$modifier)
  expect_equal(got_delta$stratum,      expected_delta$stratum)
  expect_equal(got_delta$reference,    expected_delta$reference)
  expect_equal(unique(got_delta$outcome),      "mcs")
  expect_equal(unique(got_delta$window),       "toy")
  expect_equal(unique(got_delta$intervention), "1-1-1")
  for (col in c("delta", "delta_se", "delta_ll", "delta_ul", "delta_p")) {
    expect_equal(got_delta[[col]], expected_delta[[col]], tolerance = 1e-8, info = col)
  }
  # two infinite df give an infinite delta df, and so the normal reference
  expect_true(all(got_delta$delta_df == Inf))

  # q: one row per modifier
  expected_q <- tibble::tribble(
    ~modifier, ~k, ~q,           ~q_df, ~q_p,
    "hiqual",  3,  7.5555555556, 2,     0.0228734649,
    "sex",     2,  4.0000000000, 1,     0.0455002639
  )
  got_q <- dplyr::arrange(res$q, modifier)

  expect_equal(nrow(got_q), 2L)
  expect_equal(got_q$modifier, expected_q$modifier)
  expect_equal(unique(got_q$outcome),      "mcs")
  expect_equal(unique(got_q$window),       "toy")
  expect_equal(unique(got_q$intervention), "1-1-1")
  for (col in c("k", "q", "q_df", "q_p")) {
    expect_equal(got_q[[col]], expected_q[[col]], tolerance = 1e-8, info = col)
  }
  expect_true(all(got_q$min_mi_df == Inf))

  # a modifier in results without a reference entry, or whose reference stratum is not
  # in results, or a results table without a required column, is refused
  expect_error(effect_modification("contrast", results = results,
                                   reference = c(sex = "male")),
               "hiqual")
  expect_error(effect_modification("contrast", results = results,
                                   reference = c(sex = "male", hiqual = "very high")),
               "very high")
  expect_error(effect_modification("contrast", results = dplyr::select(results, -mi_se),
                                   reference = reference),
               "missing: mi_se")
  expect_error(effect_modification("contrast", results = dplyr::select(results, -mi_df),
                                   reference = reference),
               "missing: mi_df")

  # a missing mi_se propagates: NA delta_se for that stratum, NA q for its modifier
  na_results <- dplyr::mutate(
    results,
    mi_se = dplyr::if_else(modifier == "hiqual" & stratum == "low" &
                             estimand == "diff" & term != "(Intercept)",
                           NA_real_, mi_se)
  )
  res_na   <- effect_modification("contrast", results = na_results, reference = reference)
  delta_na <- dplyr::arrange(res_na$delta, modifier, stratum)
  q_na     <- dplyr::arrange(res_na$q, modifier)

  expect_true(all(is.na(delta_na[1, c("delta_se", "delta_df", "delta_ll", "delta_ul",
                                      "delta_p")])))
  expect_equal(delta_na$delta[1], -3)
  expect_false(anyNA(delta_na[2:3, c("delta_se", "delta_df", "delta_ll", "delta_ul",
                                     "delta_p")]))
  expect_true(is.na(q_na$q[q_na$modifier == "hiqual"]))
  expect_true(is.na(q_na$q_p[q_na$modifier == "hiqual"]))
  expect_false(is.na(q_na$q[q_na$modifier == "sex"]))
})

test_that("effect_modification: contrast takes its t reference from the strata's own df", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  # one outcome x window x intervention, one contrast term, sex with male as reference
  results <- tibble::tibble(
    outcome = "mcs", window = "toy", modifier = "sex", intervention = "1-1-1",
    stratum = c("male", "female"), estimand = "diff", term = "factor(regime)8",
    mi_effect = c(2, 1), mi_se = c(0.3, 0.4), mi_df = c(20, 10)
  )
  res <- effect_modification("contrast", results = results, reference = c(sex = "male"))

  # Constants worked out by hand, not by the formula under test.
  #   delta    = 1 - 2 = -1
  #   delta_se = sqrt(0.4^2 + 0.3^2) = 0.5
  #   delta_df = 0.5^4 / (0.4^4 / 10 + 0.3^4 / 20) = 0.0625 / 0.002965 = 21.0792580101
  #   t(0.975, 21.0792580101) = 2.0791378290, so the CI is -1 -/+ 2.0791378290 * 0.5
  #   p        = 2 * pt(-|-1 / 0.5|, 21.0792580101) = 0.0585499975 (z would give 0.0455)
  #   Q        = (2 - 1)^2 / (0.3^2 + 0.4^2) = 4, on 1 df: p = 0.0455002639
  #   min_mi_df = min(20, 10) = 10
  expect_equal(nrow(res$delta), 1L)
  expect_equal(res$delta$delta,    -1,            tolerance = 1e-8)
  expect_equal(res$delta$delta_se, 0.5,           tolerance = 1e-8)
  expect_equal(res$delta$delta_df, 21.0792580101, tolerance = 1e-8)
  expect_equal(res$delta$delta_ll, -2.0395689145, tolerance = 1e-8)
  expect_equal(res$delta$delta_ul, 0.0395689145,  tolerance = 1e-8)
  expect_equal(res$delta$delta_p,  0.0585499975,  tolerance = 1e-8)

  expect_equal(nrow(res$q), 1L)
  expect_equal(res$q$k,         2L)
  expect_equal(res$q$q,         4,            tolerance = 1e-8)
  expect_equal(res$q$q_df,      1,            tolerance = 1e-8)
  expect_equal(res$q$q_p,       0.0455002639, tolerance = 1e-8)
  expect_equal(res$q$min_mi_df, 10,           tolerance = 1e-8)

  # one infinite df drops out of the Welch-Satterthwaite sum; no special case is needed:
  # 0.25^2 / (0.4^4 / 10) = 24.4140625
  inf_ref <- dplyr::mutate(results, mi_df = c(Inf, 10))
  res_inf <- effect_modification("contrast", results = inf_ref, reference = c(sex = "male"))
  expect_equal(res_inf$delta$delta_df, 24.4140625, tolerance = 1e-8)
  expect_equal(res_inf$q$min_mi_df, 10)

  # a missing df makes every t-based cell NA, and min_mi_df too; the z-free ones stay
  na_df     <- dplyr::mutate(results, mi_df = c(20, NA))
  res_na_df <- effect_modification("contrast", results = na_df, reference = c(sex = "male"))
  expect_true(all(is.na(res_na_df$delta[c("delta_df", "delta_ll", "delta_ul", "delta_p")])))
  expect_equal(res_na_df$delta$delta, -1)
  expect_equal(res_na_df$delta$delta_se, 0.5)
  expect_true(is.na(res_na_df$q$min_mi_df))
  expect_equal(res_na_df$q$q, 4, tolerance = 1e-8)
})

test_that("effect_modification: contrast handles one stratum, duplicated keys, a missing reference", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  results <- tibble::tribble(
    ~modifier, ~stratum, ~mi_effect, ~mi_se, ~mi_df,
    "sex",     "male",   -2,         0.3,    20,
    "sex",     "female", -3,         0.4,    10,
    "hiqual",  "high",   -1,         0.5,    30,
    "hiqual",  "medium", -2,         0.5,    25,
    "hiqual",  "low",    -4,         1.0,    15
  ) |>
    dplyr::mutate(outcome = "mcs", window = "toy", estimand = "diff",
                  term = "factor(regime)8", intervention = "1-1-1")
  reference <- c(sex = "male", hiqual = "high")

  # a modifier with one stratum has no heterogeneity to test: q, q_df and q_p are NA, not
  # 0, 0 and 1, but k and min_mi_df are still reported, and the column types do not change
  one_sex <- dplyr::filter(results, !(modifier == "sex" & stratum == "female"))
  res_one <- effect_modification("contrast", results = one_sex, reference = reference)
  q_one   <- dplyr::arrange(res_one$q, modifier)

  expect_equal(q_one$modifier, c("hiqual", "sex"))
  expect_equal(q_one$k, c(3L, 1L))
  expect_true(all(is.na(q_one[2, c("q", "q_df", "q_p")])))
  expect_false(anyNA(q_one[1, c("q", "q_df", "q_p")]))
  expect_equal(q_one$min_mi_df, c(15, 20))
  expect_type(q_one$q,    "double")
  expect_type(q_one$q_df, "integer")
  expect_type(q_one$q_p,  "double")
  expect_false("sex" %in% res_one$delta$modifier)       # no stratum left to contrast

  # and when every group has one stratum the columns keep their types too
  only_one <- effect_modification("contrast", results = dplyr::filter(one_sex, modifier == "sex"),
                                  reference = c(sex = "male"))
  expect_equal(nrow(only_one$q), 1L)
  expect_true(is.na(only_one$q$q) && is.na(only_one$q$q_df) && is.na(only_one$q$q_p))
  expect_type(only_one$q$q,    "double")
  expect_type(only_one$q$q_df, "integer")
  expect_type(only_one$q$q_p,  "double")
  expect_equal(nrow(only_one$delta), 0L)

  # a repeated key (a stratum bound twice, a target built twice) is refused, and the
  # message names the first one
  dup <- dplyr::bind_rows(results, results[2, ])
  expect_error(effect_modification("contrast", results = dup, reference = reference),
               "duplicated")
  expect_error(effect_modification("contrast", results = dup, reference = reference),
               "modifier = sex, stratum = female, estimand = diff, term = factor\\(regime\\)8")
  expect_error(effect_modification("contrast", results = dplyr::bind_rows(results, results),
                                   reference = reference),
               "stratum = male")        # the first repeated row is the first male row

  # a window without the reference stratum: no error, NA cells for that window's deltas
  # (the join finds nothing), and the windows that have it are unaffected
  no_ref <- results |>
    dplyr::filter(!(modifier == "sex" & stratum == "male")) |>
    dplyr::mutate(window = "toy2")
  both   <- dplyr::bind_rows(results, no_ref)
  res_nr <- effect_modification("contrast", results = both, reference = reference)

  cells  <- c("delta", "delta_se", "delta_df", "delta_ll", "delta_ul", "delta_p")
  d_toy2 <- dplyr::filter(res_nr$delta, window == "toy2", modifier == "sex")
  expect_equal(nrow(d_toy2), 1L)
  expect_equal(d_toy2$stratum, "female")
  expect_true(all(is.na(d_toy2[cells])))
  expect_false(anyNA(dplyr::filter(res_nr$delta, window == "toy")[cells]))
  expect_false(anyNA(dplyr::filter(res_nr$delta, window == "toy2", modifier == "hiqual")[cells]))
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

  # Test flight: two extreme regimes, two imputations. The full run is every regime in
  # `regimes` x seq_len(mice_m), i.e. m branches of one ltmleMSM fit each.
  flight <- regimes[c(paste(rep(0, n_waves), collapse = "-"),
                      paste(rep(1, n_waves), collapse = "-"))]

  message("Fitting 2 ltmleMSM models (", n_waves, " waves, ",
          length(nodes$Anodes), " A nodes, ", length(flight), " regimes)")

  fits <- lapply(seq_len(2L), function(i) {
    fit_ltmle_imp(imp_idx         = i,
                  ltmle_data_list = ltmle_data,
                  regimes         = flight,
                  sl_libs         = sl_libs,
                  outcome         = outcome,
                  n_waves         = n_waves)
  })

  # one fit per imputation, carrying estimates AND their covariance
  for (f in fits) {
    expect_setequal(f$intervention, names(flight))
    expect_length(f$estimate, length(flight))
    expect_equal(dim(f$cov), c(length(flight), length(flight)))
    expect_equal(f$cov, t(f$cov))                       # symmetric
    expect_true(all(diag(f$cov) > 0))
    expect_true(all(f$estimate > 0 & f$estimate < 1))    # Y is on the /100 scale here
  }

  pooled <- pool_ltmle(fits)

  expect_setequal(pooled$estimates$intervention, names(flight))
  expect_equal(dim(pooled$T), c(length(flight), length(flight)))
  expect_true(all(is.finite(pooled$estimates$ltmle_effect)))
  expect_true(all(pooled$estimates$ltmle_se > 0))
  expect_true(all(pooled$estimates$ltmle_ll <= pooled$estimates$ltmle_effect &
                    pooled$estimates$ltmle_effect <= pooled$estimates$ltmle_ul))
  # pool_ltmle rescales by 100, so estimates are back on the SF-12 scale
  expect_true(all(pooled$estimates$ltmle_effect > 0 &
                    pooled$estimates$ltmle_effect < 100))

  print(pooled$estimates)

  # contrasts come from the pooled covariance, so one row per non-reference regime
  contr <- pooled$contrasts
  expect_equal(nrow(contr), length(flight) - 1L)
  expect_true(all(contr$ltmle_se > 0))

  # A saturated MSM must reproduce separate ltmle() calls exactly -- that identity is
  # the whole justification for the refactor, so assert it rather than trust it.
  sep <- vapply(names(flight), function(lab) {
    fit <- ltmle::ltmle(
      data = ltmle_data[[1]], Anodes = nodes$Anodes, Lnodes = nodes$Lnodes,
      Ynodes = nodes$Ynodes, survivalOutcome = FALSE, gbounds = c(1e-6, 1),
      abar = as.vector(flight[[lab]]), SL.library = sl_libs, SL.cvControl = list(V = 3L),
      estimate.time = FALSE, variance.method = "ic", Yrange = c(0, 1))
    s <- summary(fit, estimator = "tmle")$treatment
    c(est = unname(s$estimate), se = unname(s$std.dev))
  }, numeric(2))

  expect_equal(as.vector(fits[[1]]$estimate[names(flight)]),
               unname(sep["est", ]), tolerance = 1e-8)
  expect_equal(sqrt(diag(fits[[1]]$cov))[names(flight)] |> unname(),
               unname(sep["se", ]), tolerance = 1e-8)

  # a regime whose length does not match the A nodes must fail loudly
  expect_error(
    fit_ltmle_imp(1L, ltmle_data,
                  c(flight, list(bad = rep(0, n_waves + 1L))),
                  sl_libs, outcome, n_waves),
    "element"
  )
})
## fit_ltmle_imp must carry positivity diagnostics, not just point estimates

test_that("fit_ltmle_imp returns per-regime cumulative g diagnostics", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  outcome <- "MCS"
  n_waves <- 2L
  nodes   <- ltmle_nodes(outcome, n_waves)

  # Synthetic data in the shape prepare_ltmle_data() emits: no network, no mice.
  set.seed(20260819)
  n <- 300L
  fac <- function(k) factor(sample(seq_len(k), n, TRUE))
  dat <- data.frame(
    sex_dv_base          = fac(2),
    hiqual_dv_fact_base  = fac(3),
    race_base            = fac(2),
    gor_dv_fact_base     = fac(3),
    age_dv_base          = sample(25:65, n, TRUE),
    age_dv_sq_base       = NA_real_,
    sf12mcs_dv_base      = rnorm(n, 50, 9)
  )
  dat$age_dv_sq_base <- dat$age_dv_base^2
  for (t in 0:(n_waves - 1L)) {
    dat[[paste0("pcs_lagged_", t)]]           <- rnorm(n, 50, 9)
    dat[[paste0("econ_benefits_lagged_", t)]] <- rbinom(n, 1L, 0.2)
    dat[[paste0("home_owner_lagged_", t)]]    <- rbinom(n, 1L, 0.6)
    dat[[paste0("mastat_dv_lagged_", t)]]     <- fac(3)
    dat[[paste0("dnc_fact_lagged_", t)]]      <- fac(2)
    dat[[paste0("econ_emp_bin_fact_", t)]]    <-
      rbinom(n, 1L, plogis(-1 + 0.02 * (dat$sf12mcs_dv_base - 50)))
    dat[[paste0("log_income_", t)]]           <- rnorm(n, 10, 1)
    dat[[paste0("econ_dist_bin_fact_", t)]]   <- rbinom(n, 1L, 0.25)
    dat[[paste0("sf12mcs_dv_", t)]]           <- plogis(rnorm(n))
  }
  dat <- dat[, nodes$cols]
  expect_identical(names(dat), nodes$cols)

  # every A path, so the cumulative g's must partition probability
  regs <- asplit(as.matrix(expand.grid(rep(list(0:1), n_waves))), 1)
  regs <- setNames(lapply(regs, as.vector),
                   vapply(regs, paste, character(1), collapse = "-"))

  fit <- fit_ltmle_imp(imp_idx         = 1L,
                       ltmle_data_list = list(dat),
                       regimes         = regs,
                       sl_libs         = "SL.glm",
                       outcome         = outcome,
                       n_waves         = n_waves)

  g <- fit$g_diag
  expect_s3_class(g, "data.frame")
  expect_true(all(c("intervention", "depth", "n_follow", "g_mean", "g_median",
                    "g_p01", "g_min", "pct_lt_1e2", "pct_lt_1e3", "n_clipped",
                    "ess", "max_wt_share") %in% names(g)))
  # one row per regime per cumulative depth
  expect_equal(nrow(g), length(regs) * length(nodes$Anodes))
  expect_setequal(g$intervention, names(regs))
  expect_setequal(g$depth, seq_along(nodes$Anodes))

  # IDENTITY 1: the 2^n regimes enumerate every A path, so summing cum.g over them
  # partitions probability. At depth d only the first d elements of abar matter, so
  # each distinct prefix is counted 2^(n_anodes - d) times and the means sum to that.
  n_a <- length(nodes$Anodes)
  for (d in seq_len(n_a)) {
    expect_equal(sum(g$g_mean[g$depth == d]), 2^(n_a - d), tolerance = 1e-8)
  }

  # IDENTITY 2: cum.g is a running product of probabilities, so it cannot grow with depth
  for (lab in names(regs)) {
    gi <- g[g$intervention == lab, ][order(g$depth[g$intervention == lab]), ]
    expect_true(all(diff(gi$g_mean) <= 1e-12))
  }

  # n_follow at full depth is just the observed A pattern count
  A <- as.matrix(dat[, nodes$Anodes])
  for (lab in names(regs)) {
    hand <- sum(rowSums(sweep(A, 2, regs[[lab]], `==`)) == length(nodes$Anodes))
    expect_equal(g$n_follow[g$intervention == lab & g$depth == length(nodes$Anodes)],
                 hand)
  }

  # an effective sample size can never exceed the number of rows following the regime
  expect_true(all(g$ess <= g$n_follow + 1e-8))
  expect_true(all(g$g_min > 0 & g$g_median > 0))
  expect_true(all(g$max_wt_share >= 0 & g$max_wt_share <= 100))
})

## the diagnostics must survive pooling, or nobody will look at them

test_that("pool_ltmle averages the cum.g diagnostics across imputations", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  outcome <- "MCS"
  n_waves <- 2L
  nodes   <- ltmle_nodes(outcome, n_waves)

  make_imp <- function(seed) {
    set.seed(seed)
    n <- 300L
    fac <- function(k) factor(sample(seq_len(k), n, TRUE))
    d <- data.frame(
      sex_dv_base         = fac(2),
      hiqual_dv_fact_base = fac(3),
      race_base           = fac(2),
      gor_dv_fact_base    = fac(3),
      age_dv_base         = sample(25:65, n, TRUE),
      age_dv_sq_base      = NA_real_,
      sf12mcs_dv_base     = rnorm(n, 50, 9)
    )
    d$age_dv_sq_base <- d$age_dv_base^2
    for (t in 0:(n_waves - 1L)) {
      d[[paste0("pcs_lagged_", t)]]           <- rnorm(n, 50, 9)
      d[[paste0("econ_benefits_lagged_", t)]] <- rbinom(n, 1L, 0.2)
      d[[paste0("home_owner_lagged_", t)]]    <- rbinom(n, 1L, 0.6)
      d[[paste0("mastat_dv_lagged_", t)]]     <- fac(3)
      d[[paste0("dnc_fact_lagged_", t)]]      <- fac(2)
      d[[paste0("econ_emp_bin_fact_", t)]]    <-
        rbinom(n, 1L, plogis(-1 + 0.02 * (d$sf12mcs_dv_base - 50)))
      d[[paste0("log_income_", t)]]           <- rnorm(n, 10, 1)
      d[[paste0("econ_dist_bin_fact_", t)]]   <- rbinom(n, 1L, 0.25)
      d[[paste0("sf12mcs_dv_", t)]]           <- plogis(rnorm(n))
    }
    d[, nodes$cols]
  }

  imps <- list(make_imp(11L), make_imp(12L))
  regs <- list("0-0" = c(0, 0), "0-1" = c(0, 1), "1-1" = c(1, 1))

  fits <- lapply(seq_along(imps), function(i) {
    fit_ltmle_imp(imp_idx = i, ltmle_data_list = imps, regimes = regs,
                  sl_libs = "SL.glm", outcome = outcome, n_waves = n_waves)
  })

  pooled <- pool_ltmle(fits)

  g <- pooled$g_diag
  expect_s3_class(g, "data.frame")
  expect_equal(nrow(g), length(regs) * length(nodes$Anodes))
  expect_setequal(g$intervention, names(regs))
  expect_true(all(g$n_imp == length(fits)))

  # every numeric diagnostic is the mean over imputations, keyed on regime x depth
  key <- function(d) paste(d$intervention, d$depth)
  num_cols <- setdiff(names(g)[vapply(g, is.numeric, logical(1))],
                      c("depth", "n_imp"))
  expect_true(length(num_cols) >= 8)
  for (cl in num_cols) {
    per_imp <- vapply(fits, function(f) {
      f$g_diag[[cl]][match(key(g), key(f$g_diag))]
    }, numeric(nrow(g)))
    expect_equal(g[[cl]], rowMeans(per_imp), tolerance = 1e-10, info = cl)
  }
})

test_that("meta_analysis pools the two wave-sets", {
  source(here::here("R", "meta_analysis.R"))

  regs <- c("0-0-0-0", "1-0-1-0", "1-1-1-1")
  mk <- function(eff, se) {
    tibble::tibble(intervention = regs, mi_effect = eff, mi_se = se,
                   mi_ll = eff - 1.96 * se, mi_ul = eff + 1.96 * se)
  }
  # Regime 1 is the equal-SE pair, so its fixed-effect pool must be the plain
  # mean. Regime 3 is identical in both arms, so its tau2 must be exactly 0.
  early <- list(results = mk(c(50, 47, 44), c(0.5, 0.4, 0.6)))
  late  <- list(results = mk(c(52, 46, 44), c(0.5, 0.7, 0.6)))

  fx <- meta_analysis(early, late, effects = "fixed")
  rm_ <- suppressMessages(meta_analysis(early, late, effects = "random"))

  # shape and ordering follow the early arm
  for (ma in list(fx, rm_)) {
    expect_named(ma, c("results", "fits"))
    expect_equal(ma$results$intervention, regs)
    expect_equal(names(ma$fits), regs)
    expect_true(all(vapply(ma$fits, inherits, logical(1), "meta")))
  }
  expect_equal(fx$results$effects, rep("fixed", length(regs)))
  expect_equal(rm_$results$effects, rep("random", length(regs)))

  # inverse variance on two equally precise estimates is their arithmetic mean
  expect_equal(fx$results$mi_effect[1], 51)
  # and pooling never leaves the interval spanned by the two inputs
  expect_true(all(fx$results$mi_effect >=
                    pmin(early$results$mi_effect, late$results$mi_effect)))
  expect_true(all(fx$results$mi_effect <=
                    pmax(early$results$mi_effect, late$results$mi_effect)))
  # pooling two studies is more precise than either one alone
  expect_true(all(fx$results$mi_se <
                    pmin(early$results$mi_se, late$results$mi_se)))

  # Knapp-Hartung is t on k - 1 = 1 df, so the random interval is wider even
  # where tau2 is zero and the two models share a standard error.
  expect_equal(rm_$results$tau2[3], 0)
  expect_equal(rm_$results$mi_se[3], fx$results$mi_se[3])
  expect_true(all((rm_$results$mi_ul - rm_$results$mi_ll) >
                    (fx$results$mi_ul - fx$results$mi_ll)))

  # The HK variance rescales by Q / (k - 1), so regime 3 -- identical in both
  # wave-sets, hence Q = 0 -- collapses to a zero-width interval without the
  # ad hoc correction. That collapse is exactly what the default guards.
  raw <- suppressMessages(meta_analysis(early, late, effects = "random",
                                        adhoc.hakn.ci = ""))
  expect_equal(raw$results$Q[3], 0)
  expect_equal(raw$results$mi_se[3], 0)
  expect_equal(raw$results$mi_ul[3] - raw$results$mi_ll[3], 0)
  # and it leaves the heterogeneous regimes alone
  expect_equal(raw$results$mi_se[1:2], rm_$results$mi_se[1:2])

  # no column carries the regime names through from the fits list
  expect_true(all(vapply(rm_$results, \(x) is.null(names(x)), logical(1))))

  # a bare tibble is accepted in place of the run_gform() wrapper
  expect_equal(meta_analysis(early$results, late$results)$results, fx$results)

  # disagreeing regime sets are a hard error, as in pool_ltmle()
  clash <- list(results = dplyr::mutate(late$results,
                                        intervention = c(regs[1:2], "0-1-0-1")))
  expect_error(meta_analysis(early, clash), "disagree on the regime set")
  # so is a table that is not a gFormulaMI result
  expect_error(meta_analysis(early, list(results = tibble::tibble(x = 1))),
               "missing column")
})
