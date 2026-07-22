pacman::p_load(testthat,
               here)

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

## minimal reproducible error: does gformulami crashes due to subscript out of bounds?
test_that("run_gformula runs without errors", {
  for (f in list.files(here::here("R"), "\\.R$", full.names = TRUE)) source(f); rm(f)

  data <- import_data(force = FALSE) |> clean_data() |> preproc_data()

  wide_data <- data |> build_data(round_start = 3, 
                 round_end   = 5, 
                 how_many    = "three",
                 outcome     = "PCS"
                 )
# just to check variable and attributes names
  wide_mids <- run_mice(wide_data$data,
                        m     = 35,
                        maxit = 10,
                        seed  = 42)

  g_formula_results <- run_gform(wide_mids,
                        wide_data$data,
                        M = 50,
                        estimand = "factor(regime) + 0",
                        intervention_pattern = wide_data$intervention_pattern
                        )
  
  graph3waves_marginal <- g_formula_results$results |> mutate(outcome = "PCS") |>
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
})

# synthetic wide data carrying the attributes set by make_wide()/set_exposure(),
# so the matrix structure can be tested without importing source data
stems_var <- c("pcs_lagged",
               "dnc_fact_lagged",
               "home_owner_lagged",
               "econ_benefits_lagged",
               "mastat_dv_lagged")

make_synthetic_wide <- function(waves = 0:2) {
  cols <- c("sex_dv_base", "sf12mcs_dv_base",
            unlist(lapply(waves, function(t) c(paste0("econ_emp_bin_fact_", t),
                                               paste0(stems_var, "_", t),
                                               paste0("log_income_", t),
                                               paste0("econ_dist_bin_fact_", t),
                                               paste0("sf12mcs_dv_", t)))))
  df <- as.data.frame(matrix(0, 2, length(cols), dimnames = list(NULL, cols)))
  attr(df, "baseline_vars")    <- c("sex_dv_base", "sf12mcs_dv_base")
  attr(df, "time_lagged")      <- grep("lagged", cols, value = TRUE)
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

# visual inspection of the DAG
make_counterfactual_matrix(wide_data$data) |> as.data.frame() |> 
  openxlsx::write.xlsx(here::here("tests", "counterfactual_matrix_new.xlsx"), 
                       overwrite = TRUE, 
                       rowNames = TRUE, 
                       colNames = TRUE)

