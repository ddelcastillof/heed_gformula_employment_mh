# _targets.R — pipeline orchestration for the employment and health outcomes
# from the HEED project, using gFormula via multiple imputation

library(targets)
library(tarchetypes)
library(future)
library(future.batchtools)
library(future.callr)

# Detect SLURM at runtime, if not in cluster, run locally with future.callr (in a separate r process)
on_slurm <- nzchar(Sys.getenv("SLURM_JOB_ID")) && nzchar(Sys.which("sbatch"))
# Assigning tiers for resources and walltime
if (on_slurm) {
  options(future.cache.path = here::here("logs", ".future"))
  slurm_tier <- function(memory_gb, walltime_h, ncpus = 2L) {
    future::tweak(
      future.batchtools::batchtools_slurm,
      template  = "slurm.tmpl",
      resources = list(
        ncpus    = ncpus,
        memory   = memory_gb * 1024L,       # MB
        walltime = walltime_h * 60L * 60L,  # seconds
        account  = "none"
      )
    )
  }
  # pop_data, build_data, graphs plan
  plan_light <- slurm_tier(memory_gb = 8, walltime_h = 1)   
  # run_mice plan
  plan_mice  <- slurm_tier(memory_gb = 16, walltime_h = 36)   
  # three/four gform
  plan_gform     <- slurm_tier(memory_gb = 96,  walltime_h = 12)
  # stratified EM gform, memory set per stratum in em_grid$em_gform_gb
  plan_em_gform  <- function(memory_gb) slurm_tier(memory_gb = memory_gb, walltime_h = 12)
  # ltmle plan
  plan_ltmle <- slurm_tier(memory_gb = 16, walltime_h = 4)
  future::plan(plan_light)                                  # default for untagged targets
} else {
  # Off-cluster: one local plan for all targets, not recommended as it eats a lot of RAM
  local_plan <- future::tweak(future.callr::callr, workers = 2L)
  plan_light <- plan_mice <- plan_gform <- plan_ltmle <- local_plan
  plan_em_gform <- function(memory_gb) local_plan
  future::plan(local_plan)
}

message("--- TARGETS FUTURE PLAN ---")
message("hostname:        ", Sys.info()[["nodename"]])
message("SLURM_JOB_ID:    '", Sys.getenv("SLURM_JOB_ID"), "'")
message("Sys.which sbatch: '", Sys.which("sbatch"), "'")
message("on_slurm:        ", on_slurm)
message("plan:            ", paste(class(future::plan()), collapse = "/"))
message("---------------------------")

# ---- Packages attached to every target's evaluation environment ----
tar_option_set(
  packages = c(
    "data.table",
    "bit64",
    "dplyr",
    "tidyr",
    "tibble",
    "purrr",
    "magrittr",
    "rlang",
    "here",
    "mice",
    "quarto",
    "gFormulaMI",
    "stringr",
    "fst",
    "lubridate",
    "haven",
    "ggplot2",
    "ltmle",
    "meta",
    "colorBlindness",
    "SuperLearner",
    "xgboost",
    "gam",
    "nnet",
    "ranger"
  ),
  format = "rds",
  repository = "local",
  storage   = "worker",   # workers read/write the shared _targets/ store directly, so multi-GB gFormulaImpute objects never transit the controller
  retrieval = "worker",
  seed   = 42,
  # To avoid excess of calls to the cluster for unresolved workers from the controller.
  backoff = tar_backoff(min = 10, max = 60, rate = 2)
)

# ---- Source extracted functions (R/) ----
for (f in list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE)) source(f); rm(f)

# ---- Configuration for each function ----
## mice configs
mice_m      <- 100
mice_maxit  <- 15
seed_random <- 20260728
## gFormulaMI configs
gform_M <- 100

## meta-analysis configs
# k = 2 studies, so Knapp-Hartung random effects would sit on t with 1 df -- see the
# header of R/meta_analysis.R. Fixed effect is the reported model.
ma_effects <- "fixed"

## ltmle configs
sl_libs <- c("SL.mean", "SL.glm", "SL.gam.ltmle2", "SL.gam.ltmle3", "SL.gam.ltmle4", "SL.gam.ltmle5",
             "SL.xgboost2.ltmle", "SL.xgboost4.ltmle", "SL.rf.ltmle", "SL.nnet5", "SL.nnet10", "SL.poly2", "SL.poly3")

# Regimes for four-waves LTMLE analyses
regimes <- list(
  "0-0-0-0" = c(0, 0, 0, 0),
  "0-0-1-0" = c(0, 0, 1, 0),
  "0-1-0-0" = c(0, 1, 0, 0),
  "0-1-1-0" = c(0, 1, 1, 0),
  "1-0-0-0" = c(1, 0, 0, 0),
  "1-0-1-0" = c(1, 0, 1, 0),
  "1-1-0-0" = c(1, 1, 0, 0),
  "1-1-1-0" = c(1, 1, 1, 0),
  "0-0-0-1" = c(0, 0, 0, 1),
  "0-0-1-1" = c(0, 0, 1, 1),
  "0-1-0-1" = c(0, 1, 0, 1),
  "0-1-1-1" = c(0, 1, 1, 1),
  "1-0-0-1" = c(1, 0, 0, 1),
  "1-0-1-1" = c(1, 0, 1, 1),
  "1-1-0-1" = c(1, 1, 0, 1),
  "1-1-1-1" = c(1, 1, 1, 1)
)

# Patterns for sensitivity with three waves
regimes_three <- list(
  "0-0-0" = c(0, 0, 0),
  "0-0-1" = c(0, 0, 1),
  "0-1-0" = c(0, 1, 0),
  "0-1-1" = c(0, 1, 1),
  "1-0-0" = c(1, 0, 0),
  "1-0-1" = c(1, 0, 1),
  "1-1-0" = c(1, 1, 0),
  "1-1-1" = c(1, 1, 1)
)

# ---- Wave-set spec: one row per analysis, tar_map stamps the chain below per row ----

wave_spec <- tibble::tibble(
  how_many    = c("four", "four", "three", "three"),
  round_start = c(3L, 7L, 3L, 6L),
  round_end   = c(6L, 10L, 5L, 8L),
  label       = c("four", "four_w7_w10", "three", "three_w6_w8")
)
# n_waves drives the LTMLE node expansion: one A node, one Y node and one
# confounder block per wave.
wave_spec$n_waves <- wave_spec$round_end - wave_spec$round_start + 1L

# Four-wave analyses: waves 3-6 and 7-10. `label` keys tar_map because both rows share how_many = "four".
wave_spec_one <- wave_spec[wave_spec$how_many == "four", ]

# Three-wave analyses: waves 3-5 and 6-8. `label` keys tar_map, as for the four-wave rows.
wave_spec_three <- wave_spec[wave_spec$how_many == "three", ]

stopifnot(
  all(lengths(regimes) == wave_spec_one$n_waves),
  all(lengths(regimes_three) == wave_spec_three$n_waves)
)
# ---- Per-wave-set analysis chain (generated by tar_map) ----
map <- tar_map(
  values = wave_spec_one,
  names  = "label",

  # Build wide datasets (MCS / PCS)
  tar_target(wide_data_mcs,
    build_data(data = pop_data, 
               how_many = how_many, 
               outcome = "MCS",
               round_start = round_start, 
               round_end = round_end)),
  tar_target(wide_data_pcs,
    build_data(data = pop_data, 
               how_many = how_many, 
               outcome = "PCS",
               round_start = round_start, 
               round_end = round_end)),

  # mice imputation
  tar_target(wide_mids_mcs,
    run_mice(wide_data = wide_data_mcs$data, 
            m = mice_m, 
            maxit = mice_maxit, 
            seed = seed_random),
    resources = tar_resources(future = tar_resources_future(plan = plan_mice))),
  tar_target(wide_mids_pcs,
    run_mice(wide_data = wide_data_pcs$data, 
      m = mice_m, 
      maxit = mice_maxit, 
      seed = seed_random),
    resources = tar_resources(future = tar_resources_future(plan = plan_mice))),

  # gFormulaMI: marginal + ATE, both outcomes
  tar_target(gform_mcs,
    run_gform(wide_mids = wide_mids_mcs, 
              wide_data_mi = wide_data_mcs$data,
              intervention_pattern = wide_data_mcs$intervention_pattern,
              estimand = "factor(regime) + 0", 
              M = gform_M,
              nSim = 2*nrow(wide_data_mcs$data)),
    resources = tar_resources(future = tar_resources_future(plan = plan_gform))),
  tar_target(gform_pcs,
    run_gform(wide_mids = wide_mids_pcs, 
              wide_data_mi = wide_data_pcs$data,
              intervention_pattern = wide_data_pcs$intervention_pattern,
              estimand = "factor(regime) + 0", 
              M = gform_M,
              nSim = 2*nrow(wide_data_pcs$data)),
    resources = tar_resources(future = tar_resources_future(plan = plan_gform))),
  tar_target(gform_mcs_ate,
    run_gform(wide_mids = wide_mids_mcs, 
              wide_data_mi = wide_data_mcs$data,
              intervention_pattern = wide_data_mcs$intervention_pattern,
              estimand = "factor(regime)", 
              M = gform_M,
              nSim = 2*nrow(wide_data_mcs$data)),
    resources = tar_resources(future = tar_resources_future(plan = plan_gform))),
  tar_target(gform_pcs_ate,
    run_gform(wide_mids = wide_mids_pcs, 
              wide_data_mi = wide_data_pcs$data,
              intervention_pattern = wide_data_pcs$intervention_pattern,
              estimand = "factor(regime)", 
              M = gform_M,
              nSim = 2*nrow(wide_data_pcs$data)),
    resources = tar_resources(future = tar_resources_future(plan = plan_gform))),
# Plots
  tar_target(graphs,
    make_graphs(
      gform_mcs     = gform_mcs,
      gform_pcs     = gform_pcs,
      gform_mcs_ate = gform_mcs_ate,
      gform_pcs_ate = gform_pcs_ate,
      mcs_label = "Mental Component Score (MCS)",
      pcs_label = "Physical Component Score (PCS)",
      save_dir  = here::here("figs"),
      wave_label = label
    ))
)

# ---- Three-wave chains: waves 3-5 and 6-8 ----
map_three <- tar_map(
  values = wave_spec_three,
  names  = "label",

  tar_target(wide_data_mcs,
    build_data(data = pop_data,
               how_many = how_many,
               outcome = "MCS",
               round_start = round_start,
               round_end = round_end)),
  tar_target(wide_data_pcs,
    build_data(data = pop_data,
               how_many = how_many,
               outcome = "PCS",
               round_start = round_start,
               round_end = round_end)),

  tar_target(wide_mids_mcs,
    run_mice(wide_data = wide_data_mcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random),
    resources = tar_resources(future = tar_resources_future(plan = plan_mice))),
  tar_target(wide_mids_pcs,
    run_mice(wide_data = wide_data_pcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random),
    resources = tar_resources(future = tar_resources_future(plan = plan_mice)))
)

# ---- Effect modification: stratified gFormulaMI per modifier level ----
## one row per stratum; the first row of each modifier is its reference stratum
em_spec <- tibble::tribble(
  ~em_modifier, ~em_stratum, ~em_column,            ~em_level,
  "sex",        "male",      "sex_dv_base",         "Male",
  "sex",        "female",    "sex_dv_base",         "Female",
  "race",       "white",     "race_base",           "White",
  "race",       "nonwhite",  "race_base",           "Non-white",
  "hiqual",     "high",      "hiqual_dv_fact_base", "High",
  "hiqual",     "medium",    "hiqual_dv_fact_base", "Medium",
  "hiqual",     "low",       "hiqual_dv_fact_base", "Low"
)
em_reference <- rlang::set_names(em_spec$em_stratum[!duplicated(em_spec$em_modifier)],
                                 em_spec$em_modifier[!duplicated(em_spec$em_modifier)])
em_windows  <- wave_spec_one$label
em_outcomes <- c("mcs", "pcs")
em_grid <- tidyr::expand_grid(em_window = em_windows, em_outcome = em_outcomes, em_spec)
em_grid$em_wide <- rlang::syms(paste0("wide_data_", em_grid$em_outcome, "_", em_grid$em_window))

## em_gform memory (GB) per stratum. At M = 100 and nSim = 4n, sacct MaxRSS grew ~8.7 GiB
## per 1,000 stratum rows (run of 2026-10-09: white and female four were OOM-killed at 96G,
## medium four peaked at 92.6G). Each tier sits >= 15% above the measured or predicted
## peak. The peak scales with M x nSim, so re-tier when either changes.
em_grid$em_gform_gb <- dplyr::case_when(
  em_grid$em_stratum == "white"                                ~ 192,
  em_grid$em_stratum == "female"                               ~ 128,
  em_grid$em_stratum == "medium" & em_grid$em_window == "four" ~ 128,
  em_grid$em_stratum %in% c("low", "nonwhite")                 ~ 48,
  .default = 96
)

## tar_map substitutes values into commands, never into resources, so each memory tier gets
## its own tar_map. Target names do not depend on the tier.
em_chain <- function(values) {
  tar_map(
    values = values,
    names  = c("em_outcome", "em_modifier", "em_stratum", "em_window"),
    unlist = FALSE,

    # Cut one stratum out of the wide data (cheap, so it stays on the default plan)
    tar_target(em_data,
      effect_modification("split",
                          wide_data = em_wide$data,
                          column = em_column,
                          level = em_level),
      error = "abridge",
      deployment = "main"),

    # mice imputation inside the stratum
    tar_target(em_mids,
      run_mice(wide_data = em_data,
               m = mice_m,
               maxit = mice_maxit,
               seed = seed_random),
      error = "abridge",
      resources = tar_resources(future = tar_resources_future(plan = plan_mice))),

    # gFormulaMI inside the stratum: marginal means + contrasts against the first regime
    tar_target(em_gform,
      effect_modification("gform",
                          stratum = em_data,
                          mids = em_mids,
                          intervention_pattern = em_wide$intervention_pattern,
                          M = gform_M,
                          nSim = 4L * nrow(em_data),
                          labels = list(outcome = em_outcome,
                                        window = em_window,
                                        modifier = em_modifier,
                                        stratum = em_stratum)),
      error = "abridge",
      resources = tar_resources(future = tar_resources_future(
        plan = plan_em_gform(values$em_gform_gb[[1]]))))
  )
}
map_em <- lapply(split(em_grid, em_grid$em_gform_gb), em_chain)

# All strata, one table, rows in em_grid order whatever the tier
em_gform_all   <- unlist(unname(lapply(map_em, `[[`, "em_gform")), recursive = FALSE)
em_gform_names <- paste("em_gform", em_grid$em_outcome, em_grid$em_modifier,
                        em_grid$em_stratum, em_grid$em_window, sep = "_")
stopifnot(setequal(names(em_gform_all), em_gform_names))
em_combined <- tar_combine(em_results,
                           em_gform_all[em_gform_names],
                           command = dplyr::bind_rows(!!!.x),
                           deployment = "main")

# ---- Pipeline: shared import + the mapped per-wave-set chains ----
list(
  # Data preparation: import, clean, preprocessing (shared across every wave-set).
  tar_target(pop_data,
    if (on_slurm) {
      import_data(force = FALSE) |> clean_data() |> preproc_data()
    } else {
      import_data(force = TRUE) |> clean_data() |> preproc_data()
    }),
  tar_target(tmle_imp_idx, seq_len(mice_m)), # listing imputed datasets so LTMLE can act over each one
  map,
  map_three,

  # ---- Meta-analysis: pool the two four-wave windows, one regime at a time ----
  
  tar_target(ma_mcs,
    meta_analysis(gform_early = gform_mcs_four,
                  gform_late  = gform_mcs_four_w7_w10,
                  effects     = ma_effects)),
  tar_target(ma_pcs,
    meta_analysis(gform_early = gform_pcs_four,
                  gform_late  = gform_pcs_four_w7_w10,
                  effects     = ma_effects)),
  tar_target(ma_mcs_ate,
    meta_analysis(gform_early = gform_mcs_ate_four,
                  gform_late  = gform_mcs_ate_four_w7_w10,
                  effects     = ma_effects)),
  tar_target(ma_pcs_ate,
    meta_analysis(gform_early = gform_pcs_ate_four,
                  gform_late  = gform_pcs_ate_four_w7_w10,
                  effects     = ma_effects)),

  tar_target(ma_graph,
    make_ma_graph(
      ma_mcs     = ma_mcs,
      ma_mcs_ate = ma_mcs_ate,
      ma_pcs     = ma_pcs,
      ma_pcs_ate = ma_pcs_ate,
      mcs_label  = "Mental Component Score (MCS)",
      pcs_label  = "Physical Component Score (PCS)",
      save_dir   = here::here("figs"),
      wave_label = "pooled"
    )),

  # ---- Effect modification: stratified chains, one table, delta + Q across strata ----
  map_em,
  em_combined,
  tar_target(em_contrasts,
    effect_modification("contrast",
                        results = em_results,
                        reference = em_reference),
                      deployment = "main"),

  # Plots: one forest plot per window, the strata of each modifier side by side
  tar_target(em_graph,
    make_em_graph(
      em_results      = em_results,
      stratum_labels  = rlang::set_names(em_spec$em_level, em_spec$em_stratum),
      modifier_labels = c(sex = "Sex", race = "Race", hiqual = "Education"),
      min_df          = 5,
      save_dir        = here::here("figs")
    ),
    deployment = "main")
)
