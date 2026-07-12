# targets orchestation of employment trajectories analysis
# this pipeline will run both the primary analysis as the sensitivity analyses in parallel
# using future/batchtools: run with tar_make_future(); on SLURM each heavy
# target is submitted as its own transient sbatch job rendered from
# slurm/batchtools.slurm.tmpl

pacman::p_load(targets,
               tarchetypes,
               future,
               future.batchtools,
               future.callr)

# Detect SLURM at runtime, if not in cluster, run locally
on_slurm <- nzchar(Sys.getenv("SLURM_JOB_ID")) && nzchar(Sys.which("sbatch"))

# batchtools plan factory: future.batchtools >= 0.22.0 only honors resources
# attached to the plan (backend), not per-future ones, so every plan carries
# its resources. Template conventions (slurm/batchtools.slurm.tmpl): walltime
# in seconds (rendered as minutes for --time), memory is per CPU
# (--mem-per-cpu)
slurm_plan <- function(ncpus, memory, walltime) {
  future::tweak(future.batchtools::batchtools_slurm,
                template  = here::here("slurm", "batchtools.slurm.tmpl"),
                resources = list(ncpus = ncpus, memory = memory,
                                 walltime = walltime, account = "none"))
}

# future plan for tar_make_future(): on SLURM, batchtools submits one sbatch
# job per deployment = "worker" target using the template; locally each
# target runs in a fresh callr R process. The SLURM default below is a
# defensive fallback -- every heavy target overrides it with a tier plan
if (on_slurm) {
  future::plan(slurm_plan(ncpus = 1L, memory = "8G", walltime = 3600L))
} else {
  future::plan(future.callr::callr, workers = 4L)
}

message("--- TARGETS PIPELINE ---")
message("hostname:        ", Sys.info()[["nodename"]])
message("SLURM_JOB_ID:    '", Sys.getenv("SLURM_JOB_ID"), "'")
message("Sys.which sbatch: '", Sys.which("sbatch"), "'")
message("on_slurm:        ", on_slurm)
message("future ver:           ", as.character(packageVersion("future")))
message("future.batchtools ver: ", as.character(packageVersion("future.batchtools")))
message("batchtools ver:       ", as.character(packageVersion("batchtools")))
message("future.callr ver:     ", as.character(packageVersion("future.callr")))
message("---------------------------")

# ---- Packages attached to every target's evaluation environment ------------
# deployment = "main" is the default: light targets run inside the controller
# job; only targets tagged deployment = "worker" below become SLURM jobs
tar_option_set(
  packages   = c("data.table",
                 "here",
                 "mice",
                 "gFormulaMI",
                 "magrittr",
                 "tidyverse",
                 "colorBlindness",
                 "cowplot",
                 "flextable",
                 "bit64"),
  format     = "rds",
  seed       = 42,
  deployment = "main"
)

# Load all R scripts in the R/ folder
for (f in list.files("R", "\\.R$", full.names = TRUE)) source(f); rm(f)

# ---- Configuration ---------------------------------------------------------
## mice configs
mice_m      <- 35
mice_maxit  <- 15
seed_random <- 42
## gFormulaMI configs
gform_M <- 50
## effect-modification configs (three-wave, stratified)
em_mice_m     <- 50
em_mice_maxit <- 15
em_seed       <- 20260410
em_datasets   <- 200
## per-job SLURM resources: each tier is a tweaked batchtools plan that
## targets swaps in per target at launch (tar_resources_future(plan = ...));
## locally the tiers collapse to the default (callr ignores resources)
resources_mice <- if (on_slurm) {
  targets::tar_resources(future = targets::tar_resources_future(
    plan = slurm_plan(ncpus = 2L, memory = "24G", walltime = 43200L)))  # 48G total, 12 h
} else list()
resources_gform <- if (on_slurm) {
  targets::tar_resources(future = targets::tar_resources_future(
    plan = slurm_plan(ncpus = 2L, memory = "16G", walltime = 28800L)))  # 32G total, 8 h
} else list()

# ---- DAG -------------------------------------------------------------------

# Define the targets pipeline
list(
  tar_target(data_main,
    if (on_slurm) {
      import_data(force = FALSE) |> clean_data() |> preproc_data()
    } else {
      import_data(force = TRUE) |> clean_data() |> preproc_data()
    }
  ),
  # ---- MCS outcome targets --------------------------------------------------
  ## ---- MCS for four waves ----
  tar_target(main_wide_mcs,
    build_data(data_main,
               round_start = 3,
               round_end = 6,
               step = "four",
               outcome = "MCS")),
  ## ---- MCS for three waves ----
  tar_target(sensitivity_1_mcs,
    build_data(data_main,
               round_start = 3,
               round_end = 5,
               step = "three",
               outcome = "MCS")),
  ## ---- MCS for five waves ----
  tar_target(sensitivity_2_mcs,
    build_data(data_main,
               round_start = 3,
               round_end = 7,
               step = "five",
               outcome = "MCS")),
  # ---- Run MICE for MCS outcome targets ----------------------------------------
  tar_target(main_mice_mcs,
    run_mice(main_wide_mcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random),
    deployment = "worker",
    resources = resources_mice),
  tar_target(sensitivity_1_mice_mcs,
    run_mice(sensitivity_1_mcs$data,
      m = mice_m,
      maxit = mice_maxit,
      seed = seed_random),
    deployment = "worker",
    resources = resources_mice),
  tar_target(sensitivity_2_mice_mcs,
    run_mice(sensitivity_2_mcs$data,
      m = mice_m,
      maxit = mice_maxit,
      seed = seed_random),
    deployment = "worker",
    resources = resources_mice),
  # ---- PCS outcome targets --------------------------------------------------
  ## --- PCS for four waves ----
  tar_target(main_wide_pcs,
    build_data(data_main,
               round_start = 3,
               round_end = 6,
               step = "four",
               outcome = "PCS")),
  ## --- PCS for three waves ----
  tar_target(sensitivity_1_pcs,
    build_data(data_main,
               round_start = 3,
               round_end = 5,
               step = "three",
               outcome = "PCS")),
  ## --- PCS for five waves ----
  tar_target(sensitivity_2_pcs,
    build_data(data_main,
               round_start = 3,
               round_end = 7,
               step = "five",
               outcome = "PCS")),
  # --- Run MICE for PCS outcome targets ----------------------------------------
  tar_target(main_mice_pcs,
    run_mice(main_wide_pcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random),
    deployment = "worker",
    resources = resources_mice),
  tar_target(sensitivity_1_mice_pcs,
    run_mice(sensitivity_1_pcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random),
    deployment = "worker",
    resources = resources_mice),
  tar_target(sensitivity_2_mice_pcs,
    run_mice(sensitivity_2_pcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random),
    deployment = "worker",
    resources = resources_mice),
  # ---- Run gFormulaMI --------------------------
  ## ---- Marginal means for MCS outcome targets ----
  tar_target(mi_results_four_waves_mcs_marginal,
    run_gformula(wide_mids = main_mice_mcs,
                 wide_data_mi = main_wide_mcs$data,
                 intervention_pattern = main_wide_mcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime) + 0"),
    deployment = "worker",
    resources = resources_gform),
  tar_target(mi_results_three_waves_mcs_marginal,
    run_gformula(wide_mids = sensitivity_1_mice_mcs,
                 wide_data_mi = sensitivity_1_mcs$data,
                  intervention_pattern = sensitivity_1_mcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime) + 0"),
    deployment = "worker",
    resources = resources_gform),
  tar_target(mi_results_five_waves_mcs_marginal,
    run_gformula(wide_mids = sensitivity_2_mice_mcs,
                 wide_data_mi = sensitivity_2_mcs$data,
                  intervention_pattern = sensitivity_2_mcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime) + 0"),
    deployment = "worker",
    resources = resources_gform),
  ## ---- Marginal means for PCS outcome targets ----
  tar_target(mi_results_four_waves_pcs_marginal,
    run_gformula(wide_mids = main_mice_pcs,
                 wide_data_mi = main_wide_pcs$data,
                 intervention_pattern = main_wide_pcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime) + 0"),
    deployment = "worker",
    resources = resources_gform),
  tar_target(mi_results_three_waves_pcs_marginal,
    run_gformula(wide_mids = sensitivity_1_mice_pcs,
                 wide_data_mi = sensitivity_1_pcs$data,
                 intervention_pattern = sensitivity_1_pcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime) + 0"),
    deployment = "worker",
    resources = resources_gform),
  tar_target(mi_results_five_waves_pcs_marginal,
    run_gformula(wide_mids = sensitivity_2_mice_pcs,
                 wide_data_mi = sensitivity_2_pcs$data,
                 intervention_pattern = sensitivity_2_pcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime) + 0"),
    deployment = "worker",
    resources = resources_gform),
  ## ---- ATE for MCS outcome targets ----
  tar_target(mi_results_four_waves_mcs_ate,
    run_gformula(wide_mids = main_mice_mcs,
                 wide_data_mi = main_wide_mcs$data,
                 intervention_pattern = main_wide_mcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime)"),
    deployment = "worker",
    resources = resources_gform),
  tar_target(mi_results_three_waves_mcs_ate,
    run_gformula(wide_mids = sensitivity_1_mice_mcs,
                 wide_data_mi = sensitivity_1_mcs$data,
                 intervention_pattern = sensitivity_1_mcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime)"),
    deployment = "worker",
    resources = resources_gform),
  tar_target(mi_results_five_waves_mcs_ate,
    run_gformula(wide_mids = sensitivity_2_mice_mcs,
                 wide_data_mi = sensitivity_2_mcs$data,
                 intervention_pattern = sensitivity_2_mcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime)"),
    deployment = "worker",
    resources = resources_gform),
  ## ---- ATE for PCS outcome targets ----
  tar_target(mi_results_four_waves_pcs_ate,
    run_gformula(wide_mids = main_mice_pcs,
                 wide_data_mi = main_wide_pcs$data,
                 intervention_pattern = main_wide_pcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime)"),
    deployment = "worker",
    resources = resources_gform),
  tar_target(mi_results_three_waves_pcs_ate,
    run_gformula(wide_mids = sensitivity_1_mice_pcs,
                 wide_data_mi = sensitivity_1_pcs$data,
                 intervention_pattern = sensitivity_1_pcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime)"),
    deployment = "worker",
    resources = resources_gform),
  tar_target(mi_results_five_waves_pcs_ate,
    run_gformula(wide_mids = sensitivity_2_mice_pcs,
                 wide_data_mi = sensitivity_2_pcs$data,
                 intervention_pattern = sensitivity_2_pcs$intervention_pattern,
                 M = gform_M,
                 estimand = "factor(regime)"),
    deployment = "worker",
    resources = resources_gform),
  # ---- Assemble comparison tables for MCS and PCS outcomes --------------------
  tar_target(comparison_mcs_four,
             assemble_comparison(mi_results_four_waves_mcs_marginal, 
                                 mi_results_four_waves_mcs_ate)),
  tar_target(comparison_mcs_three,
             assemble_comparison(mi_results_three_waves_mcs_marginal, 
                                 mi_results_three_waves_mcs_ate)),
  tar_target(comparison_mcs_five,
             assemble_comparison(mi_results_five_waves_mcs_marginal, 
                                 mi_results_five_waves_mcs_ate)),
  tar_target(comparison_pcs_four,
             assemble_comparison(mi_results_four_waves_pcs_marginal, 
                                 mi_results_four_waves_pcs_ate)),
  tar_target(comparison_pcs_three,
             assemble_comparison(mi_results_three_waves_pcs_marginal, 
                                 mi_results_three_waves_pcs_ate)),
  tar_target(comparison_pcs_five,
             assemble_comparison(mi_results_five_waves_pcs_marginal, 
                                 mi_results_five_waves_pcs_ate)),
  # ---- Make graphs for main and sensitivity analyses -----------------------------------
  tar_target(graphs_four_waves,
             make_graphs(comparison_mcs_four,
                         comparison_pcs_four)$files,
             format = "file"),
  tar_target(graphs_three_waves,
             make_graphs(comparison_mcs_three,
                         comparison_pcs_three)$files,
             format = "file"),
  tar_target(graphs_five_waves,
             make_graphs(comparison_mcs_five,
                         comparison_pcs_five)$files,
             format = "file"),
  # ---- Effect modification (three-wave, stratified by Sex / Age / Education) ----
  ## ---- Stratified long + wide data (strata variables retained) ----
  tar_target(em_long, build_em_long(data_main)),
  tar_target(em_wide_mcs, make_em_wide(em_long, outcome = "MCS")),
  tar_target(em_wide_pcs, make_em_wide(em_long, outcome = "PCS")),
  ## ---- MICE for the effect-modification analysis ----
  tar_target(em_mice_mcs,
             run_em_mice(em_wide_mcs, outcome = "MCS",
                         m = em_mice_m, maxit = em_mice_maxit, seed = em_seed),
             deployment = "worker",
             resources = resources_mice),
  tar_target(em_mice_pcs,
             run_em_mice(em_wide_pcs, outcome = "PCS",
                         m = em_mice_m, maxit = em_mice_maxit, seed = em_seed),
             deployment = "worker",
             resources = resources_mice),
  ## ---- gFormulaMI counterfactual predictor matrices ----
  tar_target(em_predmat_mcs, make_em_predictor_matrix(em_wide_mcs, outcome = "MCS")),
  tar_target(em_predmat_pcs, make_em_predictor_matrix(em_wide_pcs, outcome = "PCS")),
  ## ---- Stratified g-formula runs (marginal + difference per stratum) ----
  tar_target(em_results_mcs,
             run_effect_modification(em_mice_mcs, em_predmat_mcs,
                                     outcome = "MCS", datasets = em_datasets),
             deployment = "worker",
             resources = resources_gform),
  tar_target(em_results_pcs,
             run_effect_modification(em_mice_pcs, em_predmat_pcs,
                                     outcome = "PCS", datasets = em_datasets),
             deployment = "worker",
             resources = resources_gform),
  ## ---- Interaction tests (global Wald chi2 + BH within outcome) ----
  tar_target(em_interaction,
             test_interaction(em_results_mcs$diff, em_results_pcs$diff)),
  ## ---- Effect-modification forest plots ----
  tar_target(em_graphs,
             make_em_graphs(em_results_mcs, em_results_pcs)$files,
             format = "file")

)

