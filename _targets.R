# targets orchestation of employment trajectories analysis
# this pipeline will run both the primary analysis as the sensitivity analyses in parallel 
# using mirai/crew.cluster parallel orchestation

pacman::p_load(targets,
               tarchetypes,
               mirai,
               crew,
               crew.cluster)

# Detect SLURM at runtime, if not in cluster, run locally
on_slurm <- nzchar(Sys.getenv("SLURM_JOB_ID")) && nzchar(Sys.which("sbatch"))

# Controller setup for crew.cluster/targets
controller <- if (on_slurm) {
  crew.cluster::crew_controller_slurm(
    name            = "emp_traj",
    workers         = 12L,
    tls             = crew::crew_tls(mode = "automatic"),
    seconds_idle    = 60,
    options_cluster = crew_options_slurm(
      verbose = TRUE,
      script_lines = readLines(here::here("slurm", "preamble.sh")),
      cpus_per_task = 2,
      time_minutes  = 720,
      log_output    = "logs/crew-%A.out",
      log_error     = "logs/crew-%A.err"
    )
  )
} else {
  crew::crew_controller_local(
    name = "emp_traj",
    workers = 4L
  )
}

message("--- TARGETS PIPELINE ---")
message("hostname:        ", Sys.info()[["nodename"]])
message("SLURM_JOB_ID:    '", Sys.getenv("SLURM_JOB_ID"), "'")
message("Sys.which sbatch: '", Sys.which("sbatch"), "'")
message("on_slurm:        ", on_slurm)
message("crew.cluster ver: ", as.character(packageVersion("crew.cluster")))
message("crew ver:        ", as.character(packageVersion("crew")))
message("mirai ver:       ", as.character(packageVersion("mirai")))
message("---------------------------")

# ---- Packages attached to every target's evaluation environment ------------
tar_option_set(
  controller = controller,
  packages   = c("data.table", 
                 "here", 
                 "mice", 
                 "gFormulaMI", 
                 "magrittr", 
                 "tidyverse", 
                 "colorBlindness",
                 "flextable",
                 "bit64"),
  format     = "rds",
  seed       = 42
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
  tar_target(main_wide_mcs,
    build_data(data_main,
               round_start = 3,
               round_end = 6,
               step = "four",
               outcome = "MCS")),
  tar_target(sensitivity_1_mcs,
    build_data(data_main,
               round_start = 3,
               round_end = 5,
               step = "three",
               outcome = "MCS")),
  tar_target(sensitivity_2_mcs,
    build_data(data_main,
               round_start = 3,
               round_end = 7,
               step = "five",
               outcome = "MCS")),
  tar_target(main_mice_mcs,
    run_mice(main_wide_mcs$data,
             m = mice_m, 
             maxit = mice_maxit, 
             seed = seed_random)),
  tar_target(sensitivity_1_mice_mcs,
    run_mice(sensitivity_1_mcs$data,
      m = mice_m, 
      maxit = mice_maxit, 
      seed = seed_random)),
  tar_target(sensitivity_2_mice_mcs,
    run_mice(sensitivity_2_mcs$data,
      m = mice_m, 
      maxit = mice_maxit, 
      seed = seed_random)),
  # ---- PCS outcome targets --------------------------------------------------
  tar_target(main_wide_pcs,
    build_data(data_main,
               round_start = 3,
               round_end = 6,
               step = "four",
               outcome = "PCS")),
  tar_target(sensitivity_1_pcs,
    build_data(data_main,
               round_start = 3,
               round_end = 5,
               step = "three",
               outcome = "PCS")),
  tar_target(sensitivity_2_pcs,
    build_data(data_main,
               round_start = 3,
               round_end = 7,
               step = "five",
               outcome = "PCS")),
  tar_target(main_mice_pcs,
    run_mice(main_wide_pcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random)),
  tar_target(sensitivity_1_mice_pcs,
    run_mice(sensitivity_1_pcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random)),
  tar_target(sensitivity_2_mice_pcs,
    run_mice(sensitivity_2_pcs$data,
             m = mice_m,
             maxit = mice_maxit,
             seed = seed_random))
)

