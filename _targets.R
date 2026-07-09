# targets orchestation of employment trajectories analysis
# this pipeline will run both the primary analysis as the sensitivity analyses in parallel 
# using mirai/crew.cluster parallel orchestation

pacman::p_load(targets,
               tarchetypes,
               mirai,
               crew,
               crew.cluster,
               mori)

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
                 "flextable"),
  format     = "rds",
  seed       = 42
)

# Load all R scripts in the R/ folder
for (f in list.files("R", "\\.R$", full.names = TRUE)) source(f)

# ---- Configuration ---------------------------------------------------------
## mice configs
mice_m      <- 35
mice_maxit  <- 15
seed_random <- 42
## gFormulaMI configs
gform_M <- 50


# Define the targets pipeline
list(
  tar_target(data_main,
    if (on_slurm) {
      import_data(force = FALSE) |> clean_data() |> preproc_data()
    } else {
      import_data(force = TRUE) |> clean_data() |> preproc_data()
    }
  ),
  tar_target(main_wide,
    build_data(pop_data,
               step = "four"))
)

# ---- DAG -------------------------------------------------------------------
