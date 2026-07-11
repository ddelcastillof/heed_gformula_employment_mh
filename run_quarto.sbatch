#!/bin/bash
#SBATCH --job-name=tar_pipe
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=96G
#SBATCH --time=24:00:00
#SBATCH --output=logs/targets_%j.out
#SBATCH --error=logs/targets_%j.err
#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --mail-user=darwin.delcastillofernandez@glasgow.ac.uk
#SBATCH --account=none

# ---------------------------------------------------------------------------
# CONTROLLER job. It runs a single R process that calls tar_make_future(); the
# future.batchtools plan defined in _targets.R then submits each heavy target
# (MICE / g-formula, deployment = "worker") as its own transient sbatch job,
# rendered from slurm/batchtools.slurm.tmpl with that target's per-job
# ncpus/memory/walltime. Light targets run inside this job (deployment =
# "main"). This job therefore needs only light CPU but a long walltime (it
# idles while workers run) and enough memory to gather target results
# (storage = "main").
# ---------------------------------------------------------------------------

set -uo pipefail

module purge
module load apps/miniforge
source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate quarto

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export RANGER_NUM_THREADS=1

export TMPDIR="${SLURM_TMPDIR:-/tmp}"

cd "${SLURM_SUBMIT_DIR}"

mkdir -p logs

echo "Controller job ${SLURM_JOB_ID} on $(hostname)"
echo "Allocated CPUs:  ${SLURM_CPUS_PER_TASK}"
echo "sbatch on PATH:  $(command -v sbatch || echo 'NOT FOUND -> pipeline falls back to LOCAL future.callr workers!')"
echo "R: $(which R)"; R --version | head -n 1

# workers = max concurrent per-target SLURM jobs (parity with the 12 crew workers)
Rscript -e 'targets::tar_make_future(workers = 12L)' 2>&1 | tee "logs/targets_make_${SLURM_JOB_ID}.log"
status=${PIPESTATUS[0]}
echo "tar_make_future exit status: $status"

exit $status
