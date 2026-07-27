#!/bin/bash
#SBATCH --job-name=heed_tar
#SBATCH --time=72:00:00   # MEASURED at m=300 (job 2326516): mice_five 10.2h, then gform_five needs ~30-36h (timed out at the old 24h wall) => plan_gform_big raised to 48h. Controller must outlive its workers: 10.2 + 48 = 58.2h critical path + queueing, so 60h was too tight. 72h << 7d partition cap.
#SBATCH --cpus-per-task=2
#SBATCH --mem=64G   # controller only orchestrates; heavy objects live on workers (storage="worker")
#SBATCH --output=logs/targets_%j.out
#SBATCH --error=logs/targets_%j.err
#SBATCH --account=none
#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --mail-user=darwin.delcastillofernandez@glasgow.ac.uk

set -uo pipefail

module purge
module load apps/miniforge
source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate quarto

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export RANGER_NUM_THREADS=1

# Node-local scratch for worker spillover and targets temp files.
export TMPDIR="${SLURM_TMPDIR:-/tmp}"

echo "Job ${SLURM_JOB_ID} on $(hostname)"
echo "Allocated CPUs: ${SLURM_CPUS_PER_TASK}"
echo "R: $(which R)"; R --version | head -n 1
echo "Quarto: $(which quarto)"; quarto --version

cd "${SLURM_SUBMIT_DIR}"

Rscript -e 'targets::tar_make_future(workers = 8L)' 2>&1 | tee "logs/targets_make_${SLURM_JOB_ID}.log"
status=${PIPESTATUS[0]}
echo "tar_make_future exit status: $status"

# --- Peak-memory probe -------------------------------------------------------
# Worker targets run as separate SLURM allocations (batchtools), so their real
# peak RSS is NOT in this controller's accounting. Read it to right-size the
# tiers in _targets.R (the memory_gb per plan): compare MaxRSS against ReqMem.
echo "--- worker resource usage (right-size _targets.R tiers from MaxRSS) ---"
sacct -u "$USER" --starttime "$(date -d 'today 0:00' +%Y-%m-%dT%H:%M 2>/dev/null || echo now)" \
      --format="JobID,JobName%22,ReqMem,MaxRSS,Elapsed,State" 2>/dev/null \
      || echo "  (sacct unavailable here; on a worker id run: seff <jobid>)"

exit $status