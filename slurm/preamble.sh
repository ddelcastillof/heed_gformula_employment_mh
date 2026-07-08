#SBATCH --account=none

set -euo pipefail

module purge
module load apps/miniforge

source "$(conda info --base)/etc/profile.d/conda.sh"
conda activate quarto

# preventing parallelisation issues with OpenMP, OpenBLAS, MKL, and RANGER
export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export RANGER_NUM_THREADS=1

# node-local scratch for temporary files (e.g., imputed datasets)
export TMPDIR="${SLURM_TMPDIR:-/tmp}"

# retrieving job information
echo "Job ${SLURM_JOB_ID} on $(hostname)"
echo "Allocated CPUs: ${SLURM_CPUS_PER_TASK}"
echo "R: $(which R)"; R --version | head -n 1
echo "Quarto: $(which quarto)"; quarto --version
cd "${SLURM_SUBMIT_DIR}"
