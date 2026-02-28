#!/bin/bash
#SBATCH --partition=nova_short
#SBATCH --time=00-00:30:00
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
#SBATCH --mail-user=szp0237@auburn.edu
#SBATCH --job-name=copula
#SBATCH --mail-type=NONE
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null
module load R/4.3.2

INFILE=copula/copula.R
OUTFILE=copula/report/report_${n}_${SLURM_ARRAY_TASK_ID}.Rout
OUTLOG=copula/outfile/outfile_${n}_${SLURM_ARRAY_TASK_ID}.out
exec > $OUTLOG 2>&1
srun R CMD BATCH --no-save --no-restore $INFILE $OUTFILE
