#!/bin/bash
#SBATCH --job-name=recombine
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=00:10:00
#SBATCH --partition=nova_short
#SBATCH --mail-user=szp0237@auburn.edu
#SBATCH --mail-type=NONE
#SBATCH --output copula/outfile/outfile_recombine.out
module load  R/4.3.2
INFILE=copula/recombine.R
OUTFILE=copula/report/recombine.Rout
srun R CMD BATCH $INFILE $OUTFILE
