#!/bin/sh
for n in  250
  do
  eval "export n=$n"
  sbatch --array=1-251 copula/launch_copula.sh
  done
