#!/bin/bash
#
# SLURM batch script to run runwrf
#
#SBATCH --partition=workq
#SBATCH -A K1206
#SBATCH --job-name="runwrf"
#SBATCH --output=runwrf.out
#SBATCH --err=runwrf.err
#SBATCH --exclusive
#SBATCH --nodes=2

rm -f rsl.*
rm -f *.out *.err

#cp -f namelist.input-wrftime-noChem namelist.input

srun --ntasks=64 --hint=nomultithread ./wrf.exe
