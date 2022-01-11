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

export ATP_ENABLED=1
ulimit -c unlimited

rm -f rsl.*
rm -f *.err *.out
ln -sf namelist.input-realtime namelist.input

srun --ntasks=32 --hint=nomultithread ./wrf.exe
