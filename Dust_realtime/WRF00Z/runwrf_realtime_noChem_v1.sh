#!/bin/bash
#
# SLURM batch script to run runwrf
#
#SBATCH --partition=workq
#SBATCH --account=K1206
#SBATCH --job-name=runwrf
#SBATCH --output=runwrf.out
#SBATCH --error=runwrf.err
#SBATCH --exclusive
#SBATCH --nodes=4

rm -f rsl.*
rm -f *.out *.err
rm -f core

ln -sf namelist.input-realtime-noChem namelist.input

srun --ntasks=128 --hint=nomultithread --ntasks-per-node=32 --ntasks-per-socket=16./wrf.exe
