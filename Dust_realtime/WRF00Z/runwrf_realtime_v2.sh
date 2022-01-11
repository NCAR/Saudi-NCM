#!/bin/bash
#
# SLURM batch script to run runwrf
#
#SBATCH --partition=workq
#SBATCH -t 210
#SBATCH -A K1206
#SBATCH --ntasks=480
#SBATCH --ntasks-per-node=32
#SBATCH --ntasks-per-socket=16
#SBATCH --job-name="runwrf"
#SBATCH --output=runwrf.out
#SBATCH --err=runwrf.err
#SBATCH --exclusive

rm -f rsl.*
rm -f *.err *.out
rm -f wrfout*00:00
ln -sf namelist.input-realtime namelist.input

srun ./wrf.exe
