#!/bin/csh
#
# SLURM batch script to run runwrf
#
#SBATCH -A K1206
#SBATCH -J runwrf
#SBATCH -o runwrf.out
#SBATCH -e runwrf.err
#SBATCH -n 4
#SBATCH --ntasks=32
#SBATCH --ntasks-per-node=32
#SBATCH --ntasks-per-socket=16
#SBATCH --partition=workq

rm -f rsl.*
ln -sf namelist.input-realtime namelist.input

time srun ./wrf.exe
