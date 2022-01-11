#!/bin/csh
#
# SLURM batch script to run runreal
#
#SBATCH -A K1206
#SBATCH -J runreal
#SBATCH -o runreal.out
#SBATCH -e runreal.err
#SBATCH -n 4
#SBATCH --ntasks=32
#SBATCH --ntasks-per-node=32
#SBATCH --ntasks-per-socket=16
#SBATCH --partition=workq

rm -f rsl.*
ln -sf namelist.input-realtime namelist.input
ln -sf /project/k1206/Dust_realtime/WPS/met_em.d0?* .

time srun ./real.exe

rm -f met_em.d0?*
