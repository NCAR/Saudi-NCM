#!/bin/bash
#
# SLURM batch script to run runreal
#
#SBATCH --partition=workq
#SBATCH -A K1206
#SBATCH --job-name="runreal"
#SBATCH --output=runreal.out
#SBATCH --err=runreal.err
#SBATCH --exclusive
#SBATCH --nodes=2

rm -f rsl.*
rm -f *.out *.err
rm -f wrfout*00:00

ln -sf namelist.input-realtime-noChem namelist.input
ln -sf /project/k1206/Dust_realtime/WPS/met_em.d0?* .

srun --ntasks=32 --hint=nomultithread ./real.exe

rm -f met_em.d0?*
