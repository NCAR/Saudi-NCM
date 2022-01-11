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
#SBATCH --time=02:00:00

rm -f rsl.*
rm -f *.out *.err
rm -f wrfinput_d01 wrfinput_d02
rm -f wrfout*00:00

ln -sf namelist.input-realtime namelist.input
ln -sf /project/k1206/Dust_realtime/WPS00Z/met_em.d0?* .

srun --ntasks=64 --hint=nomultithread ./real.exe

rm -f met_em.d0?*
