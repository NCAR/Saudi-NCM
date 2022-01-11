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
#SBATCH --nodes=1
#SBATCH --time=08:00:00

cd /project/k1206/Dust_realtime/WRF00Z

module load ncl
/sw/xc40cle7/ncl/6.6.2/sles15_binary/bin/ncl /project/k1206/Dust_realtime/plots.GFS/plot_DUST_cities_D01_GFS00Z_realtime.ncl
