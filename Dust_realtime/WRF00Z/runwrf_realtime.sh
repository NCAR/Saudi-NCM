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
#SBATCH --nodes=20
#SBATCH --time=08:00:00

cd /project/k1206/Dust_realtime/WRF00Z
rm -f rsl.*
rm -f *.out *.err
rm -f wrfout*00:00

ln -sf namelist.input-realtime namelist.input

#srun --ntasks=640 --hint=nomultithread ./wrf.exe
/opt/slurm/default/bin/srun --ntasks=640 --hint=nomultithread ./wrf.exe

#module load ncl
#/usr/bin/csh /project/k1206/Dust_realtime/plots.GFS/plot_00.csh
#sh /project/k1206/Dust_realtime/plots.GFS/plot_00.sh &> plot_00.log
