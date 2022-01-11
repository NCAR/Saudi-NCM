#!/bin/bash
#
# SLURM batch script to run runreal and runwrf
#
#SBATCH --partition=workq
#SBATCH -A K1206
#SBATCH --job-name="runrealwrf"
#SBATCH --output=runrealwrf.out
#SBATCH --err=runrealwrf.err
#SBATCH --exclusive
#SBATCH --nodes=30
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=zhangyx@ucar.edu
#SBATCH --time=08:00:00

cd /project/k1206/Dust_realtime/WRF00Z
rm -f rsl.*

ln -sf /project/k1206/Dust_realtime/WPS00Z/met_em.d0?* .

date
#srun --ntasks=64 --hint=nomultithread ./real.exe
/opt/slurm/default/bin/srun --ntasks=64 --hint=nomultithread ./real.exe

if [ $? -eq 0 ]; then
 rm -f met_em.d0?*
 echo "real.exe finished successfully. wrf.exe will run next."
 /opt/slurm/default/bin/srun --ntasks=960 --hint=nomultithread ./wrf.exe
else
 echo "real.exe did not finish successfully. Please double check."
fi

if [ $? -eq 0 ]; then
 echo "wrf.exe finished successfully. Good."
else
 echo "wrf.exe did not finish successfully. Please double check."
fi
date
