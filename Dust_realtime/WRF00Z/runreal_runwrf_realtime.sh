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
#rm -f *.out *.err
#rm -f wrfinput_d01 wrfinput_d02
#rm -f wrfout*00:00

hh="00"
current_ymd=`date -u +%Y%m%d`
current_time="${current_ymd}${hh}"

start_time=`perl ./advance_cymdh.pl ${current_time} 0`
save_time=`perl ./advance_cymdh.pl ${current_time} -24`
remove_time=`perl ./advance_cymdh.pl ${current_time} -144`
start_ymd=`echo ${start_time} | cut -c1-8`

mkdir -p /project/k1206/Dust_realtime/WRF00Z/${save_time}
mv -f wrfout*00:00 /project/k1206/Dust_realtime/WRF00Z/${save_time}
#rm -f wrfout*00:00 
#mv -f *.{err,out} /project/k1206/Dust_realtime/WRF00Z/${save_time}
mv -f wrfbdy_d01 /project/k1206/Dust_realtime/WRF00Z/${save_time}
mv -f wrfinput* /project/k1206/Dust_realtime/WRF00Z/${save_time}
if [ -d "/project/k1206/Dust_realtime/WRF00Z/${remove_time}" ]
then
    rm -rf /project/k1206/Dust_realtime/WRF00Z/${remove_time}
else
    echo "/project/k1206/Dust_realtime/WRF00Z/${remove_time} does not exist!"
fi

yyyy=`echo ${start_time} | cut -c1-4`
mm=`echo ${start_time} | cut -c5-6`
dy=`echo ${start_time} | cut -c7-8`
cycle=${yyyy}${mm}${dy}${hh}

day0=`perl ./advance_cymdh.pl ${start_time} 0`
day1=`perl ./advance_cymdh.pl ${start_time} 24`
day2=`perl ./advance_cymdh.pl ${start_time} 48`
day3=`perl ./advance_cymdh.pl ${start_time} 72`
day4=`perl ./advance_cymdh.pl ${start_time} 96`
day5=`perl ./advance_cymdh.pl ${start_time} 120`
day6=`perl ./advance_cymdh.pl ${start_time} 144`
yyyymmdy0=`echo ${day0} | cut -c1-8`
yyyymmdy1=`echo ${day1} | cut -c1-8`
yyyymmdy2=`echo ${day2} | cut -c1-8`
yyyymmdy3=`echo ${day3} | cut -c1-8`
yyyymmdy4=`echo ${day4} | cut -c1-8`
yyyymmdy5=`echo ${day5} | cut -c1-8`
yyyymmdy6=`echo ${day6} | cut -c1-8`

syyyy=`echo ${day0} | cut -c1-4`
smm=`echo ${day0} | cut -c5-6`
sdy=`echo ${day0} | cut -c7-8`

eyyyy=`echo ${day6} | cut -c1-4`
emm=`echo ${day6} | cut -c5-6`
edy=`echo ${day6} | cut -c7-8`

namelist_realwrf_00="namelist.input-realtime-bak"
namelist_realwrf_swap1="namelist-realwrf-swap1"
namelist_realwrf_swap2="namelist-realwrf-swap2"
namelist_realwrf_swap3="namelist-realwrf-swap3"
namelist_realwrf_swap4="namelist-realwrf-swap4"
namelist_realwrf_swap5="namelist-realwrf-swap5"
namelist_realwrf_01="namelist.input-realtime"
sed "s/SYYYY/${syyyy}/g" ${namelist_realwrf_00} > ${namelist_realwrf_swap1}
sed "s/SMM/${smm}/g" ${namelist_realwrf_swap1} > ${namelist_realwrf_swap2}
sed "s/SDY/${sdy}/g" ${namelist_realwrf_swap2} > ${namelist_realwrf_swap3}
sed "s/EYYYY/${eyyyy}/g" ${namelist_realwrf_swap3} > ${namelist_realwrf_swap4}
sed "s/EMM/${emm}/g" ${namelist_realwrf_swap4} > ${namelist_realwrf_swap5}
sed "s/EDY/${edy}/g" ${namelist_realwrf_swap5} > ${namelist_realwrf_01}
rm -f ${namelist_realwrf_swap1} ${namelist_realwrf_swap2} ${namelist_realwrf_swap3}
rm -f ${namelist_realwrf_swap4} ${namelist_realwrf_swap5}

ln -sf namelist.input-realtime namelist.input
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
