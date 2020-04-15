#!/bin/tcsh -f
#
###############################################################################
echo
echo  " ----------------------------------------_-----------------------------"
echo  " ---------------- NPN_PROF decoder starts  -------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM NPN_PROF_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};    
#set echo

#	Check usage

if ( ${#argv} == 3 ) then
    set cycle_date = $1
    set jdayi = $2
    set obs_date = $3
else
    echo "usage: $0 cycle_date Julien_day obs_date "
    echo "where cycle_date and obs_date CCYYMMDDHH"
    echo  "d Julien_day is number of day"
    exit ( 4 )
endif

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_NPN_PROF ) then
  $MustHaveDir $GEAPSTMP/DECODE_NPN_PROF
  ln -s $GEAPSTMP/DECODE_NPN_PROF $RUNDIR/${cycle_date}/DECODE_NPN_PROF
 endif
else
$MustHaveDir $RUNDIR/${cycle_date}/DECODE_NPN_PROF
endif

cd ${RUNDIR}/${cycle_date}/DECODE_NPN_PROF
echo "Now working in  $cwd"

rm *.nc *1hr
set yy=`echo $cycle_date |cut -b 3-4`
set hh=`echo $obs_date |cut -b 9-10`
if($jdayi < 10) then
  set jday = 00$jdayi
else if($jdayi < 100) then
  set jday = 0$jdayi
else
  set jday = $jdayi
endif


cp ${NPN_PROF_DATA_DIR}/$yy$jday$hh*.nc  .

touch aaprofiler.nc
foreach prof (*wind.nc)
echo $prof
${EXECUTABLE_ARCHIVE}/npn_rt << END >>npn.exe.log
$prof
END
echo $prof 
if($prof != "aaprofiler.nc") then
mv fort.50 $RUNDIR/${cycle_date}/DECODE_NPN_PROF/PROF_NPN.${obs_date}_$prof.1h
endif
end

cp ${NPN_PROF_DATA_DIR}/$yy$jday$hh*.1hr  .
touch aaprofiler.1hr
foreach prof (*.1hr)
echo $prof
${EXECUTABLE_ARCHIVE}/npn_rt << END >>npn.exe.log
$prof
END
echo $prof
if($prof != "aaprofiler.1hr") then
mv fort.50 $RUNDIR/${cycle_date}/DECODE_NPN_PROF/PROF_NPN.${obs_date}_$prof.1h
endif
end

echo "NPN_PROF DATA for $obs_date is processed"
exit 0
