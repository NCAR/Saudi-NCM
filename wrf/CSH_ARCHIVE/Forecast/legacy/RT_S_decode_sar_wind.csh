#!/bin/csh -f
#
###############################################################################
echo 
echo  " ----------------------------------------------------------------------"
echo  " ------------------ SAR_WIND decoder starts  --------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################
set echo
set timestamp
setenv SUBSYSTEM SAR_WIND_FDDA

set debug = 0
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

source ${CFILE}user.mm5sys;    

#	Check usage

if ( ${#argv} == 2 ) then
    set cycle_date = $1
    set obs_date = $2
else
    echo "usage: $0 cycle_date obs_date "
    echo "where cycle_date and obs_date is CCYYMMDDHH"
    exit ( 4 )
endif

set p1 = `echo $obs_date | cut -c 1-8`
set p2 = `echo $obs_date | cut -c 9-10`
set pattern = ${p1}_${p2}

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/RD_SAR_WIND) then
  mkdir -p $GEAPSTMP/RD_SAR_WIND
  ln -s $GEAPSTMP/RD_SAR_WIND $RUNDIR/${cycle_date}/RD_SAR_WIND
 endif
else
 mkdir -p $RUNDIR/${cycle_date}/RD_SAR_WIND
endif

cd ${RUNDIR}/${cycle_date}/RD_SAR_WIND
echo "Now working in  $cwd"

foreach obs (`ls $SAR_WIND_DATA_DIR/ASA_WSM_1PNPDK${pattern}*.nc`)

  set local_file = `echo $obs | grep -o 'ASA_WSM_1PNPDK\S*'`
  set f = $local_file:r

  cp $obs .

  ${RD_SAR_WIND_EXE} -i $local_file >> sar_wind.print_out

  if (! -e ${f}.decoded) then
      echo "Output file ${f}.decoded was not created" 
  else
     echo \
    "cat $f.decoded >> sar_wind_obs.all"
     cat $f.decoded >> sar_wind_obs.all
     rm -f $f.nc $f.decoded
   endif

end


echo "SAR_WIND data processing finished"

exit 0
