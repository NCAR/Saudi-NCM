#!/bin/tcsh -f
#
###############################################################################
echo 
echo  " ----------------------------------------------------------------------"
echo  " ------------------ AMV decoder starts  --------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM AMV_FDDA
setenv BUFR_TABLES /opt/bufr_ecmwf/bufrtables/

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
#source ${CFILE}user.mm5sys.${MM5HOST};    
source ${CFILE}user.mm5sys;    
#set echo

#	Check usage

if ( ${#argv} == 2 ) then
    set cycle_date = $1
    set obs_date = $2
else
    echo "usage: $0 cycle_date obs_date "
    echo "where cycle_date and obs_date is CCYYMMDDHH"
    exit ( 4 )
endif

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_AMV) then
  mkdir -p $GEAPSTMP/DECODE_AMV
  ln -s $GEAPSTMP/DECODE_AMV $RUNDIR/${cycle_date}/DECODE_AMV
 endif
else
 mkdir -p $RUNDIR/${cycle_date}/DECODE_AMV
endif

cd ${RUNDIR}/${cycle_date}/DECODE_AMV
echo "Now working in  $cwd"

if ( -e amv.$obs_date ) rm -f amv.$obs_date
foreach obs (`ls $AMV_DATA_DIR/${obs_date}*`)
  set local_file = `echo $obs | grep -o '\d*\w*$'`
  cp $obs .

  ${RD_AMV_EXE} -in $obs -out amv.txt >> print.out.amv

  cat  amv.txt >> amv.$obs_date
  rm -f amv.txt

end

cat amv.$obs_date >> AMV_obs.all

echo "AMV data processing finished"

exit 0
