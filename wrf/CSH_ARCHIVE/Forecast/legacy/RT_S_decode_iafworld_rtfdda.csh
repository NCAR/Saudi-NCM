#!/bin/tcsh -f
#
###############################################################################
echo 
echo  " ----------------------------------------------------------------------"
echo  " --------------- IAF WORLD decoder starts  ----------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM IAFWORLD_FDDA

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

set p1 = `echo $obs_date | cut -c 1-8`
set p2 = `echo $obs_date | cut -c 9-10`
set pattern = ${p1}_${p2}

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_IAFWORLD) then
  mkdir -p $GEAPSTMP/DECODE_IAFWORLD
  ln -s $GEAPSTMP/DECODE_IAFWORLD $RUNDIR/${cycle_date}/DECODE_IAFWORLD
 endif
else
 mkdir -p $RUNDIR/${cycle_date}/DECODE_IAFWORLD
endif

cd ${RUNDIR}/${cycle_date}/DECODE_IAFWORLD
echo "Now working in  $cwd"

ln -sf $GTS_STTNID .
ln -sf $GTS_ICAO .

if (-e iaf_world_obs.$obs_date) rm -f iaf_world_obs.$obs_date
foreach obs (`ls $IAF_DATA_DIR/obs-world-${pattern}*`)
  set local_file = `echo $obs | grep -o 'obs-world\S*'`
  cp $obs .

  echo "$obs_date , 3" >! input
  ln -sf $local_file gts_data
  ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! gts.in
  $GTS_DECODER_EXE < gts.in >& gts_decoder.print.out

  cat *.7?? >> iaf_world_obs.$obs_date

  rm -f gts_out.* input gts.in
end

cat iaf_world_obs.$obs_date >> IAF_world_obs.all

echo "IAF world obs processing finished"

exit 
