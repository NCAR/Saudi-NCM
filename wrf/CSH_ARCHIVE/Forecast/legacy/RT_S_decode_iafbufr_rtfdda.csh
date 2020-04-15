#!/bin/tcsh -f
#
###############################################################################
echo 
echo  " ----------------------------------------------------------------------"
echo  " --------------- IAF BUFR decoder starts  ----------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM IAFBUFR_FDDA
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

set p1 = `echo $obs_date | cut -c 1-8`
set p2 = `echo $obs_date | cut -c 9-10`
set pattern = ${p1}_${p2}

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_IAFBUFR) then
  mkdir -p $GEAPSTMP/DECODE_IAFBUFR
  ln -s $GEAPSTMP/DECODE_IAFBUFR $RUNDIR/${cycle_date}/DECODE_IAFBUFR
 endif
else
 mkdir -p $RUNDIR/${cycle_date}/DECODE_IAFBUFR
endif

cd ${RUNDIR}/${cycle_date}/DECODE_IAFBUFR
echo "Now working in  $cwd"

if ( -e iaf_bufr_obs.$obs_date) rm -f iaf_bufr_obs.$obs_date
foreach obs (`ls $IAF_BUFR_DIR/data-world-${pattern}*`)
  set local_file = `echo $obs | grep -o 'data-world\S*'`
  cp $obs .

  set datetime = `echo $local_file | grep -o '[0-9]*_[0-9]*.[0-9]*'`
  set log = ${datetime}.log

  $RD_BUFR_EXE -in $local_file -out out.txt -DEBUG >& $log

  cat out.txt >> iaf_bufr_obs.$obs_date

  rm -f out.txt
end

cat iaf_bufr_obs.$obs_date >> IAF_bufr_obs.all

echo "IAF BUFR obs processing finished"

exit 
