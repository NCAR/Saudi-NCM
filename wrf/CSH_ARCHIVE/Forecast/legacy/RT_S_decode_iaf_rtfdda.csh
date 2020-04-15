#!/bin/tcsh -f
#
###############################################################################
echo 
echo  " ----------------------------------------------------------------------"
echo  " ------------------ IAF decoder starts  --------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM IAF_FDDA

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
 if( ! -d $GEAPSTMP/DECODE_IAF) then
  mkdir -p $GEAPSTMP/DECODE_IAF
  ln -s $GEAPSTMP/DECODE_IAF $RUNDIR/${cycle_date}/DECODE_IAF
 endif
else
 mkdir -p $RUNDIR/${cycle_date}/DECODE_IAF
endif

cd ${RUNDIR}/${cycle_date}/DECODE_IAF
echo "Now working in  $cwd"

cp -f $CONSTANT_FILES/stationlist.iaf .

if (-e iaf_obs.$obs_date) rm -f iaf_obs.$obs_date
foreach obs (`ls $IAF_DATA_DIR/obs-{idf,israel}-${pattern}*`)
  set local_file = `basename $obs`
  cp $obs .

  dos2unix $local_file
  dos2unix $local_file  # some of the files need the coversion twice!!

  echo $local_file | egrep 'TEMP|\.U' > /dev/null

  if ( $? == 0 ) then
     echo Rewrite $local_file
     ${CSH_ARCHIVE}/Forecast/rewrite_TEMP.pl $local_file
     mv ${local_file}.tmp $local_file
  endif

  ${RD_IAF_EXE} $local_file >> print.out.iaf

  if (-e fort.21) cat fort.21 >> iaf_obs.$obs_date
  rm -f fort.21

end

cat iaf_obs.$obs_date >> IAF_obs.all

echo "IAF data processing finished"

exit 0
