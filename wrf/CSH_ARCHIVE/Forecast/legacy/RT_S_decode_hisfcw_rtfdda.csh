#!/bin/tcsh -f
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- HISFCW decoder starts  ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM HISFCW_FDDA

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
#setenv RUNDIR /data/hahmann/
#set echo

#	Check usage

if ( ${#argv} == 1 ) then
    set cycle_date = $1
else
    echo "usage: $0 cycle_date "
    echo "where cycle_date is CCYYMMDDHH"
    exit ( 4 )
endif

if (-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/DECODE_HISFCW
  ln -s $GEAPSTMP/DECODE_HISFCW $RUNDIR/${cycle_date}/DECODE_HISFCW
else
  $MustHaveDir $RUNDIR/${cycle_date}/DECODE_HISFCW
endif

cd ${RUNDIR}/${cycle_date}/DECODE_HISFCW
echo "Now working in  $cwd"

rm -rf 20*.txt

echo "INFO: DATES=$cycle_date"

########## HSFW_DATA_DIR

#setenv HSFW_DATA_DIR /data/input/pmrf/surface
cp ${EXECUTABLE_ARCHIVE}/../CONSTANT_FILES/stations.HISFCW .
if ( -e HISFCW.${cycle_date}) rm HISFCW.${cycle_date}

foreach hh (-4 -3 -2 -1 0 1 2 3 4)
  rm -rf input output
  set cycle_date_hh = `echo $cycle_date |cut -c 1-10`
  echo "$cycle_date_hh , $hh" >! input
  ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
  set want_date = `cat output`

# Filenames are of the form CCYYMMDD00.txt
  if (-e ${HSFW_DATA_DIR}/${want_date}00.txt) then
    cp ${HSFW_DATA_DIR}/${want_date}00.txt .

# Program runs with the form Read_HIsfcw.exe CCYYMMDD
    ${EXECUTABLE_ARCHIVE}/Read_HIsfcw.exe ${want_date}  >> print.out

    if (-e HISFCW_${want_date}) then
       cat HISFCW_${want_date} >> HISFCW.${cycle_date}
    endif
  endif
end

set lss=`ls HISFCW_*`

@ n = 0
foreach i ( `ls HISFCW_*` )
@  n++
end

rm HISFCW_*
rm -rf 20*.txt

echo "${SUBSYSTEM} -- CREATED -> $n FILES OF HISFCW DATA "

exit 0
