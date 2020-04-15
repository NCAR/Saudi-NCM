#!/bin/tcsh -f
#
###############################################################################
echo 
echo  " ----------------------------------------------------------------------"
echo  " --------- SPDB (VDRAS/VLAS profiles) decoder starts  --------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM SPDB_FDDA

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

set year    = `echo $obs_date | cut -c 1-4`
set month   = `echo $obs_date | cut -c 5-6`
set day     = `echo $obs_date | cut -c 7-8`
set hour    = `echo $obs_date | cut -c 9-10`
set min_beg = 00
set sec_beg = 00
set min_end = 59
set sec_end = 59

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_SPDB) then
  mkdir -p $GEAPSTMP/DECODE_SPDB
  ln -s $GEAPSTMP/DECODE_SPDB $RUNDIR/${cycle_date}/DECODE_SPDB
 endif
else
 mkdir -p $RUNDIR/${cycle_date}/DECODE_SPDB
endif

cd ${RUNDIR}/${cycle_date}/DECODE_SPDB
echo "Now working in  $cwd"

${SPDB_QUERY} -url ${SPDB_URL} \
  -start "$year $month $day $hour $min_beg $sec_beg" \
  -end "$year $month $day $hour $min_end $sec_end" > spdb_out.txt

${RD_SPDB_EXE} spdb_out.txt

if (-s spdb2littler.txt) cat spdb2littler.txt >> spdb_obs.$obs_date

if (-e spdb_out.txt) rm spdb_out.txt
if (-e spdb2littler.txt) rm spdb2littler.txt

echo "SPDB data processing finished"

exit 0
