#!/bin/tcsh  -f
#
###############################################################################
echo
echo  " ----------------------------------------_-----------------------------"
echo  " ---------------- VVP_Profile decoder starts  -----------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM VVP_PROF_FDDA

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

if ( ${#argv} == 2 ) then
    set cycle_date = $1
    set obs_date = $2
else
    echo "usage: $0 cycle_date obs_date "
    echo "where cycle_date and obs_date are CCYYMMDDHH"
    exit ( 4 )
endif

if(-d $GEAPSTMP) then
 if( ! -d $GEAPSTMP/DECODE_VVP_PROF) then
  $MustHaveDir $GEAPSTMP/DECODE_VVP_PROF
  ln -s $GEAPSTMP/DECODE_VVP_PROF $RUNDIR/${cycle_date}/DECODE_VVP_PROF
 endif
else
 $MustHaveDir $RUNDIR/${cycle_date}/DECODE_VVP_PROF
endif
cd ${RUNDIR}/${cycle_date}/DECODE_VVP_PROF
echo "Now working in  $cwd"

set ccyy = `echo $obs_date | cut -b 1-4`
set date8 = `echo $obs_date | cut -b 1-8`
set hh = `echo $obs_date | cut -b 9-10`

  if( -e VVP.$obs_date) rm VVP.$obs_date
  if( -e vvp.txt ) rm vvp.txt
  if( -e VVP_littleR ) rm VVP_littleR

  cp ${RADAR_VVP_DATA_DIR}/VVPascii/vvp.$obs_date vvp.txt
  ${EXECUTABLE_ARCHIVE}/vvp_to_littleR.exe
  mv VVP_littleR radarVVP.$obs_date

echo "VVP_PROF DATA for $obs_date is processed"
exit 0
