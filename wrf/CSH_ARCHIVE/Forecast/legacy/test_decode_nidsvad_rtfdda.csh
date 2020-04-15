#!/bin/tcsh  -f
#
###############################################################################
echo  " ----------------------------------------_-----------------------------"
echo  " ---------------- NIDS VAD_Profile decoder starts  -------------------------------"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM VAD_PROF_FDDA

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

$MustHaveDir $RUNDIR/${cycle_date}/DECODE_VAD_PROF
cd ${RUNDIR}/${cycle_date}/DECODE_VAD_PROF
echo "Now working in  $cwd"
set ccyy = `echo $obs_date | cut -b 1-4`
set date8 = `echo $obs_date | cut -b 1-8`
set hh = `echo $obs_date | cut -b 9-10`

foreach radar (`ls ${NIDS_VAD_DATA_DIR}`)  
  if ( ! -e ${NIDS_VAD_DATA_DIR}/$radar/VAD/$date8 ) continue
# echo processing NIDS VAD from $radar $date8 $hh

  foreach prof (`ls ${NIDS_VAD_DATA_DIR}/$radar/VAD/$date8 | grep -i ${date8}.$hh`)  
    rm fort.50
    ${EXECUTABLE_ARCHIVE}/readNIDSvad.pl ${NIDS_VAD_DATA_DIR}/$radar/VAD/$date8 $prof last | ${EXECUTABLE_ARCHIVE}/vad_to_littleR.exe
    cat fort.50 >> NIDS.$obs_date
  end
end
#$CSH_ARCHIVE/Forecast/RT_all.obs_trim.domain.USA nidsvad.$obs_date
##cat nidsvad.${obs_date}.trim >> VAD.time.clean.all
##rm nidsvad.${obs_date}

echo "VAD_PROF DATA for $obs_date is processed"
exit 0
