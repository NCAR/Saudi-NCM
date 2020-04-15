#!/bin/tcsh -f
#
###############################################################################
echo ""
echo  "-----------------------------------------------------------------------"
echo  " ---------------- New Range profiler decoder starts -------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM PROF_FDDA

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

# ES temporary: on 'newer' clusters ncdump should be in /opt/netcdf/bin
if (-e /opt/netcdf/bin/ncdump) then
  set path = ( /opt/netcdf/bin $path )
else if ( $?NETCDF ) then
  set path = ( $NETCDF/bin $path )
endif

#set echo

#	Check usage

if ( ${#argv} == 5 ) then
    set stage = $1
    set cycle_date = $2
    set obs_date_prev = $3
    set begin_date = $4
    set end_date = $5
else
    echo "usage: $0 stage cycle_date obs_date_prev begin_date end_date"
    echo "where stage is either C-stage, F-stage, or P-stage"
    echo "cycle_date and obs_date_prev are in CCYYMMDDHH"
    echo "begin_date and end_date are in CCYYMMDDHH"
    exit ( 4 )
endif

set obs_date = $cycle_date

if(-d $GEAPSTMP) then
  mkdir -p $GEAPSTMP/DECODE_PROF
  ln -s $GEAPSTMP/DECODE_PROF $RUNDIR/${cycle_date}/DECODE_PROF
else
  mkdir -p $RUNDIR/${cycle_date}/DECODE_PROF
endif

cd ${RUNDIR}/${cycle_date}/DECODE_PROF

#foreach range (DPG EPG YPG WSMR ATC CRTC RTTC NVL IAF)
foreach range ( DPG )
  rm -rf prof*
  
  set t_prof_data_dir = ""
  if($range == DPG) then
    set t_prof_data_dir = ${PROF_DATA_DPG_DIR}
  endif

  set prof_date_prev = `echo "${obs_date_prev}" | cut -c 3-8`
  set prof_date = `echo "${obs_date}" | cut -c 3-8`
  echo "INFO: DATES=$prof_date_prev $prof_date"
  
  cp -p ${t_prof_data_dir}/prof*${prof_date_prev}* .
  cp -p ${t_prof_data_dir}/prof*${prof_date}* .
  
 #if( -e $range.prof.$prof_date) rm $range.prof.$prof_date
  foreach f ( `ls -1 prof*${prof_date_prev}* prof*${prof_date}*` )
    ${EXECUTABLE_ARCHIVE}/decode_profiler.exe -f $f -b $begin_date -e $end_date
  # if (-e fort.31) then
  #    cat fort.31 >> $range.prof.$prof_date
  #    rm -f fort.31
  # endif
    foreach out ( `ls -1 ??????????????_PROF` )
      mv $out $range.$out
    end
  end # f
end  #range

if( -e ${obs_date}.PROF.allrange) rm ${obs_date}.PROF.allrange
#foreach range (DPG YPG WSMR ATC CRTC EPG RTTC NVL IAF)
foreach range (DPG)
  cat $range.*_PROF >> ${obs_date}.PROF.allrange
  if ( $stage == 'P-stage' ) then
     cat $range.*_PROF >> $range.prof.$prof_date
     rm -f $range.*_PROF
  endif
end  #range

exit 0
