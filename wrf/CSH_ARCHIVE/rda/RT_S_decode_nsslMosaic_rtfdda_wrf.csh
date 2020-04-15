#!/bin/tcsh  -f
# $Id: RT_S_decode_nsslMosaic_rtfdda_wrf.csh,v 1.20 2016/03/02 17:58:00 hsoh Exp $
#
# Howard Soh
#  Note: Environment variable $GSJOBDIR must set before calling this.
#        $RUNDIR from env_vars.csh.
#   Input: NSSL mosaic data (override by environment variable NSSL_MOSAIC_DATA_DIR)
#  Output: wrffdda_d0$d at cycle_codes
#
# Mei Xu   updated for wrf use  2007-09-10
# Script for preparing radar obs/analysis for use in RTFDDA grid nudging 
# (1) QR analysis from DBZ
# The input NSSL radar reflectivity is pre-mapped to the horizontal model grid
# This code maps radarDBZ to wrf grid/time by vertical interp/temporal filtering
#   and converts DBZ to QR; 
# (2) VDRAS wind
# to be added later   
# Output file is in wrffdda format and ready to be ingested by WRF-RTFDDA
#
###############################################################################
#
echo  " -----------------------------------------------------------------------"
echo  " ---------------- NSSL Mosaic decoder starts ---------------------------"
echo  " -----------------------------------------------------------------------"

set _this_script_name=`basename $0`
set rda_msg_prefix = "     RDA:"
set debug_prefix   = "  ${_this_script_name} RDA DEBUG "
set info_prefix    = "  ${_this_script_name} INFO "
set timestamp
setenv SUBSYSTEM NSSL_MOSAIC_FDDA
setenv RM "rm -rf"

set REPLACE_WRFVAR = 0
set debug = 0
if ( ! $?DOMS ) set DOMS = (2 3)

#
#	Check Arguments
#
if ( ${#argv} < 6 ) then
  echo "ERROR (usage): $_this_script_name start_date end_date this_cycle last_cycle range (interval)"
  echo "where start_date is the initial time of the MM5/WRF fdda,"
  echo "end_date is the final time of the MM5/WRF fdda; both are in CCYYMMDDHH "
  exit ( 4 )
endif

set start_date = `echo $1 | cut -c1-10`
set   end_date = `echo $2 | cut -c1-10`
set this_cycle = `echo $3 | cut -c1-10`
set last_cycle = `echo $4 | cut -c1-10`

set RANGE = $5
set NUM_DOMS = $6
if ( ${#argv} > 6 ) then
  set out_min3 = $7
else
  set out_min3 = 15
endif
set input_min = 5

if ( "$out_min3" < 5) then
  echo " **** WARN **** ${_this_script_name}: The output interval [$out_min3] should be >= 5"
else if ( "$out_min3" > 60) then
  echo " **** WARN **** ${_this_script_name}: The output interval [$out_min3] should be <= 60"
endif

#
# ENVIRONMENT
#
set _env_var_script = "$GSJOBDIR/scripts/env_vars.csh"
#echo "$0 location of environment variables: [$_env_var_script]"
if ( -e "$_env_var_script" ) then
  source $_env_var_script
  if ($debug) echo "${_this_script_name}: INFO Include environment variables: [$_env_var_script]"
endif
if (! $?MM5HOST) setenv MM5HOST `echo $RANGE`
set FDDA_EXE_HOME = "$MM5HOME/cycle_code"

if (! $?DATADIR ) then
  set DATADIR = "/rdadata"
  if ( ! -d $DATADIR ) then
    set DATADIR = "/datainput"
    if ( ! -d $DATADIR ) set DATADIR = "/raid"
  endif
endif
if (! $?EXECUTABLE_ARCHIVE || ! -d $EXECUTABLE_ARCHIVE ) then
  set EXECUTABLE_ARCHIVE = "$FDDA_EXE_HOME/EXECUTABLE_ARCHIVE"
endif

if (! $?CheckConfigFiles ) set CheckConfigFiles = "$EXECUTABLE_ARCHIVE/CheckConfigFiles"

if (! $?RUNDIR ) then
  echo "${_this_script_name}:  RUNDIR is not defined"
  set RUNDIR = "$GSJOBDIR/../../cycles/$GSJOBID/$RANGE"
endif
if (! $?CYCDIR ) then
  set CYCDIR = $RUNDIR/${this_cycle}
endif

$CheckConfigFiles
set cfstat = $status
if ( "$cfstat" != 0 ) then
  echo "${_this_script_name}: ${SUBSYSTEM} -- Missing ConfigFile -> exiting"
  exit 2
endif

#if ($debug) then
#  #echo " DEBUG: start_date: $start_date, end_date: $end_date, this_cycle: $this_cycle, last_cycle: $last_cycle"
#  #echo " DEBUG: NSSL_MOSAIC_DATA_DIR: [$NSSL_MOSAIC_DATA_DIR]"
#
#  echo "${rda_msg_prefix} nsslMosaic updating interval is hardwired at $input_min min "
#  echo "${rda_msg_prefix} output data is set as $out_min3 min for domain $DOMS "
#endif

set DECODE_NSSL_MOSAIC_DIR=$CYCDIR/DECODE_NSSL_MOSAIC
# HSoh 2013-05-15: It does not work with $GEAPSTMP/DECODE_NSSL_MOSAIC
if( ! -d $DECODE_NSSL_MOSAIC_DIR ) mkdir -p $DECODE_NSSL_MOSAIC_DIR

cd $DECODE_NSSL_MOSAIC_DIR
#echo "${rda_msg_prefix} Current directory is $DECODE_NSSL_MOSAIC_DIR "

#setenv RADAR_FOR_NUDGE_EXE ${EXECUTABLE_ARCHIVE}/radar_for_nudge.exe
setenv RADAR_TO_WRFFDDA_EXE ${EXECUTABLE_ARCHIVE}/radar_to_wrffdda.exe
if (! $?NSSL_MOSAIC_DATA_DIR) then
  if ( $?RDA_MOSAIC_MODEL_INPUT_DIR ) then
    set NSSL_MOSAIC_DATA_DIR = "$RDA_MOSAIC_MODEL_INPUT_DIR/${GSJOBID}"
  else
    echo "${info_prefix} RDA_MOSAIC_MODEL_INPUT_DIR is not defined"
    set NSSL_MOSAIC_DATA_DIR = "$DATADIR/rda/resampled/mergedRefl/${GSJOBID}"
    if ( ! -d $NSSL_MOSAIC_DATA_DIR ) then
      set NSSL_MOSAIC_DATA_DIR = "$DATADIR/resampled/mergedRefl/${GSJOBID}"
    endif
  endif
endif

# link merged files of raw dbz to cycle decoding directory
# from ($start_date-1h) to $end_date
# (back an hour for temporal filtering/interpolation purpose)
#
set ccyymmddhh_before_start = `echo "$start_date, -1" | ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe`

set ccyymmddhh_list = ""
set ccyymmddhh = $start_date
while ($ccyymmddhh <= $end_date)
  set ccyymmddhh_list = "$ccyymmddhh_list $ccyymmddhh"
  set ccyymmddhh = `echo "$ccyymmddhh, 1" | ${EXECUTABLE_ARCHIVE}/advance_cymdh.exe`
end


if ($debug) then
  echo "${rda_msg_prefix} Radar files are in ${NSSL_MOSAIC_DATA_DIR}"
  echo "${rda_msg_prefix} Linking radar files [${ccyymmddhh_before_start} (-1) to ${end_date}] for domains [$DOMS]"
endif

foreach ccyymmddhh ( $ccyymmddhh_before_start $ccyymmddhh_list )
  foreach d ( $DOMS )

    set ccyy = `echo $ccyymmddhh | cut -c1-4`
    set   mm = `echo $ccyymmddhh | cut -c5-6`
    set   dd = `echo $ccyymmddhh | cut -c7-8`
    set   hh = `echo $ccyymmddhh | cut -c9-10`

#   echo "      link ${ccyy}-${mm}-${dd}_${hh}"##"_${RANGE}_D${d}.nc"
#   ln -s -f ${NSSL_MOSAIC_DATA_DIR}/${RANGE}/${ccyy}-${mm}-${dd}_${hh}*_${s}_D${d}.nc .
#   ln -s -f ${NSSL_MOSAIC_DATA_DIR}/${RANGE}/${ccyy}-${mm}-${dd}_${hh}*_DBZ_D${d}.nc .
    set _TMP_NSSL_MOSAIC_DATA_DIR = "${NSSL_MOSAIC_DATA_DIR}/${ccyy}${mm}${dd}"
    if ( -d ${_TMP_NSSL_MOSAIC_DATA_DIR} ) then
      echo "      link ${_TMP_NSSL_MOSAIC_DATA_DIR}/${ccyy}-${mm}-${dd}_${hh}*_DBZ_D${d}.nc"
      ln -s -f ${_TMP_NSSL_MOSAIC_DATA_DIR}/${ccyy}-${mm}-${dd}_${hh}*_DBZ_D${d}.nc .
    else
      echo "      === ERROR === ${_this_script_name}: ${_TMP_NSSL_MOSAIC_DATA_DIR} does not exist at `hostname`"
    endif
  end
end

set _WRF_OUT_DIR = "$RUNDIR/${last_cycle}"
if ( -e $CYCDIR/WRFVAR) then
  set _WRF_VAR_OUT_DIR = "$CYCDIR/WRFVAR/varOut"
else
  set _WRF_VAR_OUT_DIR = "$RUNDIR/WRFVAR/varOut"
endif

foreach d ( $DOMS )
#
# d is assumed as < 10 
#  
# locate model files for the period for grid computation and background T
# - for wrf grid height, pbase and pp are needed
# - use previous cycle analysis/forecasts if available
# - if no previous cycle, use model initial condition 
# - wrfreal generated wrffdda_d0# usually do not have pp and are 3 hourly
#
  @ n_mfile = 0
#
# $n_mfile is assumed as < 100
# when multiple files are used, it is assumed that the time levels of the files are
#  in sequence and donot overlap, otherwise $RADAR_TO_WRFFDDA_EXE may not work correctly 

  set modelic = "${_WRF_OUT_DIR}/${this_cycle}_wrfinput_d0${d}"
  if (! -e $modelic ) set modelic = "${_WRF_OUT_DIR}/${this_cycle}_wrfinput_d0${d}_cold"
  set gfdda1guess = "${_WRF_OUT_DIR}/wrffdda_d0$d "

  foreach ccyymmddhh ( $ccyymmddhh_list )
    set ccyy = `echo $ccyymmddhh | cut -c1-4`
    set mm = `echo $ccyymmddhh | cut -c5-6`
    set dd = `echo $ccyymmddhh | cut -c7-8`
    set hh = `echo $ccyymmddhh | cut -c9-10`

    # WRF input file is yyyymmddhh_wrfinput_d0# or yyyymmddhh_wrfinput_d0#_cold
    # WRF forecast files are wrfout_d0#_yyyy-mm-dd_hh:mn:ss.$s_P+FCST 
    # WRF grid nudging file prepared by WRFREAL is wrffdda_d0#
    #
    # Mei Xu 20101219 
    # add provision for using 3DVAR analysed fields as model background
    #
    set wrfvar_output_name = $_WRF_VAR_OUT_DIR/${ccyymmddhh}_d0${d}.wrfvar_output
    # Do not replace WRFVAR output with forecast (failed with WRF 3.5.1)
    if ( $REPLACE_WRFVAR ) then
       if ( ! -e $wrfvar_output_name) then
         set dateStr = ${ccyy}"-"${mm}"-"${dd}"_"${hh}":00:00"
         set wrfvar_output_name = $_WRF_OUT_DIR/wrfout_d0${d}_${dateStr}.${RANGE}_F
         if (! -e $wrfvar_output_name) then
           set wrfvar_output_name = $_WRF_OUT_DIR/wrfout_d0${d}_${dateStr}.${RANGE}_P+FCST
         endif
       endif
    endif
    if ( -e $wrfvar_output_name) then
      if (${n_mfile} < 10 ) then
        ln -s -f $wrfvar_output_name wrffile_d0${d}_0${n_mfile}
      else
        ln -s -f $wrfvar_output_name wrffile_d0${d}_${n_mfile}
      endif
      @ n_mfile ++
    endif

  end  # all hours

  if ($n_mfile == 0 ) then
    # if last_cycle analysis and fcst do not exist
    if (-e $modelic ) then
      ln -s -f $modelic wrffile_d0${d}_0${n_mfile}
      @ n_mfile = 1
    endif
  endif 
  if ($n_mfile == 0 ) then
    if ( $REPLACE_WRFVAR ) then
       echo "$rda_msg_prefix Cannot find first guess file for domain$d (no ${RANGE}_F or ${RANGE}_P+FCST file)"
    else
       echo "$rda_msg_prefix Cannot find first guess file for domain$d (no WRFVAR output file)"
    endif 
    goto NEXTDOMAIN
  else
    echo "$rda_msg_prefix $n_mfile model files will be used as background"
  endif

#
# read model files, convert radar data to qr/qs based on t3D
#      and insert qr/qs as model variable fields

  set output = ${this_cycle}_RADAR_DOMAIN${d}
  if ( -e $output ) then
    rm $output
    echo "$rda_msg_prefix removing existing $output" 
    #sleep 5
  endif
  if ($d == $NUM_DOMS)  @ out_min = $out_min3
# if ($d <  $NUM_DOMS) @ out_min = 2 * $out_min3
  if ($d <  $NUM_DOMS) @ out_min =  $out_min3
  set exe_command = "$RADAR_TO_WRFFDDA_EXE wrffile_d0${d} $n_mfile $output $start_date $end_date $out_min $d"
  echo "     $exe_command"
  $exe_command  > radar_proc.out
#
  if ( -e $output ) then
    ## HSoh: Add atttibutes at RT_L_Rda.csh because $modelic does not exist yet.
    if ( -e $modelic ) then
      echo "${_this_script_name}: Copy global attributes from `basename $modelic` to $output"
      set exe_command = "$CSH_ARCHIVE/util/addAttr.csh $modelic $output"
      if ($debug) echo "${_this_script_name}: $exe_command"
      $exe_command
    endif
    mv $output ../wrffdda_d0$d
  else
    echo "${_this_script_name} **** output file [$output] (wrffdda_d0$d) was not created! "
  endif

  echo "$rda_msg_prefix NSSL MOSAIC DATA for $this_cycle domain$d is processed"
  echo "$rda_msg_prefix Interval of output fields for nudging is $out_min min for domain$d"

NEXTDOMAIN:
end
echo "${_this_script_name} Done ... "

exit 0
