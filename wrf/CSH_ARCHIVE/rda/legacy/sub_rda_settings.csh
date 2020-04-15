#!/bin/tcsh -f

set dynamic_domains = 0

set DOMAINS = (2 3)
if ( 1 == $dynamic_domains ) then
  #-----------------------------------------------------
  # Borrowed from RT_S_decode_nsslMosaic_rtfdda_wrf.csh
  #
  if($?NUM_DOMS) then
    if ($NUM_DOMS == 5) then
      set DOMAINS = ( 4 5 )
    else if ($NUM_DOMS == 4) then
      set DOMAINS = ( 3 4)
    else if ($NUM_DOMS == 3) then
      set DOMAINS = ( 2 3)
    else if ($NUM_DOMS == 2) then
      set DOMAINS = ( 2 )
    else if ($NUM_DOMS == 1) then
      set DOMAINS = ( 1 )
    endif
  #else
  #  set NUM_DOMS = 3
  endif
endif


#------------------------------------
# Sets perl_dir: where the PERL script is.
#
if ( $?perl_script_name ) then
  if ( ! $?perl_dir ) set perl_dir = "$_script_dir"
  if ( ! -e $perl_dir/$perl_script_name ) then
    set perl_dir = "$_script_dir"
    if ( ! -e $perl_dir/$perl_script_name ) then
      if ( $?MM5HOME ) then
        set perl_dir = "$MM5HOME/cycle_code/CSH_ARCHIVE/rda"
      endif
      if ( ! -e $perl_dir/$perl_script_name ) then
        set perl_dir = "/home/$LOGNAME/perl"
      endif
      if ( ! -e $perl_dir/$perl_script_name ) then
        set perl_dir = "/home/$LOGNAME/datbin"
      endif
    endif
  endif
endif

#------------------------------------
# Sets job_dir from $GSJOBDIR where the configuration/parameters are.
#
#if ( $?job_dir ) then
#  if ( ! -d "$job_dir" ) then
    if ($?RUNDIR && -d "$RUNDIR" ) then
      set job_dir = $RUNDIR/rda
    else if ($?GSJOBDIR && -d "$GSJOBDIR" ) then
      set job_dir = $GSJOBDIR/rda/run
    else
      set job_dir = "/model/$LOGNAME/GMODJOBS/$GSJOBID"
      if ( ! -d "$job_dir" ) set job_dir = "/raid/GMODJOBS/$GSJOBID"
    endif
    if ( ! -d "$job_dir" ) mkdir -p $job_dir
#  endif
#endif

#------------------------------------
# Sets RAW_LEVELII_RADAR_DATA_DIR.
#
#if ( ! $?RAW_LEVELII_RADAR_DATA_DIR ) then
#  if ( $?DATADIR ) then
#    setenv RAW_LEVELII_RADAR_DATA_DIR $DATADIR/radar/levelII.mdv
#  else if ( -e /datainput/radar/levelII.mdv ) then
#    setenv RAW_LEVELII_RADAR_DATA_DIR /datainput/radar/levelII.mdv
#  else
#    setenv RAW_LEVELII_RADAR_DATA_DIR /datainput/radar/levelII.mdv
#  endif
#endif

#------------------------------------
# Sets RAW_NSSL_MOSAIC_DATA_DIR.
#
#if ( ! $?RAW_NSSL_MOSAIC_DATA_DIR ) then
#  if ( $?DATADIR ) then
#    setenv RAW_NSSL_MOSAIC_DATA_DIR $DATADIR/radar/nsslMosaic
#  else if ( -e /datainput/radar/levelII.mdv ) then
#    setenv RAW_NSSL_MOSAIC_DATA_DIR /datainput/radar/nsslMosaic
#  else
#    setenv RAW_NSSL_MOSAIC_DATA_DIR /datainput/radar/nsslMosaic
#  endif
#endif

#setenv RAW_LEVELII_RADAR_DATA_DIR $DATADIR/radar/levelII.mdv

#------------------------------------
# Sets rda_log_dir for RDA from $GSJOBDIR.
#

if ( -e /model/$LOGNAME/logs ) then
  set rda_log_dir = /model/$LOGNAME/logs/rda/$GSJOBID
  if ( ! -e $rda_log_dir ) mkdir -p $rda_log_dir
else if ($?GSJOBDIR && -e "$GSJOBDIR" ) then
  if (-d "$GSJOBDIR/logs") then
    set rda_log_dir = "$GSJOBDIR/logs/rda"
    if ( ! -e $rda_log_dir ) mkdir -p $rda_log_dir
  endif
endif

if ( $?GSJOBID ) then
  set _job_id = "${GSJOBID}"
else
  if ( $?RANGE ) set _job_id = "${RANGE}"
endif

