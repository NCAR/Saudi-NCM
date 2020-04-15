#!/bin/csh -f
set _script_dir=`dirname $0`
if ( "." == "$_script_dir" ) then
  set _script_dir=`pwd`
endif

#-----------------------
# Process arguments
#
set debug = 1
set env_var_file = "./env_vars.csh"

set argv=`getopt f:h $*`
#set argsstr="$argv"

foreach name ($argv)
  switch ($name)
    case -f:
      set env_var_file = $2
      breaksw
    case -h:
      echo "$0 <-f env_vars_script>"
      echo " where  "
      echo "   -f env_var_file:  e.g. /ops/<Logname>/GMODJOBS/JOBID/scripts/env_vars.csh, defaults to ./env_vars.csh"
      echo "All flags are optional."
      echo " "
      exit
      breaksw
    case --:
      breaksw
  endsw

  shift
end

#-----------------------------
# Gets environment variables
#
if ( -e ${env_var_file} ) then
  #echo "source ${env_var_file}"
  source ${env_var_file}
endif

#------------------------------------
# - Set GSJOBDIR if not defined: GSJOBDIR = where the configuration/parameters are.
# - perl_dir: where the PERL script is.
#
set _perl_script_name = "clean_NMQ_data.pl"

set perl_dir = "$_script_dir"
if ( ! -e $perl_dir/$_perl_script_name ) then
  set perl_dir = "$MM5HOME/cycle_code/CSH_ARCHIVE/rda"
  if ( ! -e $perl_dir/$_perl_script_name ) then
    set perl_dir = "/home/$LOGNAME/datbin"
  endif
endif

#if ( $?GSJOBDIR && -e "$GSJOBDIR" ) then
#  set job_dir = $GSJOBDIR
#else
#  set job_dir = "/raid/GMODJOBS/GWATC"
#endif

if ( ! $?NSSL_MOSAIC_DATA_DIR && $?DATADIR ) then
  set NSSL_MOSAIC_DATA_DIR = "$DATADIR/radar/rda/nsslMosaic/${GSJOBID}"
endif
if ( $?NSSL_MOSAIC_DATA_DIR && -e $NSSL_MOSAIC_DATA_DIR ) then
  set out_dir = "$NSSL_MOSAIC_DATA_DIR"
else
  set out_dir = "/raid/radar_ingest/nsslMosaic/ATC"
endif

#cd $job_dir 

# clean the data of fifth day (older than -120 hour = 5 * 24 hour).
$perl_dir/$_perl_script_name $out_dir -120
