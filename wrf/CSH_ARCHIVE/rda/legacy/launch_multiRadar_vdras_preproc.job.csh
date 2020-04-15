#!/bin/csh -f

#-----------------------
# Process arguments
#
set debug = 0
set env_var_file = "./env_vars.csh"
set begin_time = `date +%Y%m%d%H%M%S`

set argv=`getopt f:r:t:hlm $*`
#echo " argv: $argv"

set rda_sub_script = $_script_dir/sub_process_arguments.csh
if ( -e $rda_sub_script ) then
  source $rda_sub_script
else
  echo "  ** INFO  $rda_sub_script does not exist **"
endif

if ( $?show_help && 1 == $show_help ) then
  echo "  $_script_name <-f env_var_file> <-r RANGE> <-t YYYYMMDDHH> <-m> <-h> "
  echo "    where  "
  echo "      -f env_var_file:  e.g. /raidN/Logname/GMODJOBS/JOBID/scripts/env_vars.csh, defaults to ./env_vars.csh"
  echo "      -r RANGE"
  echo "      -t YYYYMMDDHH  start time if running batch job to process back data, specify time"
  echo "      -l             log the execution time"
  echo "      -m             disable adjoint_moist for testing"
  echo "   -h                Help. get this message"
  echo "  All flags are optional."
  echo " "
  exit
endif

if ( $?extra_arg ) then
  set begin_time = $extra_arg
  echo  " extra_arg: $extra_arg"
endif

if ( $?arg_time ) then
  set begin_time = $arg_time
endif
echo " begin_time: [$begin_time]"

#-----------------------------
# Gets environment variables
#
set env_getter_pl = get_env_from_perl.pl
if ( -e ${env_var_file} ) then
  source ${env_var_file}
  
  set adjoint_dir = "$GSJOBDIR/rda"
  if ( $?PERL_ARCHIVE ) then
    set env_getter_pl = "$PERL_ARCHIVE/tools/get_env_from_perl.pl"
  else if ( $?MM5HOME ) then
    set env_getter_pl = "$MM5HOME/cycle_code/PERL/tools/get_env_from_perl.pl"
  endif
  
  #echo " env_getter_pl: $env_getter_pl"
  if (-f "$env_getter_pl")  then
     if ( ! $?RANGE ) set RANGE = "`perl $env_getter_pl RANGE`"
     if ( ! $?RUNDIR ) set RUNDIR = "`perl $env_getter_pl RUNDIR`"
     if ( ! $?EXECUTABLE_ARCHIVE ) set EXECUTABLE_ARCHIVE = "`perl $env_getter_pl EXECUTABLE_ARCHIVE`"
     if ( ! -d $EXECUTABLE_ARCHIVE ) set EXECUTABLE_ARCHIVE = "`perl $env_getter_pl EXECUTABLE_ARCHIVE`"
     set range = $RANGE
  else
     echo "  $_script_name $env_getter_pl does not exist"
  endif
else
  echo " $0  ${env_var_file} does not exist"
endif

if ( $debug == 1) then
  echo "   DEBUG $0 RANGE: $RANGE, RUNDIR: $RUNDIR"
  echo "            EXECUTABLE_ARCHIVE: $EXECUTABLE_ARCHIVE"
endif

set adjoint_env = "$adjoint_dir/adjoint.env"
if ( -e $adjoint_env ) then
  source $adjoint_env
else
  limit stacksize unlimited
  setenv OMP_NUM_THREADS 2
  setenv MP_THREAD_COUNT 2
  setenv MPI_MAX_PROCESS 2
  setenv MPSTKZ 256M
endif


#------------------------------------
# Sets local variables:
#------------------------------------
#

set rda_sub_script = $_script_dir/sub_rda_settings.csh
if ( -e $rda_sub_script ) source $rda_sub_script
  
#
# Override for operational systems
#
if ( ! $?LEVELII_RADAR_DATA_DIR ) then
  #set LEVELII_RADAR_DATA_DIR = /rdadata/input/levelII.mdv
  set LEVELII_RADAR_DATA_DIR = "`perl $env_getter_pl RDA_LEVELII_INPUT_DIR`"
endif
set ppi_dir = "$LEVELII_RADAR_DATA_DIR"

if ( ! $?WRF_OUT_DIR ) then
  if ( $?RUNDIR ) set WRF_OUT_DIR = ${RUNDIR}/rda/wrf
endif
if ( $?WRF_OUT_DIR ) set wrf_dir = "${WRF_OUT_DIR}"

if ( $?MM5HOME ) then
  set adjoint_moist = "$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/adjoint_moist"
endif
if ( ! -f $adjoint_moist ) then
  if ( $?EXECUTABLE_ARCHIVE && -d $EXECUTABLE_ARCHIVE ) then
    set adjoint_moist = "$EXECUTABLE_ARCHIVE/adjoint_moist"
  endif
endif
  
if ($?GSJOBDIR && -e "$GSJOBDIR" ) then
  #
  # Input: $ppi_dir and $wrf_dir
  # Output: $LEVELII_RADAR_DATA_DIR
  #      ppi_dir = "/d1/projectData/multipleRadarIngest/mdv"
  #      wrf_dir = "/raid/fieldData/nexrad_level2/wrfout/wrfout_d02_"
  #      radar_loc_file = "/raid/fieldData/nexrad_level2/Run/radar_HRRR_info"
  #      params_file = "/raid/fieldData/nexrad_level2/Run/adjoint_params.template"
  #      vcp_file = "/raid/fieldData/nexrad_level2/Run/vcp*"
  #      run_dir = "/raid/fieldData/nexrad_level2/Run"
  #      out_dir = "/raid/fieldData/nexrad_level2"
  #
  
  set params_dir = $GSJOBDIR/rda
  #set run_dir = $GSJOBDIR/rda
  set run_dir = $RUNDIR/rda
  if ( -d /dev/shm ) then
    if ( -d /dev/shm/$LOGNAME ) then
      set out_dir = "/dev/shm/$LOGNAME/rda/nexrad_level2/$GSJOBID"
    else
      set out_dir = "/dev/shm/rda/nexrad_level2/$GSJOBID"
    endif
    set run_dir = /dev/shm/$LOGNAME/rda/work
  else
    set out_dir = "`echo $GSJOBDIR | sed 's/GMODJOBS/work/g'`/rda"
  endif
  if ( ! -d $out_dir ) mkdir -p $out_dir
  set LittleR_out_dir = "$out_dir/ascii"
  
endif

if ( ! -e $run_dir ) mkdir -p $run_dir
if ( ! -e $out_dir ) mkdir -p $out_dir
if ( ! -e $LittleR_out_dir ) mkdir -p $LittleR_out_dir

set radar_loc_file = "$params_dir/radar_HRRR_info"
set params_file = "$params_dir/adjoint_params.template"
set vcp_file = "$params_dir/vcp.*"

#if ( $debug == 1) then
#  echo "   out_dir: $out_dir"
#  echo "   run_dir: $run_dir"
#endif

setenv RDA_RUN_DIR $run_dir
setenv LEVELII_RADAR_DATA_WORK_DIR $LittleR_out_dir
setenv RDA_PARAMS_DIR $params_dir

#printenv | grep RDA

set radarlist_level2 = "$params_dir/radarlist.level2.${RANGE}${start_domain}";
if ( -e $radarlist_level2 ) set RADAR_LIST = "`cat $radarlist_level2`"
