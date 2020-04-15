# To override the input and output radar directory:
# - set environment variable NSSL_MOSAIC_DATA_DIR
# - input directory will be "$ENV{DATADIR}/radar/nsslMosaic"
# - output directory will be "$ENV{DATADIR}/radar/rda/nsslMosaic/${GSJOBID}"
#
setenv debug 1

set _script_dir=`dirname $0`
if ( "." == "$_script_dir" ) set _script_dir=`pwd`

#-----------------------
# Process arguments
#
set show_help = 0
set env_var_file = "./env_vars.csh"

set argv=`getopt f:t:j:p:h $*`
set rda_sub_script = $_script_dir/sub_process_arguments.csh
if ( -e $rda_sub_script ) then
  source $rda_sub_script
else
  echo "    ** INFO  Arguments were not processed because of missing file [${rda_sub_script}] **"
endif

if ( 1 == $show_help ) then
  echo "$0 <-f env_vars_script> <-p perl_dir> <-j job_dir> <-h>"
  echo " where  "
  echo "   -f env_var_file   e.g. /model/<Logname>/GMODJOBS/JOBID/scripts/env_vars.csh, defaults to ./env_vars.csh"
  echo "   -t yyyyddmmHH     time for merge tiles"
  echo "   -j job_dir        directory to do job"
  echo "   -p perl_dir       where the PERL script exists"
  echo "   -h                Help. get this message"
  echo "All flags are optional."
  echo " "
  exit
endif

#-----------------------------
# Gets environment variables
#
if ( -e ${env_var_file} ) then
  source ${env_var_file}
  echo " env_var_file: $env_var_file"
  
  set env_getter_pl = get_env_from_perl.pl
  if ( $?MM5HOME ) then
    set env_getter_pl = "$MM5HOME/cycle_code/PERL/tools/get_env_from_perl.pl"
  endif
  
  #echo " env_getter_pl: $env_getter_pl"
  if (-f "$env_getter_pl")  then
     if ( ! $?RANGE ) set RANGE = "`perl $env_getter_pl RANGE`"
     if ( ! $?DATADIR ) set DATADIR = "`perl $env_getter_pl DATADIR`"
     if ( ! $?RUNDIR ) set RUNDIR = "`perl $env_getter_pl RUNDIR`"
   endif
   #echo " RANGE: $RANGE, DATADIR: $DATADIR, RUNDIR: $RUNDIR"
else
  echo "    ** INFO  env_vars [${env_var_file}] does not exist **"
endif

#------------------------------------
# Override local variables:
#------------------------------------
#
set rda_sub_script = $_script_dir/sub_rda_settings.csh
if ( -e $rda_sub_script ) source $rda_sub_script

if ( ! $?arg_time ) set arg_time = ""

set pid_name = "/tmp/tmp_`basename $0 ".csh"`_${RANGE}.pid"

set is_running = 0
if( -e "$pid_name" ) then
  set running_pid = `cat $pid_name`
  set is_running = `ps -ef | grep $_script_name | grep -v "grep" | grep $running_pid | wc -l`
endif
if( $is_running != 0 ) then
  echo " ** INFO script $_script_name for $RANGE is running ... **"
  echo " ** INFO script $_script_name for $RANGE is running at `date` ... **" >> $rda_log_dir/${_script_name}.running
  exit -1
endif

