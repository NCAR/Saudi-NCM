#!/bin/tcsh -f
set _script_name = `basename $0`
echo " === $_script_name is started at `date` ==="

set DOMAINS = "2 3"
set RANGE = ATC
set job_dir = "/raid/GMODJOBS/MAGATC"
set perl_dir = "/home/fddasys/datbin"

set _job_id = "$RANGE"
set perl_script_name = "crontab_NMQ_mergtile.pl"

##########################################################
# Overrides following local variables by using job script which ends with ".job.csh"
# - RANGE, job_dir, and perl_dir
#
set job_script = `echo $0 | sed 's/.csh/.job.csh/'`
if ( -e $job_script ) source $job_script
if ( ! $?arg_time ) set arg_time = ""
#echo "  RANGE: $RANGE, job_dir: $job_dir"
#echo "  arg_time: $arg_time, perl_dir: $perl_dir"
set pid_name = "/tmp/tmp_`basename $0 ".csh"`_${_job_id}.pid"
set perl_script_fullname = "$perl_dir/$perl_script_name"

set is_running = 0
if( -e "$pid_name" ) then
  set running_pid = `cat $pid_name`
  set is_running = `ps -ef | grep $_script_name | grep -v "grep" | grep $running_pid | wc -l`
endif
if( $is_running == 0 ) then
  if (-e "$perl_script_fullname") then
    echo $$ > $pid_name
    cd $job_dir
    #run the RANGE data
    #echo "ok to run"
    foreach domain_id ( $DOMAINS )
      #echo " calling [$perl_script_fullname $RANGE $domain_id $arg_time]"
      $perl_script_fullname $RANGE $domain_id $arg_time
      set error_code = $?
      if ($error_code < 256 && $error_code > 128 ) @ error_code -= 256
      if ( $error_code < 0 |) then
        echo " error_code: $error_code from $perl_script_name"
      endif
    end
    if ( -e "$pid_name" ) rm $pid_name
  else
    echo " ***** ERROR script $perl_dir/$perl_script_name does not exist ... *******"
  endif
else
  echo " ** INFO script $_script_name for $_job_id is running ... **"
  echo " ** INFO script $_script_name for $_job_id is running ... **" >> $rda_log_dir/${_script_name}.running
endif

echo " === $_script_name is done at `date` ==="
echo ""
