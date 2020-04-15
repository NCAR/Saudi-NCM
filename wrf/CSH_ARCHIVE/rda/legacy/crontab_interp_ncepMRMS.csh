#!/bin/csh -f
set _script_name = `basename $0`
echo " === $_script_name is started at `date` ==="

set DOMAINS = "2 3"
set range = "CONUS"
set job_dir = "/modelrd/c4test/GMODJOBS/GE5CN4K"
set perl_dir = "/home/c4test/datbin"
set perl_dir = `dirname $0`


##########################################################
# Overrides following local variables.
# - RANGE, job_dir, and perl_dir
# Must set pid_name, _script_name.
set job_script = `dirname $0`/crontab_job.csh
if ( -e $job_script ) source $job_script

echo "  RANGE: $RANGE, job_dir: $job_dir"
echo "  arg_time: $arg_time, perl_dir: $perl_dir"

set perl_script_name = "crontab_interp_ncepMRMS.pl"
set perl_script_fullname = "$perl_dir/$perl_script_name"

#cd $job_dir
#set str = `ps -ef |grep Brontab_MRMS_interpNCEP_wps.pl|grep -v grep`
## to avoid concurrent jobs, change Brontab to crontab
#if( $#str == 0 ) then
#  echo "ok to run"
#setenv GSJOBDIR $job_dir
#$perl_dir/$perl_script_name $range $dom
#endif


if (-e "$perl_script_fullname") then
  echo $$ > $pid_name
  #run the RANGE data
  foreach domain_id ( $DOMAINS )
    #echo " calling [$perl_script_fullname $RANGE $domain_id $arg_time]"
    $perl_script_fullname $RANGE $domain_id $arg_time
    set error_code = $?
    if ( $error_code < 0 || $error_code > 128 ) then
      echo " error_code: $error_code from $perl_script_name"
    endif
  end
  if ( -e "$pid_name" ) rm $pid_name
else
  echo " ***** ERROR script $perl_dir/$perl_script_name does not exist ... *******"
endif

echo " === $_script_name is done at `date` ==="
echo ""
