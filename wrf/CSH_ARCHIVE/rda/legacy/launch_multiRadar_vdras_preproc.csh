#!/bin/csh -f
# $Id: launch_multiRadar_vdras_preproc.csh,v 1.13 2017/01/20 20:12:54 hsoh Exp $
#

set adjoint_file = "/home/fddasys/radar_src/RT_DECODE_NEXRAD_LEVEL2/adjoint.env"
if ( -e $adjoint_file) source $adjoint_file

# if running batch job to process back data, specify time
if ( ${#argv} == 1 ) then
  set begin_time = $1
else
  set begin_time = `date +%Y%m%d%H%M%S`
endif

#
# Default values
# out_dir: output directory for adjoint_moist (ascii)
set range = "ATC"
set start_domain = 2

set ppi_dir = "/d1/projectData/multipleRadarIngest/mdv"
set wrf_dir = "/raid/fieldData/nexrad_level2/wrfout"
set radar_loc_file = "/raid/fieldData/nexrad_level2/Run/radar_HRRR_info"
set params_file = "/raid/fieldData/nexrad_level2/Run/adjoint_params.template"
set vcp_file = "/raid/fieldData/nexrad_level2/Run/vcp*"
set run_dir = "/raid/fieldData/nexrad_level2/Run"
set out_dir = "/raid/fieldData/nexrad_level2"
set adjoint_moist = "/home/fddasys/radar_src/RT_DECODE_NEXRAD_LEVEL2/RT_VDARS_QC/vdras_preproc_yue/adjoint_moist"
set perl_dir = "/home/fddasys/radar_src/RT_DECODE_NEXRAD_LEVEL2"

set _script_dir=`dirname $0`
set _script_name=`basename $0`
if ( "." == "$_script_dir" ) set _script_dir=`pwd`
set _start_time = `date +%s`
echo "  === $_script_name started at `date +%Y-%m-%d_%H:%M:%S`"

set log_exec_time = 0
set do_adjoint_moist = 1
set _job_id = "$range"
set rda_log_dir = $_script_dir
set perl_script_name = "crontab_3dvar_mergeData.pl"
#set radar_list = "AKQ CCX DIX DOX LWX"
set radar_list = "AKQ BGM BOX BUF CCX CLE DIX DOX ENX FCX ILN LWX MHX OKX PBZ RAX RLX"

##########################################################
# Overrides following local variables by using job script which ends with ".job.csh"
# - radar_list, ppi_dir, wrf_dir, radar_loc_file, params_file, vcp_file
#   run_dir, out_dir, adjoint_moist
#
set job_script = `echo $0 | sed 's/.csh/.job.csh/'`
if ( -e $job_script ) source $job_script $*

#echo "   =====  DEBUG out_dir: $out_dir"
#echo "   =====  DEBUG begin_time: $begin_time"
if ( ! -e $params_file) then
  echo "${_script_name}: ***** ERROR ***** param file [$params_file] does not exist!"
  exit -11
endif
if ( ! -e $radar_loc_file) then
  echo "${_script_name}: ***** ERROR ***** radar location data file [$radar_loc_file] does not exist!"
  exit -12
endif
set _vcp_count=`ls $vcp_file | wc -w`
if ( $_vcp_count == 0) then
  echo "${_script_name}: ***** ERROR ***** VCP files [$vcp_file] do not exist!"
  exit -13
endif
if ( ! -e $wrf_dir ) then
  echo "  ***** ERROR ***** wrf_dir [$wrf_dir] does not exist. Exit ...."
  exit -14
endif


set pid_name = "/tmp/tmp_`basename $0 ".csh"`_${_job_id}.pid"
if ( -e /dev/shm/$LOGNAME/rda ) set pid_name = "/dev/shm/$LOGNAME/rda/tmp_`basename $0 ".csh"`_${_job_id}.pid"
set is_running = 0
if( -e "$pid_name" ) then
  set running_pid = `cat $pid_name`
  set is_running = "`ps -ef | grep $_script_name | grep -v "grep" | grep $running_pid | wc -l`"
  #echo "running_pid: $running_pid, is_running: [$is_running]"
endif
if( $is_running > 0 ) then
  echo " ** INFO $_script_name for $_job_id is running at `date` ... **"
  echo " ** INFO $_script_name for $_job_id is running at `date` ... **" >> $rda_log_dir/${_script_name}.running
else
  echo $$ > $pid_name

  cd $run_dir
  set start_time = `echo $begin_time |cut -c 1-10`
  set start_time = "${start_time}0000"
  set yyyy = `echo $start_time |cut -c 1-4`
  set mm = `echo $start_time |cut -c 5-6`
  set dd = `echo $start_time |cut -c 7-8`
  set hh = `echo $start_time |cut -c 9-10`
  
  if ( $do_adjoint_moist != 0) then
   
    foreach radar ( $RADAR_LIST )
      set ctime = `date +%Y%m%d%H%M%S`
      if ( $start_time > $ctime ) continue
      
      # add radar to the list here 
      set radar1 = K$radar
      set radar_dir = $run_dir/$radar1
      echo "processing $radar1 for $start_time at $ctime, run directory is $radar_dir "
      if ( ! -e "$radar_dir") mkdir $radar_dir 
      set adjoint_out_dir = "$out_dir/adjoint/$radar1"
      if ( ! -e $adjoint_out_dir ) mkdir -p $adjoint_out_dir
    
      cd $radar_dir
      
      set adjoint_log_dir = "."
      if ( "$rda_log_dir" != "$_script_dir" ) set adjoint_log_dir = $rda_log_dir/$radar1
      if ( ! -e "$adjoint_log_dir" ) mkdir -p $adjoint_log_dir
      
      #Modify namelist
      if ( ! -e "$radar_dir/adjoint_params.$radar1") then
        echo "editing parameter file adjoint_params.$radar1"
        cp $params_file adjoint_params.$radar1
        cp $vcp_file .
        echo "getting radar location info from $radar_loc_file"
        if ( ! -e $radar_loc_file ) continue
        if ( `grep $radar1 $radar_loc_file` == '') continue
        
        set latlon = `grep $radar1 $radar_loc_file`
        if ( "$latlon" == '') continue
        set lat = `echo $latlon | cut -c 6-12 `
        set lon = `echo $latlon | cut -c 14-21 `
        set alt = `echo $latlon | cut -c 23-28 `
        echo "radar loc $lat , $lon, $alt"
    
        #echo "   output_url: $out_dir/adjoint/$radar1"
        #echo "  output_url1: $LittleR_out_dir"
        #echo "  output_url2: $radar1"
        #echo "   wrfout_url: $wrf_dir"
        #echo "      out_dir: $out_dir"
        #echo "  input_radarPpi_dir = $ppi_dir"
    
        ed ./adjoint_params.$radar1 << EOF > /dev/null
g?output_url =?s??output_url = "$adjoint_out_dir"?g
g?output_url1 =?s??output_url1 = "$LittleR_out_dir"?g
g?output_url2 =?s??output_url2 = "$radar1"?g
g?wrfout_url =?s??wrfout_url ="$wrf_dir/wrfout_d0${start_domain}_"?g
g?input_radarPpi_dir?s??"$ppi_dir/radarPpi$radar1"?g
g/rlatd =/s//rlatd = $lat/g
g/rlond =/s//rlond = $lon/g
g/raltd =/s//raltd = $alt/g
w
q 
EOF
        echo "${_script_name} adjoint_params.$radar1 created"
      endif
    
      echo " update the time in parameter file"
      cp adjoint_params.$radar1 adjoint_params
      ed ./adjoint_params << EOF > /dev/null
g/yyyy =/s//yyyy = $yyyy/g
g/mm =/s//mm = $mm/g
g/dd =/s//dd = $dd/g
g/hh =/s//hh = $hh/g
g/mn =/s//mn = 00/g
g/ss =/s//ss = 00/g
w
q
EOF
     
      echo " done with editing adjoint_params"
      echo " launch adjoint run for $start_time at $ctime"
      \rm -f start_time
      # $adjoint_moist -params adjoint_params >& log.$radar1.$start_time &
      echo " $adjoint_moist -params $run_dir/$radar1/adjoint_params"
      $adjoint_moist -params $radar_dir/adjoint_params >& $adjoint_log_dir/log.$radar1.$start_time
      set exit_code = $?
      if ( $exit_code != 0 ) then
         echo "    === ERROR ===  There is an error. Please look at $adjoint_log_dir/log.$radar1.$start_time"
      endif
      # end all radar
    end 
  
  endif     # if ( $do_adjoint_moist != 0)
  
  # cat data together
  @ domain_id = $start_domain
  $perl_dir/$perl_script_name $range $domain_id $start_time
  @ domain_id++
  $perl_dir/$perl_script_name $range $domain_id $start_time

  if ( -e "$pid_name" ) rm $pid_name
endif

@ duration = `date +%s` - $_start_time 
if ( $?log_exec_time && $log_exec_time ) then
  echo "${_script_name}: finished at `date +%Y-%m-%d_%H:%M:%S`, duration=$duration sec" >> $rda_log_dir/${_script_name}.time.log
endif
echo "  === $_script_name ended at `date +%Y-%m-%d_%H:%M:%S`"
