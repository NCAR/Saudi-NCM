#!/bin/csh
#
# LSF batch script to run wrf.exe MPI code
#
#BSUB -n 1                              # number of total (MPI) tasks
#BSUB -J analog                         # job name
#BSUB -o analog.out                     # output filename
#BSUB -e analog.err                     # error filename
#BSUB -W 00:30                          # wallclock time
#BSUB -P projefct_id_here 
#BSUB -q regular 

set __script_dir  = `dirname $0`
set __script_name = `basename $0`
if ( $__script_dir == "." ) set __script_dir = `pwd`

#if ( ! -d log ) mkdir log

set start_time = `date +%s`
set test_mode = 0
set all_cycles = 0
set ranges = ""
set sub_opts = ""
set init_hour = ""

if ( $#argv > 0 ) then
  foreach arg ( $* )
    #echo "  arg: $arg"
    #set arg_matched = 1
    if ( "$arg" == "test_mode" || "$arg" == "_test_mode" \
         || "$arg" == "-test_mode" || "$arg" == "testmode" ) then
      set test_mode = 1
    else if ( "$arg" == "atc" || "$arg" == "crtc" || "$arg" == "dpg" \
           || "$arg" == "epg" || "$arg" == "nvl" || "$arg" == "rtc"  \
           || "$arg" == "wsmr"  || "$arg" == "ypg") then
      set ranges = "$ranges $arg"
      set range = "$arg"
    else if ( "$arg" == "all" ) then
      set all_cycles = 1
      set sub_opts = "all"
    else
      set init_hour = $arg
    endif
  end
endif
#echo " test_mode: $test_mode"
#echo " init_hour: [$init_hour]"

if ( "$init_hour" == "" ) then
  echo "   === WARN === init_hour is missing"
  echo "                Exit ..."
  exit -1
endif

#set obs_vars = "temp pres wspd wdir"
set obs_vars = "temp wspd wdir"
set obs_vars = "temp wspd"
#set cur_hour = `date +%H | bc`

if ( $test_mode == 1 ) then
  set ranges = crtc
  set obs_vars = wspd
  #set obs_vars = temp
endif

#echo "   ranges: [$ranges] all_cycles: $all_cycles"
if ($?GSJOBDIR) then
  #echo "     ${__script_name} GSJOBDIR: $GSJOBDIR"
  if ( -e $GSJOBDIR/scripts/env_vars.csh ) source $GSJOBDIR/scripts/env_vars.csh
endif

set my_exe = main_analog
if ( ! -e $my_exe && ! -l $my_exe ) then
  if ($?EXECUTABLE_ARCHIVE) then
    set my_exe = $EXECUTABLE_ARCHIVE/main_analog
  else if ($?MM5HOME) then
    set my_exe = $MM5HOME/cycle_code/EXECUTABLE_ARCHIVE/main_analog
  endif
endif
#echo "     ${__script_name}    my_exe: $my_exe"

#foreach range ( $ranges )
  echo  "   ** Processing $range ($init_hour) at `pwd` ..."
  set base_datainput_dir = /datainput
  if ($?DATA_DIR) then
    set base_datainput_dir = $DATA_DIR
  endif
  set include_user_in_path = 0
  foreach sub_dir ( $base_datainput_dir:as|/| | )
    if ( $sub_dir == $LOGNAME ) then
      set include_user_in_path = 1
      break;
    endif
  end
  if ($include_user_in_path == 0) then
    if ( -d /model/$LOGNAME/datainput )   set base_datainput_dir = /model/$LOGNAME/datainput
    if ( -d /p/work1/$LOGNAME/datainput ) set base_datainput_dir = /p/work1/$LOGNAME/datainput
    if ( -d /p/work2/$LOGNAME/datainput ) set base_datainput_dir = /p/work2/$LOGNAME/datainput
  endif
  
  set target_dir = $base_datainput_dir/$range/veri_pair.anen
  if ( ! -d $target_dir ) mkdir -p $target_dir

  set template_dir = .
  if ($?GSJOBDIR) then
    if ( -e $GSJOBDIR/veri_pair ) set template_dir = $GSJOBDIR/veri_pair
  endif
  if ( ! -d input ) mkdir input
  if ( ! -e input/$range ) then
    set datainput_dir = $base_datainput_dir/$range/veri_pair
    if ( -e $datainput_dir ) ln -sf $datainput_dir input/$range
  endif
  if ( ! -d output/$range ) mkdir -p output/$range

  if ( $all_cycles == 1 ) then
    if ( "$init_hour" == "00" || "$init_hour" == "0" ) then
      set init_hours = "00 18 12 06"
    else if ( "$init_hour" == "02" || "$init_hour" == "2" ) then
      set init_hours = "02 23 20 17 14 11 08 05"
    else if ( "$init_hour" == "05" || "$init_hour" == "5" ) then
      set init_hours = "05 02 23 20 17 14 11 08"
    else if ( "$init_hour" == "06" || "$init_hour" == "6" ) then
      set init_hours = "06 00 18 12"
    else if ( "$init_hour" == "08" || "$init_hour" == "8" ) then
      set init_hours = "08 05 02 23 20 17 14 11"
    else if ( "$init_hour" == "11" ) then
      set init_hours = "11 08 05 02 23 20 17 14"
    else if ( "$init_hour" == "12" ) then
      set init_hours = "12 06 00 18"
    else if ( "$init_hour" == "14" ) then
      set init_hours = "14 11 08 05 02 23 20 17"
    else if ( "$init_hour" == "17" ) then
      set init_hours = "17 14 11 08 05 02 23 20"
    else if ( "$init_hour" == "18" ) then
      set init_hours = "18 12 06 00"
    else if ( "$init_hour" == "20" ) then
      set init_hours = "20 17 14 11 08 05 02 23"
    #else if ( "$init_hour" == "23" ) then
    else
      set init_hours = "23 20 17 14 11 08 05 02"
    endif
  else
    set init_hours = "$init_hour"
  endif 
  #echo "   sub_opts: [$sub_opts], init_hours: [$init_hours]"
  
  foreach init_hour ( $init_hours )
    echo "  2. init_hour: $init_hour from [$init_hours]"
    
    foreach obs_var ( $obs_vars )
      echo  "      - Processing $obs_var for $init_hour ..."
      #set param_name = "analog_ini.txt"
      set param_name = "analog_input.nml"
      set param_template_name = "${param_name}.template"
      set template_param_name = $template_dir/${param_template_name}.${obs_var}
      if ( -e $template_dir/${param_template_name}.${obs_var}.${range}) then
        set template_param_name = $template_dir/${param_template_name}.${obs_var}.${range}
      endif
  
      echo  "        template name : $template_param_name"
      cat $template_param_name | sed -e 's/__INIT_HOUR__/'$init_hour'/g' -e 's/__range__/'$range'/g' -e 's/__user__/'$LOGNAME'/g' > $param_name
      set t_out_file = `cat $param_name | grep out_file_netcdf | cut -f1 -d"!" | cut -f2 -d"=" | sed "s|'||g"`
      echo  "             out_file : [$t_out_file]"
      set t_out_dir = `dirname $t_out_file`
      if ( ! -e $t_out_dir ) mkdir -p $t_out_dir

      #pwd;       cat $param_name
      set log_name = analog.$range.$obs_var.$init_hour.log
      if ( -d log ) set log_name = log/$log_name
      #echo "      MM5HOME: $MM5HOME, EXECUTABLE_ARCHIVE: $EXECUTABLE_ARCHIVE"
      echo "        $my_exe > $log_name"
      $my_exe > $log_name
      if ( -e $t_out_file ) then
        echo  "        Moving $t_out_file to $target_dir/ ..."
        mv $t_out_file $target_dir/
      else
        echo "   === ERROR === $t_out_file does not exist "
      endif
    end
    echo ""
  end
  echo ""
#end

set end_time = `date +%s`
@ duration = $end_time - $start_time
echo "   Took $duration seconds for `basename $0`"

