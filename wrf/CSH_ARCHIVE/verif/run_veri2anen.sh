#!/bin/bash

_MY_SCRIPT=`basename $0`
_MY_SCRIPT_dir=`dirname $0`
_OPT_DEBUG=0
_OPT_IS_OBS=0
_OPT_IGNORE_SIZE=1
_OPT_LEAD_HOURS=24
echo "      Usage: $_MY_SCRIPT cycle_time_yyyymmddhh python_path range <obs> <lead_hours=hh>"

[ "" != "$this_cycle" ] && MY_CYCLE=$this_cycle
if [ $# -gt 0 ]; then
  MY_CYCLE=$1
  shift
fi

cshrc_file=$GSJOBDIR/tmp/$MY_CYCLE/cshrc
if [ -e $cshrc_file ]; then
  export RUNDIR=`cat $cshrc_file | grep " RUNDIR " | cut -f3 -d" "`
  #echo "RUNDIR: $RUNDIR"
fi

[ "" != "$RANGE" ]          && MY_RANGE=`echo $RANGE | tr "[:upper:]" "[:lower:]"`
[ "" != "$PYTHON_ARCHIVE" ] && MY_PYTHON_PATH="$PYTHON_ARCHIVE/flex/veri_pair"

if [ $# -gt 0 ]; then
  for arg in $*
  do
    #echo "  arg: $arg"
    if   [ $arg == "atc" -o $arg == "crtc" -o $arg == "rtc" \
        -o $arg == "dpg" -o $arg == "epg" -o $arg == "ypg" \
        -o $arg == "wsmr" -o $arg == "nvl" ]; then
      MY_RANGE=$arg
    elif [ $arg == "obs" -o $arg == "OBS" ]; then
      _OPT_IS_OBS=1
    elif [ $arg == "debug" -o $arg == "DEBUG" ]; then
      _OPT_DEBUG=1
    elif [ -d $arg -o -L $arg ]; then
      MY_PYTHON_PATH=$arg
    else
      key=`echo $arg | cut -f1 -d=`
      value=`echo $arg | cut -f2 -d=`
      if [ "$key" == "lead_hours" ]; then
        _OPT_LEAD_HOURS=$value
      fi
    fi
  done
fi
if [ "" == "$MY_CYCLE" ]; then
  echo "  === ERROR === $_MY_SCRIPT the arguments missing, cycle_ymdh [range] [python_path]"
  exit 1
fi
[ $_OPT_DEBUG -eq 1 ] && echo "  MY_CYCLE: $MY_CYCLE, MY_RANGE: $MY_RANGE, PYTHON_PATH: $MY_PYTHON_PATH"

CYCLE_HOUR=`echo $MY_CYCLE | cut -c9-10`
DO_FORCE_MOVE_FILES=0
[ "" == "$RUNDIR" ] && RUNDIR="."
[ "" == "$GSJOBDIR" ] && GSJOBDIR="."
VERI_PAIR_FORECAST_DIR=$RUNDIR/sams_sites/fcst


function get_file_size {
  fi_file=$1
  fo_file_size=0
  [ -e $fi_file ] && fo_file_size="`stat --printf="%s" $fi_file`"
  echo $fo_file_size
}
  
function get_model_id {
  fo_model_id=0
  
  ft_gsjobid=$GSJOBID
  [ "" == "$ft_gsjobid" ] && ft_gsjobid=`basename $GSJOBDIR`
  if [ "$ft_gsjobid" == "GWATC2" ]; then
    fo_model_id=3
  elif [ "$PBS_JOBID" != "" ]; then
    ft_is_wrf2=`echo $PBS_JOBID | grep bigmacb | wc -w`
    [ $ft_is_wrf2 -ne 0 ] && fo_model_id=3
  else
    if [ "$REMOTEHOST" != "" ]; then
      ft_hostname=`echo $REMOTEHOST | cut -f1 -d"-"`
      [ "bigmacb" == "$ft_hostname" ] && fo_model_id=3
    #else
    #  ft_hostname=`hostname -s | cut -f1 -d"-"`
    fi
  fi
  echo $fo_model_id
}

function get_previous_init_hour {
  fi_cur_init_hour=$1
  fo_previous_init_hour=$fi_cur_init_hour
  
  if [ "$fi_cur_init_hour" == "00" ]; then
    fo_previous_init_hour="18"
  elif [ "$fi_cur_init_hour" == "02" -o "$fi_cur_init_hour" == "2" ]; then
    fo_previous_init_hour="23"
  elif [ "$fi_cur_init_hour" == "05" -o "$fi_cur_init_hour" == "5" ]; then
    fo_previous_init_hour="02"
  elif [ "$fi_cur_init_hour" == "06" -o "$fi_cur_init_hour" == "6" ]; then
    fo_previous_init_hour="00"
  elif [ "$fi_cur_init_hour" == "08" -o "$fi_cur_init_hour" == "8" ]; then
    fo_previous_init_hour="05"
  elif [ "$fi_cur_init_hour" == "11" ]; then
    fo_previous_init_hour="08"
  elif [ "$fi_cur_init_hour" == "12" ]; then
    fo_previous_init_hour="06"
  elif [ "$fi_cur_init_hour" == "14" ]; then
    fo_previous_init_hour="11"
  elif [ "$fi_cur_init_hour" == "17" ]; then
    fo_previous_init_hour="14"
  elif [ "$fi_cur_init_hour" == "18" ]; then
    fo_previous_init_hour="12"
  elif [ "$fi_cur_init_hour" == "20" ]; then
    fo_previous_init_hour="17"
  elif [ "$fi_cur_init_hour" == "23" ]; then
    fo_previous_init_hour="20"
  fi

  echo $fo_previous_init_hour
}

function get_previous_init_hours {
  fi_cur_init_hour=$1
  fo_previous_init_hours=$fi_cur_init_hour
  
  if [ "$fi_cur_init_hour" == "00" ]; then
    fo_previous_init_hours="18 12 06"
  elif [ "$fi_cur_init_hour" == "02" -o "$fi_cur_init_hour" == "2" ]; then
    fo_previous_init_hours="23 20 17 14 11 08 05"
  elif [ "$fi_cur_init_hour" == "05" -o "$fi_cur_init_hour" == "5" ]; then
    fo_previous_init_hours="02 23 20 17 14 11 08"
  elif [ "$fi_cur_init_hour" == "06" -o "$fi_cur_init_hour" == "6" ]; then
    fo_previous_init_hours="00 18 12"
  elif [ "$fi_cur_init_hour" == "08" -o "$fi_cur_init_hour" == "8" ]; then
    fo_previous_init_hours="05 02 23 20 17 14 11"
  elif [ "$fi_cur_init_hour" == "11" ]; then
    fo_previous_init_hours="08 05 02 23 20 17 14"
  elif [ "$fi_cur_init_hour" == "12" ]; then
    fo_previous_init_hours="06 00 18"
  elif [ "$fi_cur_init_hour" == "14" ]; then
    fo_previous_init_hours="11 08 05 02 23 20 17"
  elif [ "$fi_cur_init_hour" == "17" ]; then
    fo_previous_init_hours="14 11 08 05 02 23 20"
  elif [ "$fi_cur_init_hour" == "18" ]; then
    fo_previous_init_hours="12 06 00"
  elif [ "$fi_cur_init_hour" == "20" ]; then
    fo_previous_init_hours="17 14 11 08 05 02 23"
  elif [ "$fi_cur_init_hour" == "23" ]; then
    fo_previous_init_hours="20 17 14 11 08 05 02"
  fi

  echo $fo_previous_init_hours
}

function format_duration {
  fi_duration=$1
  fo_formatted_duration="$fi_duration"
  
  if [ $fi_duration -ge 60 ]; then
    ft_minutes=$(( fi_duration / 60 ))
    ft_seconds=$(( fi_duration % 60 ))
    [ $ft_seconds -lt 10 ] && ft_seconds="0$ft_seconds"
    if [ $ft_minutes -ge 60 ]; then
      ft_hours=$(( ft_minutes / 60 ))
      ft_minutes=$(( ft_minutes % 60 ))
      [ $ft_minutes -lt 10 ] && ft_minutes="0$ft_minutes"
      fo_formatted_duration="$ft_hours:$ft_minutes:$ft_seconds"
    else
      fo_formatted_duration="$ft_minutes:$ft_seconds"
    fi
  fi
  echo "$fo_formatted_duration"
}

function move_cdf_files {
  fi_range=$1
  fi_datatype=$2
  fi_work_dir=$3
  fi_output_dir=$4
  fi_error_log_file=$5
  fi_hour_pattern=$6
  
  ft_func_name="     move_cdf_files()"
  if [ "" != "$fi_hour_pattern" ]; then
    ft_hour_pattern="${fi_hour_pattern}"
  else
    ft_hour_pattern="*"
  fi
  [ ! -d "$fi_output_dir" ] && mkdir -p $fi_output_dir
  
  #echo "$ft_func_name  work_dir: [$fi_work_dir], output_dir: [$fi_output_dir]" >&2
  #echo "$ft_func_name  ls -rt $fi_work_dir/$fi_range.${fi_datatype}*${ft_hour_pattern}.*.cdf" >&2
  #ft_nc_files=`ls -rt $fi_work_dir/$fi_range.${fi_datatype}*.cdf`
  ft_sub_name_pattern="${fi_datatype}*${ft_hour_pattern}.*.cdf"
  ft_nc_files=`ls -rt $fi_work_dir/${fi_range}.${ft_sub_name_pattern} 2> /dev/null | sort `
  if [ "${fi_range}" == "rtc" ]; then
    ft_nc_files="$ft_nc_files `ls -rt $fi_work_dir/rttc.${ft_sub_name_pattern} 2> /dev/null | sort `"
  fi
  for ft_nc_file in $ft_nc_files
  do 
    if [ -e $ft_nc_file ]; then 
      ft_nc_file_name=`basename $ft_nc_file`
      ft_recent_file=`echo $ft_nc_file_name | grep recent | wc -w`
      ft_yyyymmddhh=`echo $ft_nc_file_name | cut -f2 -d"." | sed 's/'$fi_datatype'//g'`
      ft_yymmdd=`echo $ft_yyyymmddhh | cut -c3-8`
      ft_hh=`echo $ft_yyyymmddhh | cut -c9-10`
      
      ft_move_file=0
      ft_create_soft_link=0
      ft_target_name=$fi_output_dir/${fi_datatype}.${ft_yymmdd}.${ft_hh}0000.cdf
      [ $ft_recent_file -eq 1 ] && ft_target_name=$fi_output_dir/recent.veri_pair.${ft_hh}.cdf
      if [ $_OPT_IGNORE_SIZE -eq 1 ]; then
        ft_move_file=1
      else
        ft_old_file_size=`get_file_size $ft_target_name`
        ft_new_file_size=`get_file_size $ft_nc_file`

        if [ $ft_recent_file -eq 1 ]; then
          ft_move_file=1
        else
          if [ $ft_new_file_size -ge $ft_old_file_size ]; then
            ft_move_file=1
          fi
        fi
      fi
      [ $ft_recent_file -eq 0 -a $ft_move_file -eq 1 ] && ft_create_soft_link=1

      if [ $ft_move_file -eq 1 ]; then
        echo "      - Moving $ft_nc_file to $ft_target_name"
        echo "      - recent_file: $ft_recent_file, create_soft_link: $ft_create_soft_link"
        mv $ft_nc_file $ft_target_name
        chmod g+rw $ft_target_name
        if [ $ft_create_soft_link -eq 1 ]; then
          ft_latest_link=$fi_output_dir/latest.veri_pair.${ft_hh}.cdf
          [ -e $ft_latest_link ] && rm $ft_latest_link
          echo "      - Creating soft link $ft_target_name to $ft_latest_link"
          ln -sf $ft_target_name $ft_latest_link
        fi
      else
        file_ext=`date +"%m-%d_%H%M%S"`
        ft_date_str=`date +"%Y-%m-%d %T"`
        echo "   === WARN === Renaming $ft_nc_file to ${ft_nc_file}.${file_ext}" >> $fi_error_log_file
        echo "   === WARN === Because the file size was shrunk ($ft_old_file_size to $ft_new_file_size) at $ft_date_str" >> $fi_error_log_file
        mv $ft_nc_file ${ft_nc_file}.${file_ext}
      fi
    fi
  done
}

DATA_TYPE="veri"
BASE_OUTPUT_DIR=/datainput
[ "" != "$DATA_DIR" ] && BASE_OUTPUT_DIR=$DATA_DIR
IS_USER_DEPENDENT_DIR=0
for sub_dir in `echo $BASE_OUTPUT_DIR | sed -e 's|/| |g'`
do
  if [ $sub_dir == $LOGNAME ]; then
    IS_USER_DEPENDENT_DIR=1
    break
  fi
done
if [ $IS_USER_DEPENDENT_DIR -eq 0 ]; then
  [ -d /model/$LOGNAME/datainput ]   && BASE_OUTPUT_DIR=/model/$LOGNAME/datainput
  [ -d /p/work1/$LOGNAME/datainput ] && BASE_OUTPUT_DIR=/p/work1/$LOGNAME/datainput
  [ -d /p/work2/$LOGNAME/datainput ] && BASE_OUTPUT_DIR=/p/work2/$LOGNAME/datainput
fi

start_time=`date +%s`
anen_duration=0
dump_duration=0
date_str=`date +"%Y-%m-%d %H:%M:%S"`
echo "   === Started $_MY_SCRIPT at $date_str"

_MODEL_ID=`get_model_id`
#echo "   --- MMM  GSJOBDIR: $GSJOBDIR, GSJOBID: $GSJOBID, PBS_JOBID: $PBS_JOBID, REMOTEHOST: $REMOTEHOST, _MODEL_ID: $_MODEL_ID"

if [ "" == "$PYTHONPATH" ]; then
  echo "  === ERROR === The environment variable PYTHONPATH is not defined"
else
  # Make veri2nc.cfg from veri2nc.cfg.template by replacing some varibales 
  cfg_name=veri2nc.cfg
  cfg_file=$GSJOBDIR/veri_pair/$cfg_name
#  cat ${cfg_file}.template | sed -e 's|^work_dir=.*__RUNDIR__|work_dir='$RUNDIR'|g' \
#      -e 's|__GSJOBDIR__|'$GSJOBDIR'|g' -e 's|__RANGE__|'$MY_RANGE'|g' -e 's|site=rtc|site=rttc|g' > $cfg_name
  cat ${cfg_file}.template | sed -e 's|^work_dir=.*__RUNDIR__|work_dir='$RUNDIR'|g' \
      -e 's|__GSJOBDIR__|'$GSJOBDIR'|g' -e 's|__RANGE__|'$MY_RANGE'|g' \
      -e 's|site=rtc|site=rttc|g' > $cfg_name
  log_dir=` grep "^log_dir" $cfg_name | cut -f2 -d"="`
  work_dir=`grep "^work_dir" $cfg_name | cut -f2 -d"="`
  [ ! -d $log_dir ] && mkdir -p $log_dir
  [ ! -d $work_dir ] && mkdir -p $work_dir
  
  dump_start_time=`date +%s`
  # Make sure the log directory is writable.
  my_log_dir=${GSJOBDIR}/logs/$MY_CYCLE
  if [ ! -w $my_log_dir ]; then
    my_log_dir=$log_dir
    [ ! -d $my_log_dir ] && mkdir -p $my_log_dir
  fi
  my_log_name=${my_log_dir}/veri2nc.log
  #[ $_OPT_IS_OBS -eq 1 ] && my_log_name=${my_log_dir}/veri2nc.obs.log
  my_obs_log_name=${my_log_dir}/veri2nc.obs.log

  #[ -e $my_log_name ] && rm $my_log_name
  echo "      working directory for `basename $0`: `pwd`"
  #
  # Adjust the cycle hour for verification pairs. Move the init_hour to
  # the previous cycle if the verification pair for the current cycle is not
  # available yet.
  w_cycle_hour=$CYCLE_HOUR
  if [ -d $VERI_PAIR_FORECAST_DIR ]; then
    veri_pair_forecast_file=$VERI_PAIR_FORECAST_DIR/${MY_CYCLE}_wrfVars_samsSites_P+FCST
    if [ -e "$veri_pair_forecast_file" ]; then
      echo "      $_MY_SCRIPT $veri_pair_forecast_file exists"
    else
      #w_cycle_hour=`get_previous_init_hour $CYCLE_HOUR`
      echo "      $_MY_SCRIPT $veri_pair_forecast_file does not exist. "
    fi
  fi
  PREV_CYCLE_HOURS=""
  if [ $_OPT_IS_OBS -eq 1 ]; then
    PREV_CYCLE_HOURS=`get_previous_init_hours $w_cycle_hour`
    for t_cycle_hour in $w_cycle_hour $PREV_CYCLE_HOURS
    do
      my_cmd="python $MY_PYTHON_PATH/veri2nc.py -c $cfg_name --model=$_MODEL_ID --cycle_hour=$t_cycle_hour -m 5 --lead-hours=$_OPT_LEAD_HOURS"
      echo "      $my_cmd"
      $my_cmd >> $my_obs_log_name 2>&1
    done
  else
    my_cmd="python $MY_PYTHON_PATH/veri2nc.py -c $cfg_name --model=$_MODEL_ID --cycle_hour=$w_cycle_hour -m 5 --lead-hours=$_OPT_LEAD_HOURS"
    echo "      $my_cmd"
    $my_cmd >> $my_log_name 2>&1
  fi
  #echo "    CYCLE_HOUR: $CYCLE_HOUR, init hours: [$w_cycle_hour $PREV_CYCLE_HOURS]"
  my_cmd="python $MY_PYTHON_PATH/veri2nc.py -c $cfg_name --model=$_MODEL_ID --cycle_hour=$w_cycle_hour --lead-hours=$_OPT_LEAD_HOURS"
  echo "      $my_cmd"
  $my_cmd >> $my_log_name 2>&1
  dump_end_time=`date +%s`
  dump_duration=$(( dump_end_time - dump_start_time ))
  
  error_log_file=$log_dir/${DATA_TYPE}2nc.err
  out_dir=$BASE_OUTPUT_DIR/${MY_RANGE}/veri_pair
  #echo " work_dir: $work_dir, out_dir: $out_dir"
  for t_cycle_hour in $w_cycle_hour $PREV_CYCLE_HOURS
  do
    move_cdf_files "$MY_RANGE" "$DATA_TYPE" "$work_dir" "$out_dir" "$error_log_file" "$t_cycle_hour"
  done

  anen_start_time=`date +%s`
  #duration=$((end_time - start_time))
  return_dir=`pwd`
  if [ -f input -o -f output ]; then
    if [ -d "../verify" ]; then
      cd ../verify
    else
      cd $work_dir
    fi
  fi
  echo "  * AnEn processing ($_MY_SCRIPT_dir/runreal.csh ${MY_RANGE} $w_cycle_hour)..."
  $_MY_SCRIPT_dir/runreal.csh ${MY_RANGE} $w_cycle_hour
  [ "" != "$return_dir" ] && cd $return_dir
  
  #$_MY_SCRIPT_dir/runreal.csh ${MY_RANGE} all
  
  anen_end_time=`date +%s`
  anen_duration=$(( anen_end_time - anen_start_time ))
  echo ""
  echo "    - took $anen_duration seconds [`format_duration $anen_duration`] for AnEn $MY_RANGE"

  range_end_time=`date +%s`
  range_duration=$(( range_end_time - range_start_time ))
  echo ""
  echo ""
fi


end_time=`date +%s`
duration=$(( end_time - start_time ))
date_str=`date +"%Y-%m-%d %H:%M:%S"`
echo "    - took dump: $dump_duration [`format_duration $dump_duration`] and anen: $anen_duration [`format_duration $anen_duration`] for $MY_RANGE"
echo "   === Done `basename $0` === took $duration seconds [`format_duration $duration`] at $date_str"
echo ""
