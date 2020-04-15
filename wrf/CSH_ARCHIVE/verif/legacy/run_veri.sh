#!/bin/sh
_script_dir=`dirname $0`
_script_name=`basename $0`
[ "." == "$_script_dir" ] && _script_dir=`pwd`
#echo "  script_dir: $_script_dir"

_HAS_PYTHONPATH=0
if [ "" != "$PYTHONPATH" ]; then
  _HAS_PYTHONPATH=`echo $PYTHONPATH | grep "python" | wc -w`
fi
if [ 0 -eq $_HAS_PYTHONPATH ]; then
  PYTHONPATH=$HOME/libs/python
  [ ! -e $PYTHONPATH ] && PYTHONPATH=$HOME/lib_python
  [ ! -e $PYTHONPATH ] && PYTHONPATH=$HOME/python_lib
  if [ -e $PYTHONPATH ]; then
    export PYTHONPATH
  else
    echo " == ERROR == Can not set PYTHONPATH"
    echo "             Exit ..."
    exit -1
  fi
fi

if [ "" == "$NCARG_ROOT" ]; then
  if [ -d "/opt/ncl" ]; then
    export NCARG_ROOT=/opt/ncl
  else
    echo "  === ERROR === Environment variable NCARG_ROOT is not defined"
    echo "  Exiting..."
    exit -1
  fi
fi

sub_script=$HOME/bin/sub_mysql.sh
[ ! -e $sub_script ] && sub_script=$HOME/datbin/sub_mysql.sh
if [ -e $sub_script ]; then
  source $sub_script
else
  echo "  === ERROR === subscript [$sub_script] dos not exist"
  echo "  Exiting..."
  exit -1
fi

_MODEL_ID=0
_my_hostname=`hostname -s | cut -f1 -d"-"`
if [ "$LOCALHOST" != "" ]; then
  tmp_hostname=`echo $LOCALHOST | cut -f1 -d"-"`
  [ "bigmacb" == "$tmp_hostname" -o "$GSJOBID" == "GWATC2" ] && _MODEL_ID=3
else
  [ "bigmacb" == "$_my_hostname" -o "$GSJOBID" == "GWATC2" ] && _MODEL_ID=3
fi

DO_FORCE_MOVE_FILES=0
DO_RECENT_FIRST=1
#target_ranges="atc crtc"
#target_ranges="crtc"
target_ranges="atc crtc dpg epg rtc wsmr ypg"

ranges=""
do_default_jobs=1
do_obs_data=0
do_model_data=0
sub_opts=""
if [ $# -gt 0 ]; then
  for arg in $*
  do
    if [ "$arg" == "force_move" -o "$arg" == "move" ]; then
      DO_FORCE_MOVE_FILES=1
    #elif   [ "$arg" == "obs" ]; then
    #  do_obs_data=1
    #  do_default_jobs=0
    #  sub_opts="obs"
    #elif [ "$arg" == "model" ]; then
    #  do_model_data=1
    #  do_default_jobs=0
    #  sub_opts="model"
    #elif [ "$arg" == "all" ]; then
    #  do_obs_data=1
    #  do_model_data=1
    #  do_default_jobs=0
    elif [ "$arg" == "atc" -o "$arg" == "crtc" -o "$arg" == "dpg" \
        -o "$arg" == "epg" -o "$arg" == "nvl" -o "$arg" == "rtc" \
        -o "$arg" == "wsmr" -o "$arg" == "ypg" ]; then
      ranges="$ranges $arg"
    fi
  done
fi

#if [ $do_default_jobs -eq 1 ]; then
#  do_obs_data=1
#  do_model_data=1
#fi

#
#  Global variables
#
cur_hour=`date +%H | bc`

# =====================================
#  Functions
#
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

function get_file_size {
  fi_filename=$1
  fo_file_size=0
  if [ -e "$fi_filename" ]; then
    fo_file_size=`stat $fi_filename | grep "Size: " | cut -f1 | cut -f2 -d":" | sed -e 's/^ *//' -e 's/ *$//'`
  fi
  [ "$fo_file_size" == "" ] && fo_file_size=0
  echo "$fo_file_size"
}

function get_cur_cycle_hours {
  fi_target_range=$1
  fo_cycle_hours=""
  
  ft_func_name="     get_cur_cycle_hours"
  #echo "$ft_func_name is called" >&2
  if [ "crtc" == "$fi_target_range" ]; then
    if   [ $cur_hour -eq  0 ]; then
      fo_cycle_hours="18 0"
    elif [ $cur_hour -lt  6 ]; then
      fo_cycle_hours="0"
    elif [ $cur_hour -eq  6 ]; then
      fo_cycle_hours="0 6"
    elif [ $cur_hour -lt 12 ]; then
      fo_cycle_hours="6"
    elif [ $cur_hour -eq 12 ]; then
      fo_cycle_hours="6 12"
    elif [ $cur_hour -lt 18 ]; then
      fo_cycle_hours="12"
    elif [ $cur_hour -eq 18 ]; then
      fo_cycle_hours="12 18"
    else
      fo_cycle_hours="18"
    fi
  else
    if   [ $cur_hour -lt 2 ]; then
      fo_cycle_hours="23"
    elif [ $cur_hour -eq 2 ]; then
      fo_cycle_hours="23 2"
    elif [ $cur_hour -lt 5 ]; then
      fo_cycle_hours="2"
    elif [ $cur_hour -eq 5 ]; then
      fo_cycle_hours="2 5"
    elif [ $cur_hour -lt 8 ]; then
      fo_cycle_hours="5"
    elif [ $cur_hour -eq 8 ]; then
      fo_cycle_hours="5 8"
    elif [ $cur_hour -lt 11 ]; then
      fo_cycle_hours="8"
    elif [ $cur_hour -eq 11 ]; then
      fo_cycle_hours="8 11"
    elif [ $cur_hour -lt 14 ]; then
      fo_cycle_hours="11"
    elif [ $cur_hour -eq 14 ]; then
      fo_cycle_hours="11 14"
    elif [ $cur_hour -lt 17 ]; then
      fo_cycle_hours="14"
    elif [ $cur_hour -eq 17 ]; then
      fo_cycle_hours="14 17"
    elif [ $cur_hour -lt 20 ]; then
      fo_cycle_hours="17"
    elif [ $cur_hour -eq 20 ]; then
      fo_cycle_hours="17 20"
    elif [ $cur_hour -lt 23 ]; then
      fo_cycle_hours="20"
    else
      fo_cycle_hours="20 23"
    fi
  fi
  #echo "$ft_func_name  cycle_hours: [$fo_cycle_hours]" >&2
  echo "$fo_cycle_hours"
}

function get_cycle_hours {
  fi_target_range=$1
  fo_cycle_hours=""

  ft_func_name="     get_cur_cycle_hours"
  #echo "$ft_func_name is called" >&2
  if [ "crtc" == "$fi_target_range" ]; then
    if   [ $cur_hour -lt  6 ]; then
      fo_cycle_hours="6 12 18 0"
    elif [ $cur_hour -lt 12 ]; then
      fo_cycle_hours="12 18 0 6"
    elif [ $cur_hour -lt 18 ]; then
      fo_cycle_hours="18 0 6 12"
    else
      fo_cycle_hours="0 6 12 18"
    fi
  else
    if [ $DO_RECENT_FIRST -eq 1 ]; then
      if   [ $cur_hour -lt 2 ]; then
        fo_cycle_hours="23 20 17 14 2 11 8 5"
      elif [ $cur_hour -lt 5 ]; then
        fo_cycle_hours="2 23 20 17 14 11 8 5"
      elif [ $cur_hour -lt 8 ]; then
        fo_cycle_hours="8 11 14 17 20 23 2 5"
      elif [ $cur_hour -lt 11 ]; then
        fo_cycle_hours="11 14 17 20 23 2 5 8"
      elif [ $cur_hour -lt 14 ]; then
        fo_cycle_hours="14 17 20 23 2 5 8 11"
      elif [ $cur_hour -lt 17 ]; then
        fo_cycle_hours="17 20 23 2 5 8 11 14"
      elif [ $cur_hour -lt 20 ]; then
        fo_cycle_hours="17 14 11 8 5 2 23 20"
      elif [ $cur_hour -lt 23 ]; then
        fo_cycle_hours="23 2 5 8 11 14 17 20"
      else
        fo_cycle_hours="2 5 8 11 14 17 20 23"
      fi
    else
      if   [ $cur_hour -lt 2 ]; then
        fo_cycle_hours="2 5 8 11 14 17 20 23"
      elif [ $cur_hour -lt 5 ]; then
        fo_cycle_hours="5 8 11 14 17 20 23 2"
      elif [ $cur_hour -lt 8 ]; then
        fo_cycle_hours="8 11 14 17 20 23 2 5"
      elif [ $cur_hour -lt 11 ]; then
        fo_cycle_hours="11 14 17 20 23 2 5 8"
      elif [ $cur_hour -lt 14 ]; then
        fo_cycle_hours="14 17 20 23 2 5 8 11"
      elif [ $cur_hour -lt 17 ]; then
        fo_cycle_hours="17 20 23 2 5 8 11 14"
      elif [ $cur_hour -lt 20 ]; then
        fo_cycle_hours="20 23 2 5 8 11 14 17"
      elif [ $cur_hour -lt 23 ]; then
        fo_cycle_hours="23 2 5 8 11 14 17 20"
      else
        fo_cycle_hours="2 5 8 11 14 17 20 23"
      fi
    fi
  fi
  #echo "$ft_func_name  cycle_hours: [$fo_cycle_hours]" >&2
  echo "$fo_cycle_hours"
}

#Returns running count for the given process
function get_running_count {
  fi_executable=$1
  fo_running_count=0
  
  #echo "cur pid:  $$" >&2
  #ps -ef | grep "$fi_executable" | grep -v grep | grep $USER | grep "/bin"  >&2
  
  ft_running=`ps -ef | grep "$fi_executable" | grep -v grep | grep $USER | grep "/bin" | grep -v $$`
  fo_running_count=`echo $ft_running | wc -l`
  #echo "   get_running_count() running_count: $fo_running_count" >&2
  echo "$fo_running_count"
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
    if [ $fi_hour_pattern -lt 10 ]; then
      ft_hour_pattern="0${fi_hour_pattern}"
    else
      ft_hour_pattern="${fi_hour_pattern}"
    fi
  else
    ft_hour_pattern="*"
  fi
  [ ! -d $fi_output_dir ] && mkdir -p $fi_output_dir
  
  #ft_nc_files=`ls -rt $fi_work_dir/$fi_range.${fi_datatype}*.cdf`
  #echo "$ft_func_name  work_dir: [$fi_work_dir], output_dir: [$fi_output_dir]" >&2
  #echo "$ft_func_name  ls -rt $fi_work_dir/$fi_range.${fi_datatype}*${ft_hour_pattern}.*.cdf" >&2
  ft_nc_files=`ls -rt $fi_work_dir/$fi_range.${fi_datatype}*${ft_hour_pattern}.*.cdf 2> /dev/null`
  for ft_nc_file in $ft_nc_files
  do 
    if [ -e $ft_nc_file ]; then 
      ft_nc_file_name=`basename $ft_nc_file`
      ft_yyyymmddhh=`echo $ft_nc_file_name | cut -f2 -d"." | sed 's/'$fi_datatype'//g'`
      ft_yymmdd=`echo $ft_yyyymmddhh | cut -c3-8`
      ft_hh=`echo $ft_yyyymmddhh | cut -c9-10`
      ft_target_name=$fi_output_dir/${fi_datatype}.${ft_yymmdd}.${ft_hh}0000.cdf

      ft_old_file_size=`get_file_size $ft_target_name`
      ft_new_file_size=`get_file_size $ft_nc_file`
      if [ $ft_new_file_size -ge $ft_old_file_size ]; then
        echo "      - Moving $ft_nc_file to $ft_target_name"
        mv $ft_nc_file $ft_target_name
        #chmod ga+rw $ft_target_name
        chmod 666 $ft_target_name
        ft_latest_link=$fi_output_dir/latest.veri_pair.${ft_hh}.cdf
        [ -e $ft_latest_link ] && rm $ft_latest_link
        ln -sf $ft_target_name $ft_latest_link
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

function set_envars {
  fi_range=$1
  
  ft_range=`echo $fi_range | tr "[:lower:]" "[:upper:]"`
  ft_tmp_script=/tmp/`basename $0 .sh`.$$.tmp
  echo "#!/bin/sh"    > $ft_tmp_script
  ft_envs_script=/model/$USER/GMODJOBS/GW${ft_range}/scripts/env_vars.csh
  export ft_envs_script
  /bin/csh -c 'source $ft_envs_script; \
               echo "export MM5HOME=$MM5HOME"     ;\
               echo "export GSJOBDIR=$GSJOBDIR"   ;\
               echo "export EXECUTABLE_ARCHIVE=$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE"   ;\
               echo "export PYTHON_ARCHIVE=$MM5HOME/cycle_code/PYTHON" ;\
              ' >> $ft_tmp_script
  source $ft_tmp_script
  [ -e $ft_tmp_script ] && rm $ft_tmp_script
}

#----------------------------------------------------
# Main
#

ignore_count=0
running_count=`get_running_count $_script_name`
#echo " running_count: [$running_count]"
running_count=$((running_count - 1))
if [ $running_count -gt 0 ]; then
  cur_in_seconds=`date +%s`
  killed_count=0
  execution_times=`ps -ef | grep "$_script_name" | grep -v grep | grep $USER | grep "/bin/sh" | grep -v $$ | awk '{ print $5;}'` 
  for execution_time in $execution_times
  do
    exe_start_time=`date +%s -d "$execution_time"`
    if [ $((cur_in_seconds - 3600)) -gt $exe_start_time ]; then
      ignore_count=$(( ignore_count + 1 ))
    fi
  done
fi
  
#echo "   running_count: $running_count, ignore_count: $ignore_count"
if [ $running_count -gt 0 -a $ignore_count -ne $running_count ]; then
  echo "   --- WARN --- The script $_script_name is running"
  echo "   exit."
  exit -1
fi

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
MY_LOG_DIR=$HOME/datlog
[ ! -d $MY_LOG_DIR -a ! -L $MY_LOG_DIR ] && MY_LOG_DIR=$HOME/log

date_str=`date +"%Y-%m-%d %H:%M:%S"`
echo "   === Started at $date_str"

start_time=`date +%s`
[ "" == "$ranges" ] && ranges=$target_ranges
if [ "" == "$ranges" ]; then
  echo "  === WARN === The target range is not specified"
fi

#target_ranges="crtc"

#echo "  * Update raw data ..."
#$_script_dir/push_raw_veri_pair.sh

#is_obs_updated=0
#is_model_updated=0

for target_range in $ranges
do
  range_start_time=`date +%s`
  echo "   Processing $target_range ..."

  db_key=${target_range}_verification
  db_name=`get_db_name $db_key`

  set_mysql_login_credentials $db_key    # Sets _MYSQL_OPTS
  set_mysql_command                      # Sets _MYSQL_COMMAND
  #check_mysql_login
  
  set_envars $target_range
  export MM5HOME
  export EXECUTABLE_ARCHIVE=$MM5HOME/cycle_code/EXECUTABLE_ARCHIVE
  
  cur_cycle=`ls $GSJOBDIR/logs | sed -e "s|/||g" | tr " " "\n" | grep 20 | sort | tail -1`
  sql_string="SELECT DATE_FORMAT(MAX(cycle_time), '%Y%m%d%H') FROM surface_model_data WHERE cycle_time > DATE_ADD(NOW(), INTERVAL -7 DAY);"
  last_model_cycle=`$_MYSQL_COMMAND -s -e "$sql_string"`
  echo "    cur_cycle: [$cur_cycle], last_model_cycle: [$last_model_cycle]"

  my_python_path=$PYTHON_ARCHIVE/flex/veri_pair
  [ ! -d $my_python_path ] && my_python_path=$_script_dir
  
  cur_dir=`pwd`
  tmp_cur_dir=`basename $cur_dir`
  if [ "verify" != "$tmp_cur_dir" ]; then
    work_dir=/model/$USER/tmp/analogcode_run/$target_range
    if [ ! -d $work_dir ]; then
      mkdir -p $work_dir
    fi
    cd $work_dir
  fi
  
  echo "     cur dir: `pwd`"
  last_veri_pair_obs_file=$work_dir/last_veri_pair_obs.${target_range}.txt
  prev_veri_pair_obs=""
  if [ -e $last_veri_pair_obs_file ]; then
    prev_veri_pair_obs=`cat $last_veri_pair_obs_file`
  fi
  last_veri_pair_model_file=$work_dir/last_veri_pair_model.${target_range}.txt
  #echo "     last_veri_pair_obs_file: $last_veri_pair_obs_file"
  #echo "   last_veri_pair_model_file: $last_veri_pair_model_file"
  prev_veri_pair_model=""
  if [ -e $last_veri_pair_model_file ]; then
    prev_veri_pair_model=`cat $last_veri_pair_model_file`
  fi
  sql_string="SELECT max(obs_time) FROM surface_obs_data;" 
  #echo "   SQL: $_MYSQL_COMMAND -s -e \"$sql_string\""
  last_veri_pair_obs=`$_MYSQL_COMMAND -s -e "$sql_string"`
  sql_string="SELECT MAX(valid_time) FROM surface_model_data WHERE cycle_time > DATE_ADD(NOW(), INTERVAL -7 DAY);"
  #echo "   SQL: $_MYSQL_COMMAND -s -e \"$sql_string\""
  last_veri_pair_model=`$_MYSQL_COMMAND -s -e "$sql_string"`
  
  is_obs_updated=0
  is_model_updated=0
  [ "$prev_veri_pair_obs"   != "$last_veri_pair_obs" ] &&   is_obs_updated=1
  [ "$prev_veri_pair_model" != "$last_veri_pair_model" ] && is_model_updated=1
  echo "       is_obs_updated: $is_obs_updated, is_model_updated: $is_model_updated"
  echo "       obs: prev - [$prev_veri_pair_obs], cur - [$last_veri_pair_obs]"
  echo "     model: prev - [$prev_veri_pair_model], cur - [$last_veri_pair_model]"
  echo ""

  if [ $is_obs_updated -gt 0 -o $is_model_updated -gt 0 ]; then
    #echo " MM5HOME: $MM5HOME"
    #pwd
    extra_arg=""
    [ $is_obs_updated -gt 0 ] && extra_arg="obs"
    if [ "$last_model_cycle" != "" ]; then
      my_args="$last_model_cycle $my_python_path $target_range $extra_arg"
    else
      my_args="$cur_cycle $my_python_path $target_range $extra_arg"
    fi
    my_cmd="$MM5HOME/cycle_code/CSH_ARCHIVE/verif/run_veri2anen.sh"
    echo "     calling run_veri2anen.sh $my_args"
    $my_cmd $my_args

    [ $is_obs_updated   -eq 1 -a "$last_veri_pair_obs"   != "" ] && \
        echo "$last_veri_pair_obs"   > $last_veri_pair_obs_file
    [ $is_model_updated -eq 1 -a "$last_veri_pair_model" != "" ] && \
        echo "$last_veri_pair_model" > $last_veri_pair_model_file
    
  else
    echo "   No update for $target_range" 
  fi
  
  range_end_time=`date +%s`
  range_duration=$((range_end_time - range_start_time))  
  echo ""
  echo "    - took $range_duration seconds [`format_duration $range_duration`] for $target_range "
  echo ""
done


end_time=`date +%s`
duration=$((end_time - start_time))
date_str=`date +"%Y-%m-%d %H:%M:%S"`
echo "   === Done $_script_name === took $duration seconds [`format_duration $duration seconds`] at $date_str"
echo ""
