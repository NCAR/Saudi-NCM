#!/bin/tcsh -f
#
###############################################################################
echo ""
echo  "-----------------------------------------------------------------------"
echo  " ---------------- SAMS decoder starts  --------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set timestamp
setenv SUBSYSTEM SAMS_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
  echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
  exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};

# ES temporary: on 'newer' clusters ncdump should be in /opt/netcdf/bin
if ( $?NETCDF ) then
  set path = ( $NETCDF/bin $path )
endif
set is_netcdf_available = `which ncdump | grep "not found" | wc -l`
if ($is_netcdf_available != 0) then
  if (-e /opt/netcdf/bin/ncdump) then
    set path = ( /opt/netcdf/bin $path )
  else if ( -e /opt/cray/netcdf/default/bin ) then
    set path = ( $path /opt/cray/netcdf/default/bin )
  else if ( $?NETCDF ) then
    set path = ( $NETCDF/bin $path )
  endif
endif

#set echo

#	Check usage

if ( ${#argv} == 2 ) then
  set cycle_date = $1
  set obs_date = $2
else
  echo "usage: $0 cycle_date obs_date"
  echo "where cycle_date and obs_date is CCYYMMDDHH"
  exit ( 4 )
endif

set DEBUG = 0
set opt_reuse = 0
set retry_count = 10
set email_notice = 1
set email_to = "hsoh@ucar.edu"
#set email_to = "hsoh@ucar.edu,jshaw@ucar.edu,becky@ucar.edu,fisherh@ucar.edu,sheu@ucar.edu"
set target_range_name = `echo ${MM5HOST} | tr "[:upper:]" "[:lower:]"`
set target_range_code = $target_range_name
if ($target_range_code == "rtc") set target_range_code = "rttc"
set target_sams_dir = `echo ${SAMS_DATA_DIR_DPG} | sed -e 's/dpg/'$target_range_name'/g'`
set cycle_ymd = `echo $cycle_date | cut -c1-8`
set cycle_hh  = `echo $cycle_date | cut -c9-10`
set target_obs_date = $cycle_date
if ("00" == $cycle_hh && $cycle_date != $obs_date) then
  set target_obs_date = $obs_date
endif

set OBS_HOUR_BEFORE = -6
set OBS_HOUR_AFTER  =  4
set cycle_time_in_sec = `date -u +%s -d "$cycle_ymd $cycle_hh"`
@ min_obs_time_in_sec = $cycle_time_in_sec + ($OBS_HOUR_BEFORE * 3600)

set sams_decoded_dir = $RUNDIR/${cycle_date}/DECODE_SAMS
if(-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/DECODE_SAMS
  if(! -e $sams_decoded_dir) ln -s $GEAPSTMP/DECODE_SAMS $sams_decoded_dir
else
  $MustHaveDir $sams_decoded_dir
endif
set REAL_RANGES = (DPG EPG YPG WSMR ATC CRTC RTC)
set RANGES = ($REAL_RANGES IAF)

echo " sams_decoded_dir: $sams_decoded_dir ============================"
cd $sams_decoded_dir

set is_regular_range = 0
foreach range ($REAL_RANGES)
  if ($range == ${MM5HOST} ) then
    set is_regular_range = 1
    break
  endif
end
set failed_crfil_rt_count = 0
set failed_crfil_rt_count_range = 0

set my_pbs_job_id = "unknown_job_id"
set my_remote_host="bma"
if ($?PBS_JOBID) then
  set my_pbs_job_id = $PBS_JOBID
  set my_remote_host = `echo $my_pbs_job_id | cut -f2 -d"." | cut -f1 -d"-"`
else
  set my_remote_host = `hostname -s | cut -f1 -d"-"`
endif

set bad_input_base_dir = $RUNDIR
if ( -d /model/$LOGNAME ) then
  set bad_input_base_dir = /model/$LOGNAME
else if ( -d /p/work2/$LOGNAME ) then
  set bad_input_base_dir = /p/work2/$LOGNAME
else if ( -d /p/work1/$LOGNAME ) then
  set bad_input_base_dir = /p/work1/$LOGNAME
endif
set bad_input_dir = $bad_input_base_dir/tmp/bad_sams_input
if ( $?GSJOBID ) set bad_input_dir = $bad_input_dir/$GSJOBID
if ( ! -d $bad_input_dir ) then
  mkdir -p $bad_input_dir
  set tmp_status = $?
  if ($tmp_status != 0) then
    set bad_input_dir = $RUNDIR/tmp/bad_sams_input
    if ( ! -d $bad_input_dir ) mkdir -p $bad_input_dir
  endif
endif

set valid_range_sams_count = 0
set matched_target_range_hours = 0
set tmp_file = "`pwd`/sams_count_$$.msg"
set tmp_email_file = "`pwd`/sams_count_email_$$.msg"
#if ( -e $tmp_email_file ) rm $tmp_email_file

foreach range ($RANGES)
  rm -rf sams* rtms*
  set range_l = `echo ${range} | tr "[:upper:]" "[:lower:]"`
  if ($range_l == "rtc") set range_l = "rttc"
  
  set is_target_range = 0
  if ($range == ${MM5HOST} ) set is_target_range = 1
  set sams_date = `echo "${obs_date}" | cut -c 3-8`
  echo "INFO: DATES=$sams_date"
  
  if ($range == IAF) then
    cp ${SAMS_DATA_DIR_DPG}/../../iaf/sams/iaf*${sams_date}* .
  else
    set t_sams_data_dir = ""
    if($range == DPG) then
      cp ${SAMS_DATA_DIR_DPG}/../pwids/pwids*${sams_date}* .
      set t_sams_data_dir = ${SAMS_DATA_DIR_DPG}
    else if ($range == EPG) then
      set t_sams_data_dir = ${SAMS_DATA_DIR_EPG}
    else if ($range == YPG) then
      set t_sams_data_dir = ${SAMS_DATA_DIR_YPG}
    else if ($range == WSMR) then
      set t_sams_data_dir = ${SAMS_DATA_DIR_WSMR}
    else if ($range == ATC) then
      set t_sams_data_dir = ${SAMS_DATA_DIR_ATC}
    else if ($range == CRTC) then
      set t_sams_data_dir = ${SAMS_DATA_DIR_CRTC}
    else if ($range == RTC) then
      set t_sams_data_dir = ${SAMS_DATA_DIR_RTTC}
    else if ($range == NVL) then
      set t_sams_data_dir = ${SAMS_DATA_DIR_NVL}
    endif
    cp -p ${t_sams_data_dir}/sams*${sams_date}* .
  endif
  
  @ n = 0
  set sams_decoded_output = $range.sams.$sams_date
  if( -e $sams_decoded_output) then
    set file_size = `stat -c %s $sams_decoded_output`
    if ($file_size > 0 ) mv $sams_decoded_output $sams_decoded_output.prev
  endif
  
  foreach f ( `ls -1 sams*${sams_date}* rtms*${sams_date}* pwids*${sams_date}* iaf*${sams_date}*`)
    rm -rf input output
    #if($range == YPG) then
    #${EXECUTABLE_ARCHIVE}/samsproc -F $f
    #mv $f ${f}.old
    #mv fred $f
    #endif
    echo "$f" > input
    chmod 777 *
    set fsn="../netcdf.stations"
    echo " &OPARAM"   > $fsn
    echo " strname=" >> $fsn
    ncdump -v platform $f | grep \"  | sed 's/;//' >> $fsn
    set input_status = $?
    echo " &END"     >> $fsn

    # Backup SAMS NetCDF file
    if ($is_target_range == 1) cp -p $f save_${range}_$f
    
    set obs_base_time   = `ncdump -v base_time   $f | sed -e '1,/^data:$/d' | grep \; | rev | cut -f2 -d" " | rev | bc`
    set obs_time_offset = `ncdump -v time_offset $f | sed -e '1,/^data:$/d' | grep \; | rev | cut -f2 -d" " | rev | bc`
    if ( "$obs_base_time"   == "") set obs_base_time   = 0
    if ( "$obs_time_offset" == "") set obs_time_offset = 0
    @ last_obs_time_in_sec = $obs_base_time + $obs_time_offset
    if ($last_obs_time_in_sec < $min_obs_time_in_sec) then
      echo " == CHECK SAMS == Skip decoding ${range}/${f}. latest obs_time: `date +'%Y-%m-%d %H:%M:%S' -d @$last_obs_time_in_sec`"
      continue
    endif
    
    if ($is_target_range == 1) @ valid_range_sams_count++

    set log_msg = " == CHECK SAMS == Processing $range/$f  input md5sum `md5sum $f`"
    echo ""         >> print.out
    echo "$log_msg" >> print.out
    echo "$log_msg"
    echo "   input md5sum `md5sum $f` for $range"               >>  $tmp_email_file
    set log_msg = "   input md5sum `md5sum $t_sams_data_dir/$f`"
    echo $log_msg
    echo $log_msg                                               >>  $tmp_email_file
    set sams_output_count = 0
    foreach idx ( `seq 1 $retry_count` )
      ${EXECUTABLE_ARCHIVE}/crfil_rt < input  >> print.out
      
      set sams_output_count = `ls *_SAMS | wc -w`
      set fail_to_run_crfil_rt = `tail -n 4 print.out | grep -i ERROR | wc -l`
      if (0 < $sams_output_count && 0 == $fail_to_run_crfil_rt) break
      
      set no_obs_time = `tail -n 4 print.out | grep "Error getting array time" | wc -l`
      if ($no_obs_time > 0) then
        set log_msg = " == CHECK SAMS == Can not find the obs time array from ${range}/${f} by crfil_rt"
        echo $log_msg
        echo $log_msg >> print.out
        echo $log_msg >> $tmp_email_file
        break
      endif

      @ failed_crfil_rt_count++
#      if ($target_obs_date != $obs_date) continue
      if ($is_target_range == 1) @ failed_crfil_rt_count_range++
      
      set log_f = ${range}/${f}
      echo " == CHECK SAMS == found error on ${idx}th crfil_rt with $log_f"
      echo " == CHECK SAMS == log from print.out for ${range}/${f}:"
      tail -n 3 print.out
      set log_msg = "   failed ${idx}th md5sum `md5sum $f` (from $t_sams_data_dir/$f)"
      echo $log_msg
      echo $log_msg                                                 >>  $tmp_email_file
      
      if ($DEBUG > 0) cp -p "$f" bad_${range}_${f}.`date "+%Y%m%d_%H%M%S"`
      if ($email_notice == 1 && $idx == 1) then
        echo " Input file: $log_f at `date +'%Y-%m-%d %H:%M:%S'`"   >>  $tmp_email_file
        echo ""                                                     >>  $tmp_email_file
        ncdump -v base_time,platform,time_offset $f > $tmp_file
        set input_status = $?
        cat $tmp_file
        cat $tmp_file                                               >>  $tmp_email_file
        echo ""                                                     >>  $tmp_email_file
        echo "From print.out:"                                      >>  $tmp_email_file
        tail -n 4 print.out                                         >>  $tmp_email_file
      endif
      echo ""                                                       >>  $tmp_email_file
      ls -l $f $t_sams_data_dir/$f                                  >>& $tmp_email_file
      set log_msg = "   failed ${idx}th md5sum `md5sum $f` from $t_sams_data_dir/$f"
      echo $log_msg
      echo $log_msg                                                 >>  $tmp_email_file
      if ($input_status != 0) then
        set log_msg = " == WARN == Bad NetCDF input $f for $range by ncdump"
        echo $log_msg
        echo $log_msg                                               >>  $tmp_email_file
        break
      endif
      
      sleep 2
    end

    @ tmp_n = 0
    set lss=`ls *_SAMS`
    foreach i ( $lss )
    @  n++
    @  tmp_n++
    end
    
    #if( -e $sams_decoded_output) rm $sams_decoded_output
    if ($tmp_n > 0) then
      set matched_count = 0
      set cycle_date_hh = `echo $cycle_date |cut -c 1-10`
      foreach h (`seq $OBS_HOUR_BEFORE $OBS_HOUR_AFTER`)
        echo "$cycle_date_hh , $h" >! input
        ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
        set want_date = `cat output`
        echo "want_date=" $want_date
        cat ${want_date}*_SAMS >> $sams_decoded_output
        set match_hh_count = `ls ${want_date}*_SAMS | wc -w`
        @ matched_count = $matched_count + $match_hh_count
      end
      
      if (1 == $is_target_range && $target_obs_date == $obs_date && 0 == $matched_count) then
        echo "No matched hourly SAMS for $cycle_date [$MM5HOST]"    >>  $tmp_email_file
        ls -l *_SAMS                                                >>  $tmp_email_file
        ncdump -v base_time,platform,time_offset $f                 >>& $tmp_email_file
      else
        @ matched_target_range_hours = $matched_target_range_hours + $matched_count
      endif
    else if ($is_target_range == 1) then
      set log_msg = "  == ERROR == No output from crfil_rt for $cycle_date ($range/$f) [$MM5HOST]"
      echo $log_msg
      echo $log_msg                                                 >>  $tmp_email_file
    endif
    rm *_SAMS
    
    set sams_obs_count = `grep "SAMS ATEC" $sams_decoded_output | grep $range_l | wc -l`
    if ($sams_obs_count == 0 && "RTC" == $range) then
      set tmp_range_l = "rttc"
      if ($range_l == $tmp_range_l) set tmp_range_l = "rtc"
      set sams_obs_count = `grep "SAMS ATEC" $sams_decoded_output | grep $tmp_range_l | wc -l`
    endif
    set log_msg = " == CHECK SAMS ==  Processed $range/$f. sams count: $sams_obs_count"
    echo ""         >> print.out
    echo "$log_msg" >> print.out
    echo "$log_msg"
    
  end # f
  
  if ($n == 0 && 0 != $opt_reuse) then
    if( -e ${range}.sams.${sams_date}.prev) then
      if ( -e ${range}.sams.${sams_date}) then
        set sams_size = `stat -c %s ${range}.sams.${sams_date}`
        if (0 == $sams_size) rm ${range}.sams.${sams_date}
      endif
      if ( ! -e ${range}.sams.${sams_date}) then
        mv ${range}.sams.${sams_date}.prev ${range}.sams.${sams_date}
      endif
    endif
  endif

  echo "${SUBSYSTEM} -- CREATED -> $n FILES OF SAMS DATA for $range ($obs_date)"
  if ($is_target_range == 1 && $n == 0 ) then
    set copy_time = `date +%m%d_%H%M`
    foreach sams_file ( `ls -1 sams*${sams_date}*` )
      cp -p ${sams_file} $bad_input_dir/bad_0_out_${range}_${sams_file}.$copy_time
      echo " == CHECK SAMS == input [`ls -l ${t_sams_data_dir}/$sams_file`] at `hostname`"
      echo " == CHECK SAMS ==       [`ls -l $bad_input_dir/bad_0_out_${range}_${sams_file}.$copy_time`]"
    end
  endif
end  #range

if( -e ${obs_date}.SAMS.allrange) rm ${obs_date}.SAMS.allrange

foreach range ($RANGES)
  cat $range.sams.$sams_date >> ${obs_date}.SAMS.allrange
end  #range

@ sams_obs_count = 0
set sams_decoded_output_all = ${obs_date}.SAMS.allrange
set file_size = `stat -c %s $sams_decoded_output_all`
if ($file_size > 0) then
  set sams_obs_count = `grep "SAMS ATEC" $sams_decoded_output_all | grep $target_range_code | wc -l`
  if ($sams_obs_count == 0 && "RTC" == $MM5HOST) then
    set tmp_range_l = "rttc"
    if ($target_range_code == $tmp_range_l) set tmp_range_l = "rtc"
    set sams_obs_count = `grep "SAMS ATEC" $sams_decoded_output_all | grep $tmp_range_l | wc -l`
  endif
endif
echo "${SUBSYSTEM} == CHECK SAMS == SAMS count for ${MM5HOST}/${obs_date}: $sams_obs_count at `date +'%Y-%m-%d %H:%M:%S'`"

if (1 == $email_notice && $target_obs_date == $obs_date) then
  set email_title_postfix = "[$MM5HOST / $cycle_date / $obs_date] from $my_remote_host"
  set email_title = "No SAMS obs $email_title_postfix"
  set saved_input_sams_name = save_${MM5HOST}_sams.${sams_date}.000000.cdf
  if (0 == $valid_range_sams_count) then
    set email_title = "No SAMS input $email_title_postfix"
  else if (0 == $matched_target_range_hours) then
    set email_title = "No decoded SAMS output $email_title_postfix"
    if (-e $saved_input_sams_name) then
      if ($failed_crfil_rt_count == 0) then
        set email_title = "No decoded SAMS output without any failures of crfil_rt $email_title_postfix"
      else if ($failed_crfil_rt_count_range == 0) then
        set email_title = "No decoded SAMS output without failure of crfil_rt, $email_title_postfix"
      endif
    endif
  endif
  if (0 == $sams_obs_count || 0 == $matched_target_range_hours) then
    echo "${SUBSYSTEM} == CHECK SAMS == No matching SAMS observation data at `hostname` `date +'%Y-%m-%d %H:%M:%S'`"
    
    if (0 < $valid_range_sams_count) echo "Found $valid_range_sams_count valid SAMS input [${MM5HOST}/$cycle_date/$obs_date)]"   >> $tmp_email_file
    #ls -l sams_count_email_*.msg                                                                  >> $tmp_email_file
    echo "Found 0 SAMS data ${MM5HOST}/$cycle_date for obs_date ($obs_date) at `date +'%D %T'`"   >> $tmp_email_file
    echo "       Other SAMS count: `grep 'SAMS ATEC' $sams_decoded_output_all | wc -l`"           >> $tmp_email_file
    echo "   Failures of crfil_rt: all: ${failed_crfil_rt_count}, ${MM5HOST}: $failed_crfil_rt_count_range"  >> $tmp_email_file
    echo ""                                            >> $tmp_email_file
    echo "  === File list ==="                         >> $tmp_email_file
    ls -l                                              >> $tmp_email_file
    echo ""                                            >> $tmp_email_file
    echo "  === Input SAMS NetCDF files ==="           >> $tmp_email_file
    ls -l ${target_sams_dir}/*${sams_date}* $saved_input_sams_name  >> $tmp_email_file
    if (-e $saved_input_sams_name) md5sum $saved_input_sams_name    >> $tmp_email_file
    set input_sams_name = ${target_sams_dir}/sams.${sams_date}.000000.cdf
    if (-e $input_sams_name) then
      md5sum $input_sams_name                                   >> $tmp_email_file
      echo ""                                                   >> $tmp_email_file
      ncdump -v base_time,platform,time_offset $input_sams_name >>& $tmp_email_file
    endif
    echo ""                                            >> $tmp_email_file
    echo "From print.out:"                             >> $tmp_email_file
    grep "CHECK_SAMS" print.out                        >> $tmp_email_file
    echo ""                                            >> $tmp_email_file
    echo "  job id: $my_pbs_job_id"                    >> $tmp_email_file
    echo ""                                            >> $tmp_email_file
  endif
  if (-e $tmp_email_file && $is_regular_range == 1) then
    cat $tmp_email_file | mail -s "$email_title" $email_to
    if ($? != 0) cat $tmp_email_file
  endif
  
  if (-e $tmp_email_file && $is_regular_range == 1) then
    cp -p $tmp_email_file $bad_input_dir/${MM5HOST}_${cycle_date}_`basename ${tmp_email_file}`
  endif
endif

if (0 == $sams_obs_count) cp -rpH $sams_decoded_dir ${sams_decoded_dir}.backup.${obs_date}.`date +%H%M%S`
if (-e $tmp_file) rm $tmp_file

exit 0
