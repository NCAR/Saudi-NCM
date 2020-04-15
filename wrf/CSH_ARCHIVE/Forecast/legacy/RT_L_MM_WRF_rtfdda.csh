#!/bin/csh -f

#
# This script is called by apps/4dwx/GJOBS/mm-template/script/submitCycleMM.csh
# (cvs location). It sets necessary MM env-vars and calls RT_L_wrf_rtfdda.csh.
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  "$0 $argv[*]"
echo  "  Setting env vars necessary for MM and calling RT_L_wrf_rtfdda.csh"
echo  " ----------------------------------------------------------------------"
###############################################################################

# ES - 20071218: Until QC and verification is adjusted, i.e., both can handle
# wrfout-files in cycle_dir and cycle_dir/WRF_*, set the MM env-var to "notMM".
# This will force RT_L_wrf_rtfdda.csh to do the moving of wrf output files from 
# cycle_dir/WRF_* to cycle_dir.
# When MM = "MM", then RT_L_wrf_rtfdda.csh *WILL NOT* move wrf output files from 
# cycle_dir/WRF_* to cycle_dir.   

echo PID: $$

setenv MM "notMM"

# $CSHRC_RT is $GSJOBDIR/tmp/$this_cycle/cshrc
echo "RT_L_MM_WRF_rtfdda.csh: sourcing $CSHRC_RT"
source $CSHRC_RT

# $CSHRC_WRF is RUNDIR/$this_cycle/cshrc.WRF_F or cshrc.WRF_P+FCST
# It contains $NUM_NODES, $PPN, etc
echo "RT_L_MM_WRF_rtfdda.csh: sourcing $CSHRC_WRF"
source $CSHRC_WRF

if ( $BATCH_SYSTEM == "PBS" )then
  echo "  PBS_JOBID: $PBS_JOBID"
  echo "  Allocated nodes are: "
  cat $PBS_NODEFILE
  echo ""
endif

### The PBS directive
#PBS -l nodes=${NUM_NODES}:ppn=${PPN}

#echo "############### START ENVVARS #######################################"
#env
#echo "################ END ENVVARS  #######################################"

# set this_cycle = $1 = $this_cycle - in tmp/$this_cycle/cshrc
# set fcst_id    = $2 = $fcst_id - in RUNDIR/$this_cycle/cshrc.WRF_F
# set FCST_H     = $3 = $CF_LENGTH - in RUNDIR/$this_cycle/cshrc.WRF_F
# set start_date = $4 = $f_fdda_start - in RUNDIR/$this_cycle/cshrc.WRF_F
# set cpun       = $5 = $NUM_PROCS - in tmp/$this_cycle/cshrc
# set d4start    = $6 = $D4_start - in tmp/$this_cycle/cshrc
# set out_int    = $7 = $OUT_INT - in tmp/$this_cycle/cshrc
# set NODE       = $8 = $NODE - in tmp/$this_cycle/cshrc
# set ETAAVN     = $9 = $BCS - in tmp/$this_cycle/cshrc

set this_cycle = $this_cycle
set fcst_id    = $fcst_id
set FCST_H     = $FCST_H
set start_date = $start_date
set cpun       = $NUM_PROCS
set d4start    = $D4_start
set out_int    = $OUT_INT
set NODE       = $NODE
set ETAAVN     = $BCS
set NAMGFS     = $BCS

if($fcst_id == 2) set FCST_H = $CYC_INT

set __script_start_time = `date +%s`
# FCV 20080130 force forecast on domain 1 only for C-FDDA (input data NNRP, NNRP2 or FNL)
if ($BCS == "NNRP" || $BCS == "NNRP2" || $BCS == "FNL" || $BCS == "CFSV1" || $BCS == "CFSV2" || $BCS == "CFSR" || $BCS == "CFSF") then
    if ($fcst_id == 1 || $fcst_id == 2) then   # Final FDDA cycle
         echo "Final analysis NUM_DOMS = $NUM_DOMS"
    else # Forecast for CFDDA, use first domain only
         echo
         setenv NUM_DOMS 1
         echo "CFDDA Forecast: Use first domain only, set NUM_DOMS = $NUM_DOMS"
         echo
    endif
endif

# Create GEAPSTMP if does not exist (non interactive only)
if ($BATCH_SYSTEM != "INTER") then
 if (! -d $GEAPSTMP) mkdir -p $GEAPSTMP
endif 

set i=1
set end_date=$start_date
while ( $i < 217 && $i <= $FCST_H)
#echo "$end_date , 1" >! input
#${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
#set end_date = `cat output`
 set end_date = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $end_date 1`
 @ i ++
end

echo "RT_L_MM_WRF_rtfdda.csh: fcst_id=$fcst_id start=$start_date end=$end_date FH=$FCST_H"
echo

set y_start = `echo $start_date | cut -b 1-4`
set m_start = `echo $start_date | cut -b 5-6`
set d_start = `echo $start_date | cut -b 7-8`
set h_start = `echo $start_date | cut -b 9-10`

set y_end = `echo $end_date | cut -b 1-4`
set m_end = `echo $end_date | cut -b 5-6`
set d_end = `echo $end_date | cut -b 7-8`
set h_end = `echo $end_date | cut -b 9-10`

# ES: we need *_end from F-Analysis in *.pl, so write them back to a file - argh!!!
set wrf_type="${MODEL}_P+FCST"
if ($fcst_id <= 2) then
  echo "#\!/usr/bin/perl" >> $GSJOBDIR/tmp/$this_cycle/wrf_end_dates.pl
  echo '$y_end' = \"$y_end\" ';' >> $GSJOBDIR/tmp/$this_cycle/wrf_end_dates.pl
  echo '$m_end' = \"$m_end\" ';' >> $GSJOBDIR/tmp/$this_cycle/wrf_end_dates.pl
  echo '$d_end' = \"$d_end\" ';' >> $GSJOBDIR/tmp/$this_cycle/wrf_end_dates.pl
  echo '$h_end' = \"$h_end\" ';' >> $GSJOBDIR/tmp/$this_cycle/wrf_end_dates.pl
  set wrf_type="${MODEL}_F"
endif

# Call the common RT_L_wrf_rtfdda.csh:
echo "Calling $CSH_ARCHIVE/Forecast/RT_L_wrf_rtfdda.csh $this_cycle $fcst_id $FCST_H $start_date $NUM_PROCS $D4_start $OUT_INT $NODE $BCS"

$CSH_ARCHIVE/Forecast/RT_L_wrf_rtfdda.csh $this_cycle $fcst_id $FCST_H $start_date $NUM_PROCS $D4_start $OUT_INT $NODE $BCS

set __script_end_time = `date +%s`
set __script_duration = `echo "$__script_end_time - $__script_start_time" | bc`
echo "  --- ${wrf_type}: _START_TIME: `date +'%Y/%m/%d %H:%M:%S' -d @$__script_start_time` $__script_start_time"
echo "  --- ${wrf_type}:   _END_TIME: `date +'%Y/%m/%d %H:%M:%S' -d @$__script_end_time` $__script_end_time"
echo "  --- ${wrf_type}: at `date +'%Y/%m/%d %H:%M:%S'`, duration $__script_duration"
