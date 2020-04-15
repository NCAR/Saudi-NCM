#!/bin/csh -f
#
# This script is called by apps/4dwx/GJOBS/mm-template/script/submitCycleMM.csh
# (cvs location). It sets necessary MM env-vars and calls RT_L_mm5_rtfdda.csh.
#
###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  "$0 $argv[*]"
echo  "  Setting env vars necessary for MM and calling RT_L_mm5_rtfdda.csh"
echo  " ----------------------------------------------------------------------"
###############################################################################

# ES - 20071218: Until QC and verification is adjusted, i.e., both can handle
# mm5out-files in cycle_dir and cycle_dir/MM5_*, set the MM env-var to "notMM".
# This will force RT_L_mm5_rtfdda.csh to do the moving of mm5 output files from 
# cycle_dir/MM5_* to cycle_dir.
# When MM = "MM", then RT_L_mm5_rtfdda.csh *WILL NOT* move mm5 output files from 
# cycle_dir/MM5_* to cycle_dir.   

echo PID: $$

setenv MM "notMM"

# $CSHRC_RT is $GSJOBDIR/tmp/$this_cycle/cshrc
echo "RT_L_MM_MM5_rtfdda.csh: sourcing $CSHRC_RT"
source $CSHRC_RT

# $CSHRC_MM5 is RUNDIR/$this_cycle/cshrc.MM5_F or cshrc.MM5_P
# It contains $NUM_NODES, $PPN, etc
echo "RT_L_MM_MM5_rtfdda.csh: sourcing $CSHRC_MM5"
source $CSHRC_MM5

if ( $BATCH_SYSTEM == "PBS" ) then
  echo "  PBS_JOBID: $PBS_JOBID"
  echo "  Allocated nodes are: "
  cat $PBS_NODEFILE
  echo ""
endif

### The PBS directive
#PBS -l nodes=${NUM_NODES}:ppn=${PPN}

echo "############### START ENVVARS #######################################"
env
echo "################ END ENVVARS  #######################################"

# RT_L_mm5_rtfdda.csh takes 9 arguments:
# set this_cycle = $1
# set fcst_id    = $2
# set timemax    = $3
# set resttime   = $4
# set cpun       = $5
# set d4start    = $6
# set out_int    = $7
# set NODE       = $8
# set ETAAVN     = $9

set this_cycle = $this_cycle
set fcst_id    = $fcst_id
set timemax    = $timemax
set resttime   = $resttime
set cpun       = $NUM_PROCS
set d4start    = $D4_start
set out_int    = $OUT_INT
set NODE       = $NODE
set ETAAVN     = $BCS
set NAMGFS     = $BCS

echo "Calling $CSH_ARCHIVE/Forecast/RT_L_mm5_rtfdda.csh $this_cycle $fcst_id $timemax $resttime $cpun $d4start $out_int $NODE $BCS"
echo

ls -l $CSH_ARCHIVE/Forecast/RT_L_mm5_rtfdda.csh

$CSH_ARCHIVE/Forecast/RT_L_mm5_rtfdda.csh $this_cycle $fcst_id $timemax $resttime $cpun $d4start $out_int $NODE $BCS
