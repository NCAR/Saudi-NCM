#!/bin/csh
#set echo
###############################################################################
#
# Usage: submitMCycles.csh -b begin_cycle -e end_cycle -i interval <-s start_time> <-f env_var_file> -h
#
################################################################################

set argv=`getopt b:e:i:s:f:h $*`
set begin_cycle = 0
set end_cycle = 0
set interval = 0
set account = 0
set start = 0
set env_var_file = "./env_vars.csh"

set argsstr="$argv"

echo $0

foreach name ($argv)

  switch ($name)
            case -b:
               set begin_cycle = $2
               breaksw
            case -e:
               set end_cycle = $2
               breaksw
            case -i:
               set interval = $2
               breaksw
            case -s:
               set start = $2
               breaksw
            case -f:
               set env_var_file = $2
               breaksw
            case -h:
               echo "$0 -b begin_cycle -e end_cycle -i interval <-f env_var_file> <-s start_time>"
               echo " where  "
               echo "   -b begin_cycle: the first cycle, format is YYYYMMDDhh (required)"
               echo "   -e end_cycle: the last cycle, format is YYYYMMDDhh (required)"
               echo "   -i interval: the interval between cycles (in hours),"
               echo "                has to be the same as CYC_INT in flexinput.pl (required)"
               echo "   -s start_time: when should Moab run the jobs, format is YYYYMMDDhhmm.ss (optional)"
               echo "   -f env_var_file:  e.g. /raidN/Logname/GMODJOBS/JOBID/scripts/env_vars.csh, defaults to ./env_vars.csh"
               echo " "
               exit -1
               breaksw
            case --:
               breaksw
  endsw

  shift
end

if ( $begin_cycle == 0 || $end_cycle == 0 || $interval == 0) then
  echo "$0 -b begin_cycle -e end_cycle -i interval <-f env_var_file> <-s start_time>"
  echo " where  "
  echo "   -b begin_cycle: the first cycle, format is YYYYMMDDhh (required)"
  echo "   -e end_cycle: the last cycle, format is YYYYMMDDhh (required)"
  echo "   -i interval: the interval between cycles (in hours),"
  echo "                has to be the same as CYC_INT in flexinput.pl (required)"
  echo "   -s start_time: when should Moab run the jobs, format is YYYYMMDDhhmm.ss (optional)"
  echo "   -f env_var_file:  e.g. /raidN/Logname/GMODJOBS/JOBID/scripts/env_vars.csh, defaults to ./env_vars.csh"
  echo " "
  exit -1
endif

echo "begin_cycle = $begin_cycle"
echo "end_cyle = $end_cycle"
echo "interval = $interval"
echo "start_time = $start"
echo "env-vars file: ${env_var_file}"

set current_cycle = $begin_cycle

source ${env_var_file}

if (! -d ${GSJOBDIR}/logs) mkdir -p ${GSJOBDIR}/logs
if (! -d ${GSJOBDIR}/tmp)  mkdir -p ${GSJOBDIR}/tmp

if (! -e ${GSJOBDIR}/tmp/cshrc)  rm -f ${GSJOBDIR}/tmp/cshrc
if (! -e ${GSJOBDIR}/tmp/pre_process_in_donotedit.pl)  rm -f ${GSJOBDIR}/tmp/pre_process_in_donotedit.pl

if ( $start != 0) then
  echo "Submitting -c $current_cycle -s $start"
  ${GSJOBDIR}/scripts/submitCycleMM.csh -c $current_cycle -s $start -f ${env_var_file} >& ${GSJOBDIR}/logs/${current_cycle}_submit.log
else
  echo "Submitting -c $current_cycle"
  ${GSJOBDIR}/scripts/submitCycleMM.csh -c $current_cycle -f ${env_var_file} >& ${GSJOBDIR}/logs/${current_cycle}_submit.log
endif

if (-e ${GSJOBDIR}/tmp/${current_cycle}/cshrc) then
    echo sourcing ${GSJOBDIR}/tmp/${current_cycle}/cshrc
    source ${GSJOBDIR}/tmp/${current_cycle}/cshrc
else
    echo "ERRROR: Cannot find file ${GSJOBDIR}/tmp/${current_cycle}/cshrc" 
    exit -1
endif

setenv LOGDIR ${GSJOBDIR}/logs/$current_cycle
set wait_for_jobid = `cat ${LOGDIR}/moabID_post_clean`

set current_cycle = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $current_cycle $interval`

echo "current cycle $current_cycle"

while ( $current_cycle <= $end_cycle )

  echo "Submitting -c $current_cycle -W ${wait_for_jobid}"
  ${GSJOBDIR}/scripts/submitCycleMM.csh -c $current_cycle -f ${env_var_file} -W ${wait_for_jobid} >& ${GSJOBDIR}/logs/${current_cycle}_submit.log

  # get the jobid of post_clean
  setenv LOGDIR ${GSJOBDIR}/logs/$current_cycle
  set wait_for_jobid = `cat ${LOGDIR}/moabID_post_clean`

  set current_cycle = `${EXECUTABLE_ARCHIVE}/geth_newdate.exe $current_cycle $interval`

end

#------------------------------------------------------------------------------#
# Move obs QC files from cycles directory to a common repository 
# when variable OUTPUT_DIR has been set in env_vars.csh 

if ($?OUTPUT_DIR > 0) then
# Move the qc_out files in the ouput dir once runs are over

echo \
"moveMCycles.csh -b $begin_cycle -e $end_cycle -f ${env_var_file} -t qc_out -W ${wait_for_jobid}"

 ${QSUB_PATH}/qsub -N ${end_cycle}_Q -l nodes=1:ppn=2,walltime=3600 -j oe -o ${GSJOBDIR}/logs/${end_cycle}_move_qc_out.log -v begin_cycle=$begin_cycle,end_cycle=$end_cycle,env_var_file=${env_var_file},output_type=qc_out -W depend=afterany:$wait_for_jobid ${GSJOBDIR}/scripts/moveMCycles.csh >& ${GSJOBDIR}/logs/${end_cycle}_moabID_move_qc_out 

  # get the jobid of moveMCycles
  set wait_for_jobid = `cat ${GSJOBDIR}/logs/${end_cycle}_moabID_move_qc_out`

#------------------------------------------------------------------------------#
# Move WRF output files from cycles directory to a common repository 
# when variable OUTPUT_DIR has been set in env_vars.csh 

echo \
"moveMCycles.csh -b $begin_cycle -e $end_cycle -f ${env_var_file} -t wrfout -W ${wait_for_jobid}"

 ${QSUB_PATH}/qsub -N ${end_cycle}_W -l nodes=1:ppn=2,walltime=3600 -j oe -o ${GSJOBDIR}/logs/${end_cycle}_move_wrfout.log -v begin_cycle=$begin_cycle,end_cycle=$end_cycle,env_var_file=${env_var_file},output_type=wrfout -W depend=afterany:$wait_for_jobid ${GSJOBDIR}/scripts/moveMCycles.csh >& ${GSJOBDIR}/logs/${end_cycle}_moabID_move_wrfout

endif
#------------------------------------------------------------------------------#

exit 0
