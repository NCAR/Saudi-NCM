#!/bin/csh

###############################################################################
#
# Usage: check_cycle.csh FULL_PATH_TO_ENV_VARS.CSH  rangename
#
#    FULL_PATH_TO_ENV_VARS should be 'GSJOBDIR'/scripts/env_vars.csh
#    'rangename'  should be the name of the range to a) search for
#    range obs and b) name the range report.
#
#    Example:
#    To report the status of a 3-hourly cycling RTFDDA, add the following to
#	your crontab:
#
#    30 0,3,6,9,12,15,18,21 * * * 'MM5HOME'/cycle_code/CSH_ARCHIVE/util/cycle_report.csh full_path_to_env_vars.csh GRM  >& 'GSJOBDIR'/logs/cycle_report.log
#
################################################################################

set env_vars = $1
set rangename = $2
set CHECK_COMPLETE = $3 # optional, if exists and equal 1, check cycle completeness
echo "cycle_report.csh - using env_vars file: $env_vars"

if ( $env_vars == "" ) then
 echo "'FULL_PATH_TO_ENV_VARS' is not specified.... exiting"
 exit -1
endif

if ( ! -e $env_vars ) then
 echo "'FULL_PATH_TO_ENV_VARS' does not exist.... exiting"
 exit -1
endif

source $env_vars
if ( ! -e $GSJOBDIR ) then
 echo "'GSJOBDIR' does not exist.... exiting"
 exit -1
endif
$MM5HOME/cycle_code/CSH_ARCHIVE/util/cycle_report.pl $GSJOBDIR $rangename $CHECK_COMPLETE
