#!/bin/csh

###############################################################################
#
# Usage: check_cycle.csh FULL_PATH_TO_ENV_VARS.CSH RANGE title <OFFSET-in-hours>
#
#    FULL_PATH_TO_ENV_VARS should be 'GSJOBDIR'/scripts/env_vars.csh
#    RANGE  should be 'GRM' or ATEC-range-name (DPG, ATC, etc)
#    title is a string to be included in the email subject - otherwise, $RANGE is used
#    OFFSET is the hours-offset from the cycle-start time 
#
#    Example:
#    To check the status of a 3-hourly cycling RTFDDA, add the following to
#	your crontab:
#
#    30 0,3,6,9,12,15,18,21 * * * 'MM5HOME'/cycle_code/CSH_ARCHIVE/util/check_cycle.csh full_path_to_env_vars.csh DPG "GMDPG" 1>& 'GSJOBDIR'/logs/check_cycle.log
#
################################################################################

set env_vars = $1
set RANGE = $2
set title = $3
set offset = $4
echo "check_cycle.csh - using env_vars file: $env_vars"

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

$MM5HOME/cycle_code/CSH_ARCHIVE/util/check_cycle.pl $GSJOBDIR $RANGE "$title" $offset
