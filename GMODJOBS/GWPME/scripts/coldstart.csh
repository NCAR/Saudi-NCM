#!/bin/csh

###############################################################################
#
# Usage: ./coldstart.csh FULL_PATH_TO_ENV_VARS.CSH
#
#    FULL_PATH_TO_ENV_VARS should be 'GSJOBDIR'/scripts/env_vars.csh
#
#    Example:
#    To cold-start RTFDAA once a week, add the following to your crontab:
#
#    5 16 * * 6 'GSJOBDIR'/scripts/coldstart.csh full_path_to_env_vars.csh >& 'GSJOBDIR'/logs/zout_coldstart.log
#
################################################################################

set env_vars = $1
echo "coldstart.csh - using env_vars file: $env_vars"

if ( $env_vars == "" ) then
 echo "'FULL_PATH_TO_ENV_VARS' is not specified.... exiting"
 exit -1
endif

if ( ! -e $env_vars ) then
 echo "'FULL_PATH_TO_ENV_VARS' does not exist.... exiting"
 exit -1
endif

source $env_vars
$PERL_FLEX/coldstart_rtfdda3hcyc.pl $FLEXINPUT
