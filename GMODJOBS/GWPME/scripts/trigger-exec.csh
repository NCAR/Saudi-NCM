#!/bin/csh
#
# This script is an example. 
# If you are running Moab and have reservations set up, then this 
# can be called by the trigger attached to your reservation.
# It will launch an FDDA cycle and allocate nodes within your reservation.
#
# submitCycleMM.csh needs to be executed by the user/goup account who is running 
# the job operationally. Change LOGNAME to your user/group account and change 
# the path to submitCycleMM.csh
#

su - LOGNAME -c "/raidN/LOGNAME/GMODJOBS/GSJOBID/scripts/submitCycleMM.csh -f full_path_envvars_file -r $1"
