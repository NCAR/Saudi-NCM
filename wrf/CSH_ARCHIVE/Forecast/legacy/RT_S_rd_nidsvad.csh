#!/bin/csh -f
##------------------------------------------------------------------------------
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- NIDSVAD decoder starts  ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set debug = 0

if ( ${#argv} != 2 ) then
	echo "usage: $0 cycle_date obs_date"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM NIDSVAD_FDDA

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

# Testing only

if ($debug > 0) then 
    setenv MM5HOST GRM
endif

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif
source ${CFILE}user.mm5sys.${MM5HOST};    

set start_date = $1
set obs_date   = $2

#	Does the directory exist

if(-d $GEAPSTMP) then
 if (! -d $GEAPSTMP/RD_NIDSVAD) then
  $MustHaveDir $GEAPSTMP/RD_NIDSVAD
  ln -s $GEAPSTMP/RD_NIDSVAD $RUNDIR/${start_date}/RD_NIDSVAD
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_NIDSVAD
endif

# Testing only
if ($debug > 0) then 
set RUNDIR = /data/cycles/GM0004/ETA_NEWQ
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_NIDSVAD) mkdir $RUNDIR/${start_date}/RD_NIDSVAD
endif

#	Go to the directory


cd $RUNDIR/${start_date}/RD_NIDSVAD
echo "Now working in  $cwd"

#	Bring obs valid at obs_time and obs_time-1
cp ${NIDSVAD_DATA_DIR}/NIDSVAD.$obs_date.gz NIDSVAD.$obs_date.gz
gunzip -f NIDSVAD.$obs_date.gz

#	Put output where it is visible

#	Clean up house

exit ( 0 ) 
