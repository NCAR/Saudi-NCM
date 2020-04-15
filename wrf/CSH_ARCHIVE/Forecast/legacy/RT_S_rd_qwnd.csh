#!/bin/csh -f
##------------------------------------------------------------------------------
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- QWND decoder starts  ------------------------------"
echo "$0 $argv[*]"
echo  " ----------- RT_S_rd_qwnd.csh -----------------------------------------"
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
setenv SUBSYSTEM QWND_FDDA

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
 if (! -d $GEAPSTMP/RD_QWND) then
  $MustHaveDir $GEAPSTMP/RD_QWND
  ln -s $GEAPSTMP/RD_QWND $RUNDIR/${start_date}/RD_QWND
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_QWND
endif

# Testing only
if ($debug > 0) then 
set RUNDIR = /data/cycles/GM0004/ETA_NEWQ
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_QWND) mkdir $RUNDIR/${start_date}/RD_QWND
endif

#	Go to the directory


cd $RUNDIR/${start_date}/RD_QWND
echo "Now working in  $cwd"

#	Bring obs valid at obs_time and obs_time-1
if (-e ${QWND_DATA_DIR}/NPR.QWND.QS_$obs_date.decoded.gz) then
    echo "Found file ${QWND_DATA_DIR}/NPR.QWND.QS_$obs_date.decoded.gz"
    cp ${QWND_DATA_DIR}/NPR.QWND.QS_$obs_date.decoded.gz QWND.$obs_date.decoded.gz
    gunzip -f QWND.$obs_date.decoded.gz
else
    echo "Found file ${QWND_DATA_DIR}/NPR.QWND.QS_$obs_date.decoded"
    cp ${QWND_DATA_DIR}/NPR.QWND.QS_$obs_date.decoded QWND.$obs_date.decoded
endif

#	Put output where it is visible

if (! -e  QWND.$obs_date.decoded) then
    echo "Unable to find file ${QWND_DATA_DIR}/NPR.QWND.QS_$obs_date.decoded(.gz)"
endif
#	Clean up house

exit ( 0 ) 
