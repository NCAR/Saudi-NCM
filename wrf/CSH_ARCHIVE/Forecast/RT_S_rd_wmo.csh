#!/bin/csh -f
##------------------------------------------------------------------------------
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- WMO decoder starts  ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set debug = 0

if ( ${#argv} != 2 ) then
	echo "usage: $0 cycle_date obs_date"
	echo "where start_date is CCYYMMDDHH"
	echo "and obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM WMO_FDDA

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
 if (! -d $GEAPSTMP/RD_WMO) then
  $MustHaveDir $GEAPSTMP/RD_WMO
  ln -s $GEAPSTMP/RD_WMO $RUNDIR/${start_date}/RD_WMO
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_WMO
endif

# Testing only
if ($debug > 0) then 
set RUNDIR = /data/cycles/GM0004/ETA_NEWQ
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_WMO) mkdir $RUNDIR/${start_date}/RD_WMO
endif

#	Go to the directory


cd $RUNDIR/${start_date}/RD_WMO
echo "Now working in  $cwd"

echo
#	Bring obs valid at obs_time and obs_time-1
if (-e ${WMO_D_DATA_DIR}/wmo_$obs_date.decoded) then 
    echo "Found file ${WMO_D_DATA_DIR}/wmo_$obs_date.decoded"
    cp ${WMO_D_DATA_DIR}/wmo_$obs_date.decoded WMO.$obs_date.decoded
else
    if (-e ${WMO_D_DATA_DIR}/wmo_$obs_date.decoded.gz) then 
        echo "Found file ${WMO_D_DATA_DIR}/wmo_$obs_date.decoded.gz"
        cp ${WMO_D_DATA_DIR}/wmo_$obs_date.decoded.gz WMO.$obs_date.decoded.gz
        gunzip -f WMO.$obs_date.decoded.gz
    endif
endif

ls -alh

if (! -e  WMO.$obs_date.decoded) then 
    echo "Unable to find file ${WMO_D_DATA_DIR}/wmo_$obs_date.decoded(.gz)"
endif

#	Put output where it is visible

#	Clean up house

exit ( 0 ) 
