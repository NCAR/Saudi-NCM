#!/bin/csh -f
##------------------------------------------------------------------------------
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- MADIS decoder starts  ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set debug = 0

if ( ${#argv} != 2 ) then
	echo "usage: $0 cycle_date obs_date"
	echo "where cycle_date is CCYYMMDDHH"
	echo "and obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM MADIS_FDDA

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
 if (! -d $GEAPSTMP/RD_MADIS) then
  $MustHaveDir $GEAPSTMP/RD_MADIS
  ln -s $GEAPSTMP/RD_MADIS $RUNDIR/${start_date}/RD_MADIS
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_MADIS
endif

# Testing only
if ($debug > 0) then 
set RUNDIR = /data/cycles/GM0004/ETA_NEWQ
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_MADIS) mkdir $RUNDIR/${start_date}/RD_MADIS
endif

#	Go to the directory


cd $RUNDIR/${start_date}/RD_MADIS
echo "Now working in  $cwd"

echo

set obs_date_before = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $obs_date -1`

foreach madis_date ($obs_date_before $obs_date)

#	Bring obs valid at obs_time and obs_time-1
if (-e ${MADIS_DATA_DIR}/madis_$madis_date.decoded) then
    echo "Found file ${MADIS_DATA_DIR}/madis_$madis_date.decoded"
    cp ${MADIS_DATA_DIR}/madis_$madis_date.decoded MADIS.$madis_date.decoded
else
    if (-e ${MADIS_DATA_DIR}/madis_$madis_date.decoded.gz) then
         echo "Found file ${MADIS_DATA_DIR}/madis_$madis_date.decoded.gz"
         cp ${MADIS_DATA_DIR}/madis_$madis_date.decoded.gz MADIS.$madis_date.decoded.gz
         gunzip -f MADIS.$madis_date.decoded.gz
    endif
endif

if (! -e MADIS.$madis_date.decoded) then
    echo "Unable to find file ${MADIS_DATA_DIR}/madis_$madis_date.decoded(.gz)"
endif

end


#	Put output where it is visible

#	Clean up house

exit ( 0 ) 
