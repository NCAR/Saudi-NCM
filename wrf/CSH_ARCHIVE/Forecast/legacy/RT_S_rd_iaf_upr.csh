#!/bin/csh -f
##------------------------------------------------------------------------------
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " -------------- IAF upperair obs reader begins ------------------------"
echo  " --------------- Reads upperair little_r obs --------------------------"
echo  " ------------ Obs were converted from CSV data ------------------------"
echo  " -- This script was used during OPA_DV and OPA_HR case studies only ---"
echo  "$0 $argv[*]"
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
setenv SUBSYSTEM IAF_UPR_FDDA

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
 if (! -d $GEAPSTMP/RD_IAFUPR) then
  $MustHaveDir $GEAPSTMP/RD_IAFUPR
  ln -s $GEAPSTMP/RD_IAFUPR $RUNDIR/${start_date}/RD_IAFUPR
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_IAFUPR
endif

#	Go to the directory


cd $RUNDIR/${start_date}/RD_IAFUPR
echo "Now working in  $cwd"

echo

set obs_date_before = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $obs_date -1`

foreach iafupr_date ($obs_date_before $obs_date)

#	Bring obs valid at obs_time and obs_time-1
if (-e ${IAF_UPR_DIR}/iaf_uppair_obs_$iafupr_date.decoded) then
    echo "Found file ${IAF_UPR_DIR}/iaf_uppair_obs_$iafupr_date.decoded"
    cp ${IAF_UPR_DIR}/iaf_uppair_obs_$iafupr_date.decoded IAFUPR.$iafupr_date.decoded
else
    if (-e ${IAF_UPR_DIR}/iaf_uppair_obs_$iafupr_date.decoded.gz) then
         echo "Found file ${IAF_UPR_DIR}/iaf_uppair_obs_$iafupr_date.decoded.gz"
         cp ${IAF_UPR_DIR}/iaf_uppair_obs_$iafupr_date.decoded.gz IAFUPR.$iafupr_date.decoded.gz
         gunzip -f IAFUPR.$iafupr_date.decoded.gz
    endif
endif

if (! -e IAFUPR.$iafupr_date.decoded) then
    echo "Unable to find file ${IAF_UPR_DIR}/iaf_uppair_obs_$iafupr_date.decoded(.gz)"
endif

end


#	Put output where it is visible

#	Clean up house

exit ( 0 ) 
