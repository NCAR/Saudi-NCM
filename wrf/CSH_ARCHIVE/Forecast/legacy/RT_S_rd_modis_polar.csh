#!/bin/csh -f
##------------------------------------------------------------------------------
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------

###############################################################################
echo  ""
echo  " ----------------------------------------------------------------------"
echo  " ---------------- MODIS_POLAR decoder starts  -------------------------"
echo  "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

if ( ${#argv} != 2 ) then
	echo "usage: $0 cycle_date obs_date"
	echo "where start_date is CCYYMMDDHH"
	echo "and obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM MODIS_POLAR

#
# ENVIRONMENT
#
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

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
 if (! -d $GEAPSTMP/RD_MODIS_POLAR) then
  $MustHaveDir $GEAPSTMP/RD_MODIS_POLAR
  ln -s $GEAPSTMP/RD_MODIS_POLAR $RUNDIR/${start_date}/RD_MODIS_POLAR
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_MODIS_POLAR
endif

#	Go to the directory


cd $RUNDIR/${start_date}/RD_MODIS_POLAR

echo
#	Bring obs valid at obs_time and obs_time-1

foreach sat (aqua terra)

  if (-e ${MODIS_POLAR_DATA_DIR}/MODIS_POLAR_${sat}_${obs_date}.decoded) then
      echo "Found file ${MODIS_POLAR_DATA_DIR}/MODIS_POLAR_${sat}_${obs}_date.decoded"
      cp ${MODIS_POLAR_DATA_DIR}/MODIS_POLAR_${sat}_${obs_date}.decoded MODIS_POLAR_${sat}.${obs_date}.decoded
  else

      if (-e ${MODIS_POLAR_DATA_DIR}/MODIS_POLAR_${sat}_${obs_date}.decoded.gz) then
          echo "Found file ${MODIS_POLAR_DATA_DIR}/MODIS_POLAR_${sat}_${obs_date}.decoded.gz"
          cp ${MODIS_POLAR_DATA_DIR}/MODIS_POLAR_${sat}_${obs_date}.decoded.gz MODIS_POLAR_${sat}.${obs_date}.decoded.gz
          gunzip -f MODIS_POLAR${sat}.${obs_date}.decoded.gz
      endif

  endif

  if (! -e MODIS_POLAR_${sat}.${obs_date}.decoded) then
      echo "Unable to find file ${MODIS_POLAR_DATA_DIR}/MODIS_POLAR_${sat}_${obs_date}.decoded(.gz)"
  endif

end

#	Put output where it is visible

#	Clean up house

exit ( 0 ) 
