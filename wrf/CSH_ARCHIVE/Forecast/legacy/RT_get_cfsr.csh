#!/bin/csh -f 

# /raid2/fddasys/gcat_wrf_cfsr/gcat_code/cycle_code/CSH_ARCHIVE/Forecast/RT_get_cfsr.csh
#  This shell script extracts GRIB CFSV1 data for a given time period.
###############################################################################
echo
echo   " ----------------------------------------------------------------------"
echo   " --------------------- RT_get_cfsr starts ----------------------------"
echo "$0 $argv[*]"
echo   " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM REGRID
setenv RM "rm -rf"

#
# ENVIRONMENT
#
#set CFILE="$CONFIGURE_FILES/cshrc_"
set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

#$UTILS/CheckConfigFiles.csh
$CheckConfigFiles

set cfstat = $status
if ( $cfstat != 0 ) then
 echo " ${SUBSYSTEM} --  Missing ConfigFile -> exiting"
  exit (2)
endif

source ${CFILE}user.mm5sys.${MM5HOST}
source ${CFILE}sizes.mm5sys.${MM5HOST}

#------------------------------------------------------------------------------#
# Necessary arguments

if ($#argv != 5) then
echo ""
echo "Usage: RT_get_cfs.csh cycle_date startdate enddate itimint DATADIR"
echo "-----"
echo "With startdate/enddate =  CCYYMMDDHH"
echo "and  itimint is a multiple of 6."
echo "CFS data shall reside at DATAIN/CFSR/data/CCYY/MM"
exit 1
endif

set cycle_date = $1 # 1998120102
set startdate  = $2 # 1998120100
set enddate    = $3 # 1998120200
set ITT        = $4 # 6
set DATAIN     = $5 # /raid1/static/

# Paths to CFS data
#set CFSIN = $DATAIN/CFSR/data

# subdirectory under work directory to hold necessary CFS data for WPS ungrib
set CFSDAT = ${RUNDIR}/"data/CFSR" 
if (! -d $CFSDAT) then
   mkdir -p $CFSDAT 
endif 
# clean the subdirectory 
rm -f $CFSDAT/*

cd $CFSDAT
echo "Now working in  $cwd"

# WPSV3.4.1 or earlier versions have a bug putting CFS Surface forecast data 
# at it initial time not forecast time. WPSV3.5 or later fixed it.
# for time being on NGIC-C2 running WPSV3.2, an extra slice of CFS data is required
# due to the WPS bug. So we require the extra slice by add '$ITT' to ensure
# ungrib has all required data. THis is what '$edate' does here. 
#It won't affect WPSV3.5 or later.
# Wanli Wu (wanliwu@ucar.edu) Dec. 2 2013
set edate = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $enddate +$ITT`
set CCYYMMDDHH = $startdate

# Loop over time levels that required by given cycle and forecasting length
while ($CCYYMMDDHH <= $edate)
#while ($CCYYMMDDHH <= $enddate)
  set ccyy     = `echo $CCYYMMDDHH |cut -c1-4`
  set mm   = `echo $CCYYMMDDHH |cut -c5-6`
  set CFSIN = $DATAIN/CFSR/data/$ccyy/$mm 
  echo "CFSIN: $CFSIN"

# copy or soft link CFS data from $CFSIN to WRF WPS working directory 
  foreach product (upr sfc)
    set fn = cfs.${product}.${CCYYMMDDHH}.grb2
    if (! -e $fn) then 
#      cp $CFSIN/$fn .
       ln -sf $CFSIN/$fn .
       echo "Processing CFSR $product valid for ${CCYYMMDDHH}"
       echo
    endif 
  end 

  set CCYYMMDDHH = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $CCYYMMDDHH +$ITT`

end #CCYYMMDDHH
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd $RUNDIR/$cycle_date
echo "Now working in  $cwd"

exit (0)
