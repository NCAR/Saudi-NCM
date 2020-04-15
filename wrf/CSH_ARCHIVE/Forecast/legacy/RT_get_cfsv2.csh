#!/bin/csh -f 

#  This shell script extracts GRIB CFSV2 data for a given time period.
###############################################################################
echo
echo   " ----------------------------------------------------------------------"
echo   " --------------------- RT_get_cfsv2 starts ----------------------------"
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
# NCAR RDA id for CFSV2

set DSSID = ds094.0

#------------------------------------------------------------------------------#
# Necessary arguments

if ($#argv != 5) then
echo ""
echo "Usage: RT_get_cfsv2.csh cycle_date startdate enddate ITT DATAIN"
echo "-----"
echo "With startdate/enddate =  CCYYMMDDHH"
echo "and  itimint is a multiple of 6."
echo "CFSV2 data shall reside at DATAIN/$DSSID" 
exit 1
endif

set cycle_date = $1 # 1998120102
set startdate  = $2 # 1998120100
set enddate    = $3 # 1998120200
set ITT        = $4 # 6
set DATAIN     = $5 # /raid1/static/

# Paths to CFSV2 input data files
set CFSIN = $DATAIN/CFSR/data/$DSSID

# subdirectory under work directory to hold untar-ed CFS data for WPS ungrib
set CFSDAT = ${RUNDIR}/"data/CFSV2"
if (! -d $CFSDAT) then
   mkdir -p $CFSDAT 
endif 

cd $CFSDAT
echo "Now working in  $cwd"

# this part of code is taken from cfs_ungridmetgrid.csh with minor modification
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Loop over surface and upper air products

foreach product (sfluxgrbf pgrbh)

# Must begin six hour earlier, as CFSV2 surface data are forecast
  set CCYYMMDDHH = `$EXECUTABLE_ARCHIVE/geth_newdate.exe  $startdate -6`
  set icount = 0
  set tf = 0

# Loop over time levels
  while ($CCYYMMDDHH <= $enddate)

# Name of the input file
    set fn = cdas1.${product}.${CCYYMMDDHH}.grb2

    if (! -e $fn) then 

       set ccyymmdd = `echo $CCYYMMDDHH |cut -c1-8`
       set ccyymm   = `echo $CCYYMMDDHH |cut -c1-6`
       set ccyy     = `echo $CCYYMMDDHH |cut -c1-4`
       set dd       = `echo $CCYYMMDDHH |cut -c7-8`

       echo
       echo \
"------------------------------------------------------------------------------"
       echo "Processing CFSV2 analysis $product valid for $ccyymmdd" 
       echo

       if ($product == sfluxgrbf) then
          tar xvf $CFSIN/$ccyy/cdas1.$ccyymmdd.$product.tar  \
          cdas1.t00z.${product}06.grib2  cdas1.t06z.${product}06.grib2 \
          cdas1.t12z.${product}06.grib2  cdas1.t18z.${product}06.grib2
          mv -f cdas1.t00z.${product}06.grib2 cdas1.${product}.${ccyymmdd}00.grb2
          mv -f cdas1.t06z.${product}06.grib2 cdas1.${product}.${ccyymmdd}06.grb2
          mv -f cdas1.t12z.${product}06.grib2 cdas1.${product}.${ccyymmdd}12.grb2
          mv -f cdas1.t18z.${product}06.grib2 cdas1.${product}.${ccyymmdd}18.grb2
       else if ($product == pgrbh) then
          tar xvf $CFSIN/$ccyy/cdas1.$ccyymmdd.$product.tar  \
          cdas1.t00z.${product}anl.grib2  cdas1.t06z.${product}anl.grib2 \
          cdas1.t12z.${product}anl.grib2  cdas1.t18z.${product}anl.grib2
          mv -f cdas1.t00z.${product}anl.grib2  cdas1.${product}.${ccyymmdd}00.grb2
          mv -f cdas1.t06z.${product}anl.grib2  cdas1.${product}.${ccyymmdd}06.grb2
          mv -f cdas1.t12z.${product}anl.grib2  cdas1.${product}.${ccyymmdd}12.grb2
          mv -f cdas1.t18z.${product}anl.grib2  cdas1.${product}.${ccyymmdd}18.grb2
       endif # sfluxgrbf

      endif

      set CCYYMMDDHH = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $CCYYMMDDHH +$ITT`
      @ tf = $tf + $ITT

    end #CCYYMMDDHH

end # product
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd $RUNDIR/$cycle_date
echo "Now working in  $cwd"

exit (0)
