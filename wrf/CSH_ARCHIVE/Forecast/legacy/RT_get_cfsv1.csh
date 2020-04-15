#!/bin/csh -f 

#  This shell script extracts GRIB CFSV1 data for a given time period.
###############################################################################
echo
echo   " ----------------------------------------------------------------------"
echo   " --------------------- RT_get_cfsv1 starts ----------------------------"
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
# NCAR RDA id for CFSV1

set DSSID = ds093.0

#------------------------------------------------------------------------------#
# Necessary arguments

if ($#argv != 5) then
echo ""
echo "Usage: RT_get_cfsv1.csh cycle_date startdate enddate itimint DATADIR"
echo "-----"
echo "With startdate/enddate =  CCYYMMDDHH"
echo "and  itimint is a multiple of 6."
echo "CFSV1 data shall reside at DATAIN/$DSSID" 
exit 1
endif

set cycle_date = $1 # 1998120102
set startdate  = $2 # 1998120100
set enddate    = $3 # 1998120200
set ITT        = $4 # 6
set DATAIN     = $5 # /raid1/static/

# Paths to CFSV1 data
set CFSIN = $DATAIN/CFSR/data/$DSSID

# subdirectory under work directory to hold untar-ed CFS data for WPS ungrib
set CFSDAT = ${RUNDIR}/"data/CFSV1" 
if (! -d $CFSDAT) then
   mkdir -p $CFSDAT 
endif 

cd $CFSDAT
echo "Now working in  $cwd"

# this part of code is taken from cfs_ungridmetgrid.csh with minor modification
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Loop over surface and upper air products

foreach product (flxf06 pgbhnl)

# Must begin six hour earlier, as CFSV1 surface data are forecast
  set CCYYMMDDHH = `$EXECUTABLE_ARCHIVE/geth_newdate.exe  $startdate -6`
  set icount = 0
  set tf = 0

# Loop over time levels
  while ($CCYYMMDDHH <= $enddate)

# Name of the input file
    set fn = ${product}.gdas.${CCYYMMDDHH}.grb2

    if (! -e $fn) then 

       set ccyymmdd = `echo $CCYYMMDDHH |cut -c1-8`
       set ccyymm   = `echo $CCYYMMDDHH |cut -c1-6`
       set ccyy     = `echo $CCYYMMDDHH |cut -c1-4`
       set dd       = `echo $CCYYMMDDHH |cut -c7-8`

       echo
       echo \
"------------------------------------------------------------------------------"
       echo "Processing CFSV1 analysis $product valid for $ccyymmdd" 
       echo

#------------------------------------------------------------------------------
# cfsv1 files are tared in 5 days increment for each month
# We use two different tables for surface and upper air, so they must 
# processed separately
#------------------------------------------------------------------------------

      set c1 = `echo $dd |cut -c1`
      set c2 = `echo $dd |cut -c2`
      if ($c1 == "0") then
         @ q = $c2 / 5 
      else
         @ q = $dd / 5 
      endif 

      @ d = $q * 5

      if ($d == 30) then
         set d = 25
      endif

      @ d = $d + 1

      set tar_day = `printf '%02i' $d`

      echo \
      "tar xvf $CFSIN/${ccyy}/$product.gdas.${ccyymm}${tar_day}-*.tar"
       tar xvf $CFSIN/${ccyy}/$product.gdas.${ccyymm}${tar_day}-*.tar

      endif

      set CCYYMMDDHH = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $CCYYMMDDHH +$ITT`
      @ tf = $tf + $ITT

    end #CCYYMMDDHH

end # product
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cd $RUNDIR/$cycle_date
echo "Now working in  $cwd"

exit (0)
