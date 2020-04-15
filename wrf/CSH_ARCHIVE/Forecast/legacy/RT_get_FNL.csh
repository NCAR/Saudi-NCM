#!/bin/csh 
#
#  This shell script extracts GRIB FNL data for a given time period.
#  See http://dss.ucar.edu/datasets/ds083.2/ For details. 
#
#  FNL files are expected in the input directory DATADIR (5th argument) and
#
#  CCYY/fnl_CCYYMMDD_HH_00.grib1 before 2007.12.06 (begin 1999.07.30)
#  CCYY/fnl_CCYYMMDD_HH_00.grib2 after  2007.12.06
#
#  Where CCYYMMDDHH is the analysis time (every 6 hours).
#
###############################################################################
echo
echo   " ----------------------------------------------------------------------"
echo   " --------------------- RT_get_fnl starts ----------------------------"
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
echo "Usage: RT_get_fnl.csh cycle_date startdate enddate itimint DATADIR"
echo "-----"
echo "With startdate/enddate =  CCYYMMDDHH"
echo "and  itimint is a multiple of 6."
echo "FNL data shall reside at DATADIR/FNL"
exit 1
endif

set cycle_date = $1 # 1998120102
set startdate  = $2 # 1998120100
set enddate    = $3 # 1998120200
set itimint    = $4 # 6
set DATADIR    = $5 # /raid/static/FNL

# Paths to input data 
# Expect data sorted by year, eg: DATADIR/FNL/CCYY/fn_CCYYMMDD_HH_00
set FNL_YEARLY = $DATADIR

#------------------------------------------------------------------------------#
# Go to work dir and clean

$MustHaveDir $RUNDIR/$cycle_date

if (-d $GEAPSTMP) then
   $MustHaveDir $GEAPSTMP/FNL_REGRID
   if (! -l $GEAPSTMP/FNL_REGRID) \
     ln -s $GEAPSTMP/FNL_REGRID  ${RUNDIR}/${cycle_date}/FNL_REGRID
else
   $MustHaveDir ${RUNDIR}/${cycle_date}/FNL_REGRID
endif

cd ${RUNDIR}/${cycle_date}/FNL_REGRID
echo "Now working in  $cwd"


#------------------------------------------------------------------------------#
# Loop from startdate to endate every itimint 

set curn =   `echo ${startdate} | cut -c 1-4,5-6,7-8,9-10`    # CCYYMMDDHH
set endn =   `echo ${enddate}   | cut -c 1-4,5-6,7-8,9-10`
set n = 0

while ( $curn <= $endn )

   set CCYY = `echo $curn | cut -b 1-4`
   set MM   = `echo $curn | cut -b 5-6`
   set DD   = `echo $curn | cut -b 7-8`
   set HH   = `echo $curn | cut -b 9-10`

       if ($CCYY >= 2009) then
          set filewant = "${FNL_YEARLY}/${CCYY}/fnl_${CCYY}${MM}${DD}_${HH}_00.grib2"
       else
          set filewant = "${FNL_YEARLY}/${CCYY}/fnl_${CCYY}${MM}${DD}_${HH}_00.grib1"
       endif


   if (! -e $filewant) then
       echo
       echo "ERROR: Cannot find file $filewant"
       exit -1
   else
           set fnl_file = 
       echo \
      "ln -sf  $filewant ."
       ln -sf  $filewant .
       @ n ++
   endif

   set curn = `$EXECUTABLE_ARCHIVE/geth_newdate.exe ${curn} ${itimint}`

end

echo "Found $n FNL files in ${FNL_YEARLY}:"
ls -1 fnl_*

cd $RUNDIR/$cycle_date
echo "Now working in  $cwd"

exit 0

