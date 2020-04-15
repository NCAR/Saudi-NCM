#!/bin/csh -f
#------------------------------------------------------------------------------#
#
# Bring AFCCC decoded data files to the local disk. 
# 
# It assumes that AFCCC data have already been decoded into hourly data
# file at little_r input format and save at $datadir/AFCCC_decoded, 
# where datadir is the second argument in the command line calling this script.
# The first argument being the MM5 RT-FFDA cycle time. The AFCCC_decoded
# directory structure must be:
#
# AFCCC_decoded/CCYY/afccc_CCYYMMDDHH.decoded
#
# Usage: RT_S_decode_AFCCC_csh cycle_data datadir     
# -----
# Where: cycle_date is the MM5 RT-FDDA cycle time (CCYYMMDDHH)
#        datadir is the path to the AFCCC directory where AFCCC data 
#        are stored.
#
# Data file valid 13hr before and 1hr after cycle_time will be copied to
# the local data directory. The "geth_newdate.exe" and "MustHaveDir" 
# functions in EXECUTABLE_ARCHIVE are used.
#
#------------------------------------------------------------------------------!
# Copyright UCAR (c) 2007.
# University Corporation for Atmospheric Research (UCAR),
# National Center for Atmospheric Research (NCAR),
# Research Applications Program (RAP),
# P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
#
# Francois Vandenberghe, vandenb@ucar.edu, September 2007.
#------------------------------------------------------------------------------#
echo
echo " ----------------------------------------------------------------------"
echo " ---------------- Bring decoded AFCCC observations --------------------" 
echo "$0 $argv[*]"
echo " ----------------------------------------------------------------------"

#set echo
set timestamp
setenv SUBSYSTEM AFCCC
setenv RM "rm -rf"

#
# ENVIRONMENT
#
set CFILE="$CONFIGURE_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile  ${CFILE}user.mm5sys.${MM5HOST} -> exiting"
 exit (4)
endif
source ${CFILE}user.mm5sys.${MM5HOST};    


#------------------------------------------------------------------------------#
# Parse Arguments
#------------------------------------------------------------------------------#

if ( ${#argv} != 2 ) then
	echo "usage: ${0}:"
        echo "$0 cycle_date datadir"
        echo "Where: cycle_date must be YYYYMMDDHH."
        echo "       datadir is the path to raw input data files."
        exit ( 1 )
endif

if ($1 < 1900000000 || $1 > 2100000000) then
        echo "ERROR: Bad cycle time $1" 
	exit ( 2 )
endif

#if (! -d $2) then
#       echo "ERROR: directory $2 is missing"
#endif

set cycle_date = $1
set DATADIR = $2

#------------------------------------------------------------------------------#
# Create a workdir on local disk and link to main run directory
#------------------------------------------------------------------------------#

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/RD_AFCCC
if (! -e $RUNDIR/${cycle_date}/RD_AFCCC) \
ln -s $GEAPSTMP/RD_AFCCC $RUNDIR/${cycle_date}/RD_AFCCC
else
$MustHaveDir $RUNDIR/$cycle_date/RD_AFCCC
endif

cd $RUNDIR/$cycle_date/RD_AFCCC
echo "Now working in  $cwd"


#------------------------------------------------------------------------------#
# Find initial and final time: [cycle_time-CYC_INT-1rh, cycle_time+1hr]
#------------------------------------------------------------------------------#

if ($CYC_INT > 12) then
    @ hour = -$CYC_INT - 1
else
    @ hour = -13
endif

set i = 1

while ($hour <= 1)
#foreach hour ( -13 1 )

   if ($i == 1) then
      set start_date = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $cycle_date $hour`
   else if ($i == 2) then
      set end_date = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $cycle_date $hour`
   else
      @ hour ++
      continue
   endif

   @ i ++

   @ hour ++
end


if ( $start_date > $end_date ) then
        echo "Look for AFCCC observations between $start_date and $end_date"
	echo "but starting_date must be <= ending_date"
        exit ( 8 )
endif

if ( ( $start_date < 1900000000 ) || ( $end_date < 1900000000 ) ) then
        echo "Look for AFCCC observations between $start_date and $end_date"
        echo "but dates are out of ranges"
	exit ( 9 )
endif

if ( ( $start_date > 2100000000 ) || ( $end_date > 2100000000 ) ) then
        echo "Look for AFCCC observations between $start_date and $end_date"
        echo "but dates are out of ranges"
	exit ( 10 )
endif

#------------------------------------------------------------------------------#
# Bring the data
#------------------------------------------------------------------------------#

set ccyymmddhh = $start_date
set n = 0

while ($ccyymmddhh <= $end_date)

       set ccyy = `echo $ccyymmddhh | cut -c -4`

       if (-e $DATADIR/AFCCC_decoded/${ccyy}/afccc_${ccyymmddhh}.decoded) then
       echo \
      "cp -f $DATADIR/AFCCC_decoded/${ccyy}/afccc_${ccyymmddhh}.decoded ."
       cp -f $DATADIR/AFCCC_decoded/${ccyy}/afccc_${ccyymmddhh}.decoded .
       else 
       echo \
       "File $DATADIR/AFCCC_decoded/${ccyy}/afccc_${ccyymmddhh}.decoded is missing"
       endif
       
       if (! -e afccc_${ccyymmddhh}.decoded) then
           echo "WARNING: Cannot find file $DATADIR/AFCCC_decoded/${ccyy}/afccc_${ccyymmddhh}.decoded"
       else
           @ n ++
       endif

      set ccyymmddhh = `$EXECUTABLE_ARCHIVE/geth_newdate.exe $ccyymmddhh +1`

end

#------------------------------------------------------------------------------#
# End
#------------------------------------------------------------------------------#

if ($n > 0) then
    echo
    echo "Found AFCCC decoded files:"
    ls -1 
    exit 0;
else
    echo
    echo "Could not find valid AFCCC obs files in $DATADIR/AFCCC_decoded/${ccyy}"
    exit -1;
endif
