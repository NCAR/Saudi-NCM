#!/bin/csh -f 

#  This shell script extracts GRIB NNRP2 data for a given time period.
# $Id: RT_get_nnrp2.csh,v 1.14 2010/11/09 22:23:15 vandenb Exp $
###############################################################################
echo
echo   " ----------------------------------------------------------------------"
echo   " --------------------- RT_get_nnrp2 starts ----------------------------"
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
echo "Usage: RT_get_nnrp2.csh cycle_date startdate enddate itimint DATADIR"
echo "-----"
echo "With startdate/enddate =  CCYYMMDDHH"
echo "and  itimint is a multiple of 6."
echo "NNRP2 data shall reside at DATADIR/NNRP2"
exit 1
endif

set cycle_date = $1 # 1998120102
set startdate  = $2 # 1998120100
set enddate    = $3 # 1998120200
set itimint    = $4 # 6
set DATADIR    = $5 # /raid2/static/

# Paths to input data by year
set NNRP2_YEARLY = $DATADIR/NNRP2/isobaric/cfdda_input

#------------------------------------------------------------------------------#
# Go to work dir and clean

$MustHaveDir $RUNDIR/$cycle_date

if (-d $GEAPSTMP) then
   $MustHaveDir $GEAPSTMP/NNRP2_REGRID
   if (! -l $GEAPSTMP/NNRP2_REGRID) \
     ln -s $GEAPSTMP/NNRP2_REGRID  ${RUNDIR}/${cycle_date}/NNRP2_REGRID
else
   $MustHaveDir ${RUNDIR}/${cycle_date}/NNRP2_REGRID
endif

cd ${RUNDIR}/${cycle_date}/NNRP2_REGRID
echo "Now working in  $cwd"


#------------------------------------------------------------------------------#
# Former script fetch.csh:
# Set up the lists "mdate" and "sfcdate".  "mdate" will hold all the 
# upper-air times we need to get.  "sfcdate" will hold all the surface
# times we need to get.  "mdate" and "sfcdate" are not the same, because
# some of the surface analyses are actually 6-hour forecasts.

set curn = `echo ${startdate} | cut -c 1-4,5-6,7-8,9-10`    # CCYYMMDDHH
set endn =   `echo ${enddate} | cut -c 1-4,5-6,7-8,9-10`

set num = 0
set idth = -$itimint
set mdate = ( )
set sfcdate = ( )
while ( $curn < $endn )
   @ idth = $idth + $itimint
   @ num ++
   set ndate = `$EXECUTABLE_ARCHIVE/geth_newdate.exe ${startdate} ${idth}`
   set sdate = `$EXECUTABLE_ARCHIVE/geth_newdate.exe ${ndate} -6`
   set mdate = ( ${mdate} ${ndate} )
   set sfcdate = ( ${sfcdate} ${sdate} )
   set curn = `echo ${ndate} | cut -c 1-4,5-6,7-8,9-10`   # CCYYMMDDHH
end

umask
echo $USER

echo "Requested dates for NNRP2 upper-air fields: $mdate"
echo "Requested dates for NNRP2 surface fields:   $sfcdate"

# ========== Link the NNRP2 UPA data to the NNRP2_REGRID directory ==========
# == currently in the form of:
# ==   $DATADIR/NNRP2/isobaric/cfdda_input/CCYY/upa/pgb.anl.CCYYMMDDHH.grib
# ==

foreach  ndate ( $mdate )

   set CCYY = `echo $ndate | cut -b 1-4`
   set CCYYMMDD = `echo $ndate | cut -b 1-4,5-6,7-8`
   set CCYYMMDDHH = `echo $ndate | cut -b 1-4,5-6,7-8,9-10`
   
   set flwant = ${NNRP2_YEARLY}/${CCYY}'/upa/pgb.anl.'${CCYYMMDDHH}'.grib'
   
   sync
   echo \
  "scp ${flwant}  ${RUNDIR}/${cycle_date}/NNRP2_REGRID/UPANNRP_`basename ${flwant}`"
   scp ${flwant}  ${RUNDIR}/${cycle_date}/NNRP2_REGRID/UPANNRP_`basename ${flwant}`
  
end

# ========== Link the NNRP2 SFC data to the NNRP2_REGRID directory ==========
# == currently in the form of:
# ==   $DATADIR/NNRP2/isobaric/cfdda_input/CCYY/sfc/flx.ft06.CCYYMMDDHH.grib
# ==

foreach ndate ( $sfcdate )

   set CCYY = `echo $ndate | cut -b 1-4` 
   set CCYYMMDD = `echo $ndate | cut -b 1-4,5-6,7-8`
   set CCYYMMDDHH = `echo $ndate | cut -b 1-4,5-6,7-8,9-10`
   set sdate = `$EXECUTABLE_ARCHIVE/geth_newdate.exe ${ndate} 6`
   
   set flwant = ${NNRP2_YEARLY}/${CCYY}'/sfc/flx.ft06.'${CCYYMMDDHH}'.grib'
   
   sync
   echo \
  "scp ${flwant}  ${RUNDIR}/${cycle_date}/NNRP2_REGRID/SFCNNRP_`basename ${flwant}`"
   scp ${flwant}  ${RUNDIR}/${cycle_date}/NNRP2_REGRID/SFCNNRP_`basename ${flwant}`

end

cd $RUNDIR/$cycle_date
echo "Now working in  $cwd"


exit (0)
