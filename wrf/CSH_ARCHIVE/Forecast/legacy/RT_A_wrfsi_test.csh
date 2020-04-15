#!/bin/csh -f

# wrfsi cycling script for RTFDDA
# Yubao Liu, 2004-2005.2

set timestamp
setenv SUBSYSTEM  WRFJOB
setenv RM "rm -rf"

echo " ++++ ++++++++++++++++++++++++++++++++++++++++++ "
echo " ++++ start GMODJOBS $GSJOBDIR wrfsi +++++++++++ "
echo " ++++ ++++++++++++++++++++++++++++++++++++++++++ "

set debug = 0

# arguments check

set this_cycle  = 2005021505
set bcsst_date   = 2005021500
set FCST_LENGTH = 1440
set BCIC = ETA   #AVN, ETA, RUC, NNRP
set BC_INT = 10800
set RUNDIR_ROOT = /data/cycles

if ( ${#argv} < 7) then

echo "usage $0 this_cycle bcsst_date bcsen_date GMID NODE bcic bc_interval"
exit

endif

if ($1 != "") set this_cycle  = $1
if ($2 != "") set bcsst_date  = $2
if ($3 != "") set bcsen_date  = $3
if ($4 != "") set GMID        = $4
if ($5 != "") set NODE        = $5
if ($6 != "") set BCIC        = $6
if ($7 != "") set BC_INT      = $7
if ($8 != "") set RUNDIR_ROOT = $8

echo $1 $2 $3 $4 $5 $6

if($BCIC == "AVNFTP") then
 set BCIC="AVN"
endif

#
# ENVIRONMENT
#

if( $debug == 1) then
 setenv MM5HOST  DPG
 setenv RUNDIR  "/data/cycles/GWDPG/DPG"
endif

set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif

source ${CFILE}user.mm5sys.${MM5HOST};
source ${CFILE}sizes.mm5sys.${MM5HOST}

setenv INSTALLROOT "/data/fddahome/wrf/wrfsi"

set CYCDIR="$RUNDIR/$this_cycle"
$MustHaveDir $RUNDIR
$MustHaveDir $RUNDIR/data
$MustHaveDir $CYCDIR
cd $CYCDIR
rm -r WRFSI

if( -d /d1/WRFSI ) then
 setenv MOAD_DATAROOT "/d1/WRFSI"
 $MustHaveDir $MOAD_DATAROOT
 ln -s $MOAD_DATAROOT $CYCDIR/WRFSI
 ln -s $CYCDIR /d1/bridge
else
 setenv MOAD_DATAROOT "$CYCDIR/WRFSI"
 $MustHaveDir $MOAD_DATAROOT
endif

setenv EXT_DATAROOT "$MOAD_DATAROOT"

# 0: Prepare right namelists and eta data for all

cd $CYCDIR

set FH = 0
set tmp_date = $bcsst_date
echo $tmp_date $bcsst_date $bcsen_date $FH
while ($tmp_date < $bcsen_date)
 echo "$tmp_date , 1" >! input
 ${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
 set tmp_date = `cat output`
 @ FH = $FH + 1
 echo $tmp_date $FH
end

echo $bcsst_date $bcsen_date $FH
echo $CYCDIR $BCIC $BC_INT

set yyyy_start = `echo $bcsst_date | cut -b 1-4`
set mm_start = `echo $bcsst_date | cut -b 5-6`
set dd_start = `echo $bcsst_date | cut -b 7-8`
set hh_start = `echo $bcsst_date | cut -b 9-10`

set yyyy_end = `echo $bcsen_date | cut -b 1-4`
set mm_end = `echo $bcsen_date | cut -b 5-6`
set dd_end = `echo $bcsen_date | cut -b 7-8`
set hh_end = `echo $bcsen_date | cut -b 9-10`

cp $GSJOBDIR/namelists/wrfsi.nl.template wrfsi.nl
cp $GSJOBDIR/namelists/grib_prep.nl.template grib_prep.nl
echo $RUNDIR_ROOT

ed wrfsi.nl << EOF > /dev/null
g/SYY/s//$yyyy_start/
g/SMM/s//$mm_start/
g/SDD/s//$dd_start/
g/SHH/s//$hh_start/
g/EYY/s//$yyyy_end/
g/EMM/s//$mm_end/
g/EDD/s//$dd_end/
g/EHH/s//$hh_end/
g/BCIC/s//$BCIC/
g/ITT/s//$BC_INT/
g/RUNDIR_ROOT/s||$RUNDIR_ROOT|
g/GMID/s//$GMID/
g/NODE/s//$NODE/
g/THISCYC/s//$this_cycle/
w
q
EOF

ed grib_prep.nl << EOF > /dev/null
g/SYY/s//$yyyy_start/
g/SMM/s//$mm_start/
g/SDD/s//$dd_start/
g/SHH/s//$hh_start/
g/EYY/s//$yyyy_end/
g/EMM/s//$mm_end/
g/EDD/s//$dd_end/
g/EHH/s//$hh_end/
g/RUNDIR_ROOT/s||$RUNDIR_ROOT|
g/GMID/s//$GMID/
g/NODE/s//$NODE/
g/BCIC/s//$BCIC/
g/ITT/s//$BC_INT/
w
q
EOF


# 1. Run SI

rm -r $MOAD_DATAROOT/*
cp -r $GSJOBDIR/wrfsi/* $MOAD_DATAROOT/
cp $INSTALLROOT/extdata/static/* $MOAD_DATAROOT/static/
cp $CYCDIR/*nl $MOAD_DATAROOT/static/
mkdir $MOAD_DATAROOT/extprd

#$INSTALLROOT/etc/grib_prep.pl -g $BCIC -s $bcsst_date -l $FH -f
$INSTALLROOT/etc/grib_prep.pl -s $bcsst_date -l $FH $BCIC
$INSTALLROOT/etc/wrfprep.pl -s $bcsst_date -f $FH
###/data/yliu/wrf/wrfsi/etc/wrfsi.pl $bcsst_date $FH $BCIC

# WRFSI done : the real input is located in $MOAD_DATAROOT/siprd/

ls $MOAD_DATAROOT/siprd/wrf_real_input_*

echo $RUNDIR_ROOT
exit
