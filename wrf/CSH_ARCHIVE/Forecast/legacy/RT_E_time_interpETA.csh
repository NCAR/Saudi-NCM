#!/bin/tcsh -f

# This script decodes the GRIB ETA AWIP 212 data and regrids it to match MM5 DOMAIN1 data.
###############################################################################
echo
echo  "----------------------------------------------------------------------"
echo  " ----- Regrid to get hourly ETA, AVN or NNRP 1st guesses  ------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM REGRID
setenv RM "rm -rf"
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
source ${CFILE}user.mm5sys.${MM5HOST}
source ${CFILE}sizes.mm5sys.${MM5HOST}

#	Check usage

if      ( ${#argv} < 4 )  then
	echo "usage: $0 this_cycle start_date end_date ptop"
	echo "where start_date and end_date are time window for the 1st guess, and"
	echo "this_cycle is the time when this_cycle start"
	echo "ptop is the model top in hPa"
	echo "where all are given in CCYYMMDDHH"
	exit ( 1 ) 
endif

set this_cycle = $1
set start_date = $2
set end_date   = $3
set ptop       = ${4}00
if (${#argv} == 5) then
  set InterpType = $5
endif


setenv  T_INTERP_ETA  ${RUNDIR}/${this_cycle}/ETA_1GUESS

if (-d $GEAPSTMP) then
  $MustHaveDir $GEAPSTMP/ETA_1GUESS
  ln -s $GEAPSTMP/ETA_1GUESS  $T_INTERP_ETA
else
  $MustHaveDir $T_INTERP_ETA
endif

cd $T_INTERP_ETA
echo "Now working in  $cwd"


#
#       Look for the pregridded eta data in ETA-REGRID directory.
#

mv ../ETA_REGRID/FILE* .    #ETA
if ($status == 1) echo "No ETA_REGRID/FILE* files found"
mv ../NNRP_REGRID/FILE* .   #NN
if ($status == 1) echo "No NNRP_REGRID/FILE* files found"
mv ../NNRP2_REGRID/FILE* .  #NNRP2
if ($status == 1) echo "No NNRP2_REGRID/FILE* files found"
mv ../AVN_REGRID/MOSAIC* .  #AVN
if ($status == 1) echo "No AVN_REGRID/MOSAIC* files found"
mv ../AVN_REGRID/FILE* .    #AVN
if ($status == 1) echo "No AVN_REGRID/FILE* files found"
mv ../AVN_REGRID/SSTFILE .  #AVN
if ($status == 1) echo "No AVN_REGRID/SSTFILE file found"

#	Where we want the data to go, and just head over there.


#	The TERRAIN file has the record header stuff,
#	plus all of the 2d constant fields (latitude, longitude, etc.).

if ( -e $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN1 ) then
 ln -s $GSJOBDIR/TERRAIN/TERRAIN_DOMAIN1 TERRAIN
 echo "${SUBSYSTEM}  Using Terrain in $GSJOBDIR "
else if ( -e  $TERRAIN_DIR/Domain1_New_LU.V.${MM5HOST} ) then
 ln -s $TERRAIN_DIR/Domain1_New_LU.V.${MM5HOST} TERRAIN
 echo "${SUBSYSTEM}  Using Terrain in $TERRAIN_DIR"
else
 echo "${SUBSYSTEM}  Missing file -> NO terrain in $GSJOBDIR or $TERRAIN_DIR"
 exit (2)
endif

#
# Build the namelists.
#

set yyyy_start = `echo $start_date | cut -b 1-4`
set mm_start = `echo $start_date | cut -b 5-6`
set dd_start = `echo $start_date | cut -b 7-8`
set hh_start = `echo $start_date | cut -b 9-10`
set yyyy_end = `echo $end_date | cut -b 1-4`
set mm_end = `echo $end_date | cut -b 5-6`
set dd_end = `echo $end_date | cut -b 7-8`
set hh_end = `echo $end_date | cut -b 9-10`

# 1 Hour Output is Default
set time_int = 3600
if ($InterpType == 2) set time_int = 21600

cp ${REGRIDDER_NML_TEMPL} namelist.input
ed namelist.input << EOF > /dev/null
g/_YYYY_START/s//$yyyy_start/
g/_MM_START/s//$mm_start/
g/_DD_START/s//$dd_start/
g/_HH_START/s//$hh_start/
g/_YYYY_END/s//$yyyy_end/
g/_MM_END/s//$mm_end/
g/_DD_END/s//$dd_end/
g/_HH_END/s//$hh_end/
g/_INT_SEC/s//$time_int/
g/_TERRAIN_FILE/s//TERRAIN/
g/_PTOP/s//$ptop/
w
q
EOF

#
# Get the Vtable:
#

echo "Running  ${REGRIDDER_EXE}"
(time ${REGRIDDER_EXE} ) >> print.out

#	The important files are the REGRID_DOMAIN1

##mv REGRID_DOMAIN1 ../${this_cycle}_1STGUESSETA_DOMAIN1.${MM5HOST}
mv REGRID_DOMAIN1 $RUNDIR/$this_cycle/${this_cycle}_1STGUESSMM5_DOMAIN1.${MM5HOST}
mv print.out $RUNDIR/$this_cycle/${this_cycle}_1stguessETA_print.out.${MM5HOST}

find . -name TERRAIN -type l -exec rm {} \;
# Clean up:
exit ( 0 )
