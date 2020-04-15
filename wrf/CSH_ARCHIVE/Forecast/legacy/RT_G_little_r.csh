#!/bin/csh -f

###############################################################################
echo 
echo  "----------------------------------------------------------------------"
echo  " --CLIMO ------- Little_r reanalysis for BC and IC starts ------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

# set echo
set timestamp
setenv SUBSYSTEM RAP
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
source ${CFILE}user.mm5sys.${MM5HOST};
source ${CFILE}sizes.mm5sys.${MM5HOST}

if ( ${#argv} < 5 ) then
	echo "ERROR (usage): $0 eta_date start_date end_date cycle_date"
	echo "where eta_date is initial time of the ETA forecast, and"
	echo "start_date is the initial time of the MM5 forecast"
	echo "end_date is the final time of the MM5 forecast"
	echo "all are in CCYYMMDDHH) "
	exit ( 1 )
endif

set eta_date   = $1
set start_date = $2
set end_date   = $3
set cycle_date   = $4
set cold_start   = $5

set JobType  = 0   # Standard operational run
if ( ${#argv} == 6 ) then
  set JobType = $6
endif


#	Get to the right directory

if(-d $GEAPSTMP) then
$MustHaveDir $GEAPSTMP/RAP
ln -s $GEAPSTMP/RAP $RUNDIR/${cycle_date}/RAP
else
$MustHaveDir $RUNDIR/$cycle_date/RAP
endif

cd $RUNDIR/$cycle_date/RAP
echo "Now working in  $cwd"


#	Bring stuff over that we need

if ( ! -e ${RAP_TEMPLATE} ) then
 echo "${SUBSYSTEM} -- Missing Template ${RAP_TEMPLATE} -> exiting"
 exit (3)
endif

cp ${LITTLE_R_TEMPLATE} namelist.input

# Remove old  obs file
if ( -e all.obs) rm all.obs

# Include Obs from BLP,NPN,CLASS,SAMS,PROF,GTS,RAWS,OKMESO,MADIS,QCOUT,SPECIAL,ADP  

cat ../DECODE_BLP_PROF/PROF_* ../DECODE_NPN_PROF/PROF_* ../DECODE_CLASS/CLASS*all ../DECODE_SAMS/*SAMS.allrange ../DECODE_PROF/PROF_QC* ../DECODE_GTS/*_GTS_data.${MM5HOST} ../RD_RAWS/*_RAWS_data.${MM5HOST} ../RD_OKMESO/*_OKMESO_data.${MM5HOST} ../RD_MADIS/MADIS.* >! all.obs

cat ../RD_QCOUT/QCOUT* >> all.obs
cat ../RD_SPECIAL/SPECIAL* >> all.obs
cat ../RD_ADP/*_ADP_data.${MM5HOST} >> all.obs

## Include any special obs in all.obs format
cat /data/inputspecial/* >> all.obs

## AH, make latlon file to trim down observations
if (-e latlon.txt) rm latlon.txt
set mm5_file = ../${this_cycle}_REGRIDv3.GRM
$EXECUTABLE_ARCHIVE/latlon.exe -i $mm5_file


$CSH_ARCHIVE/Forecast/RT_all.obs_trim.domain.pl all.obs latlon.txt >/dev/null

mv all.obs.trim all.obs.glob

# do reanalysis at cold-start, but do nothing on normal restart
touch all.obs

if ($cold_start == 5 || $cold_start == 17 || $cold_start == 99) mv all.obs.trim all.obs

ln -s ../${cycle_date}_REGRIDv3.${MM5HOST} REGRID
set ok = $status
if ( $ok != 0 ) then
  cp ../${cycle_date}_REGRIDv3.${MM5HOST} REGRID
endif

#	Modify namelist

set rstart_y = `echo $start_date | cut -b 1-4`
set rstart_m = `echo $start_date | cut -b 5-6`
set rstart_d = `echo $start_date | cut -b 7-8`
set rstart_h = `echo $start_date | cut -b 9-10`
set rstart_date = ${rstart_y}-${rstart_m}-${rstart_d}_${rstart_h}

set rend_y = `echo $end_date | cut -b 1-4`
set rend_m = `echo $end_date | cut -b 5-6`
set rend_d = `echo $end_date | cut -b 7-8`
set rend_h = `echo $end_date | cut -b 9-10`
set rend_date = ${rend_y}-${rend_m}-${rend_d}_${rend_h}

set time_int = 3600
if ($cold_start == 1 ) set time_int = 10800
if ($JobType == 2) then
  set time_int = 21600
endif

ed namelist.input << EOF > /dev/null
g/START_YEAR/s//$rstart_y/
g/START_MONTH/s//$rstart_m/
g/START_DAY/s//$rstart_d/
g/START_HOUR/s//$rstart_h/
g/END_YEAR/s//$rend_y/
g/END_MONTH/s//$rend_m/
g/END_DAY/s//$rend_d/
g/END_HOUR/s//$rend_h/
g/time_int/s//$time_int/
w
q
EOF

if ( -e all.obs ) then
  echo  "${SUBSYSTEM} -- all.obs being used for ANALYSIS"
else
  echo  "${SUBSYSTEM} -- all.obs IS GONE"
  exit ( 2 )
endif

#	Run the program

#( time $RAP_EXE ) >! rap_print.out
( time ${EXECUTABLE_ARCHIVE}/little_r.exe ) >! rap_print.out

#	Move the important files around

mv rap_print.out $RUNDIR/$cycle_date/${cycle_date}_rap_print.out
mv LITTLE_R_DOMAIN1 ../${cycle_date}_ANALYSIS.${MM5HOST}

if ( -e sum_out1 ) then
cp sum_out1 $RUNDIR/$cycle_date/${cycle_date}_sum_out
endif

#	Clean up

#rm input output


