#!/bin/csh -f
##------------------------------------------------------------------------------
## This shell decodes the data from the Radiometrics Water Vapor Radiometer
## (*.prf files) for lamont and boulder locations.
##
## Francois VANDENBERGHE, November 2003
## vandenb@ucar.edu
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------

###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- WVR location decoder starts  ------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set debug = 0 # 0 = normal usage

if ( ${#argv} < 2 ) then
	echo "usage: $0 start_date obs_date [location]"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	echo "location is lamont ot boulder"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM WVR_FDDA


## ENVIRONMENT

set CFILE="$MM5HOME/cycle_code/CONFIG_FILES/cshrc_"

## Testing only

if ($debug > 0) then
    setenv MM5HOST GRM
endif

$CheckConfigFiles
set cfstat = $status
if ( $cfstat != 0 ) then
 echo "${SUBSYSTEM} -- Missing ConfigFile -> exiting"
 exit (2)
endif

source ${CFILE}user.mm5sys.${MM5HOST};

set start_date = $1
set obs_date   = $2

if ($#argv >= 3 ) then
    set location   = $3      # lamont or boulder
else
    set location   = lamont  # By default lamont data
endif


## Position of the WVR at Lamont, OK and time subsampling (0=no sampling)

if ($location == lamont) then
  set latACF =   "36.606"
  set lonACF =  "-97.485"
  set elvACF =   "316."
  set sampling_mn_ACF = "5"
else if ($location == boulder) then
  set latACF =   "40.039"
  set lonACF =  "-105.227"
  set elvACF =   "1618."
  set sampling_mn_ACF = "5"
else
  echo  "Unknown location $3, must be boulder of lamont."
  exit (1)
endif

## Testing only

if ($debug > 0) then
setenv  WVR_DATA_DIR /data/input/wvr_${location}
setenv  RUNDIR  /data/cycles/DARPAP/$MM5HOST
setenv  RD_WVR_EXE /data/fddahome/cycle_code/{EXECUTABLE_ARCHIVE}/rd_wvr.exe
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_WVR) mkdir $RUNDIR/${start_date}/RD_WVR
endif

##	Does the directory exist

if ($debug == 0) then
if(-d $GEAPSTMP) then
 if (! -d $GEAPSTMP/RD_WVR) then
  $MustHaveDir $GEAPSTMP/RD_WVR
  ln -s $GEAPSTMP/RD_WVR $RUNDIR/${start_date}/RD_WVR
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_WVR
endif
endif

echo  "Decoding radiometer data for $location"

echo "Working directory is $RUNDIR/${start_date}/RD_WVR"


##	Have we already done this
#if ( -e $RUNDIR/${start_date}/${obs_date}_WVR_data.${MM5HOST} ) then
#	echo "already decoded the WVR data set"
#endif

##	Go to the directory

cd $RUNDIR/${start_date}/RD_WVR
echo "Now working in  $cwd"

## Bring the most recent WVR file


set wvr_dir = ${WVR_DATA_DIR}

ls -1 ${wvr_dir}/*.prf >&! list_files_prf_${location}

set FILES = `sort -r -n list_files_prf_${location}`

if ($#FILES > 0) then
   set file = $FILES[1]
   set fileo = $file:t
   set fileo = $fileo:r
   set fileo = ${fileo}_${location}.prf
else
    echo  "No *.prf file was found at ${WVR_DATA_DIR}"
    exit (1)
endif

cp $file $fileo

## date and time dateb= t, datea=t+59mn

echo "${obs_date} , -0" >! input_${location}
${EXECUTABLE_ARCHIVE}/advance_cymdh < input_${location} >! output_${location}
set dateb = `cat output_${location} | cut -c 1-8`
set timeb = `cat output_${location} | cut -c 9-10`
set timeb = ${timeb}0000

set datea = `echo ${obs_date} | cut -c 1-8`
set timea = `echo ${obs_date} | cut -c 9-10`
set timea = ${timea}5959

set hh = `echo ${obs_date} | cut -c 9-10`

## Set-up the namelist

cat >! namelist_${location}.wvr << EOF

&record1
latitude  = ${latACF},
longitude = ${lonACF},
elevation = ${elvACF},
/

&record2
time_window_min = '${dateb}.${timeb}',
time_window_max = '${datea}.${timea}',
sampling_mn     =  ${sampling_mn_ACF},
/

&record3
print_obs_found = .TRUE.,
debug           = .FALSE.
/
EOF

## Run the decoder

$RD_WVR_EXE -i ${fileo} -n namelist_${location}.wvr -o rd_wvr_${location}_${hh} >&! rd_wvr_${location}_${hh}_print.out

#	Put output where it is visible

#if ( -e $RUNDIR/${start_date}/RD_WVR/${obs_date}_WVR_data.${MM5HOST} ) rm $RUNDIR/${start_date}/RD_WVR/${obs_date}_WVR_data.${MM5HOST}

if ( -e rd_wvr_${location}_${hh}) cat rd_wvr_${location}_${hh} >>!  $RUNDIR/${start_date}/RD_WVR/WVR.${obs_date}_data.${MM5HOST}

if ( -e rd_wvr_${location}_${hh}_print.out) cat rd_wvr_${location}_${hh}_print.out >>! $RUNDIR/${start_date}/${start_date}_wvr_print.out.all

#	Clean up house
if ($debug > 0) exit (0)

if (-f input_${location})          rm input_${location}
if (-f output_${location})         rm output_${location}
if (-f namelist_${location}.wvr)   rm namelist_${location}.wvr
if (-f list_files_prf_${location}) rm list_files_prf_${location}
if (-f rd_wvr_${location}_${hh})   rm rd_wvr_${location}_${hh}
if (-f rd_wvr_${location}_${hh}_print.out) rm rd_wvr_${location}_${hh}_print_out

exit ( 0 )
