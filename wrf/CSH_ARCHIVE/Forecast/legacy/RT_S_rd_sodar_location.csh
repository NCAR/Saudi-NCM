#!/bin/tcsh -f
##------------------------------------------------------------------------------
## This shell decodes data files from SODAR.
##
## Copyright UCAR (c) 1992 - 2004.
## University Corporation for Atmospheric Research (UCAR),
## National Center for Atmospheric Research (NCAR),
## Research Applications Program (RAP),
## P.O.Box 3000, Boulder, Colorado, 80307-3000, USA.
##
## Francois Vandenberghe, vandenb@ucar.edu, April 2004.
##------------------------------------------------------------------------------

echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- SODAR location decoder starts -----------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"

set debug = 0 # 0 = normal usage

if ( ${#argv} < 2 ) then
	echo "usage: $0 start_date obs_date [location]"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	echo "location is Pentagon"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM SODAR_FDDA


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
    set location   = $3
else
    set location   = Pentagon  # By default Pentagon, see below
endif


## Position of the SODAR near the Pentagon and time subsampling (0=no sampling)

if ($location == Pentagon) then
  set latDC =  " 38.87102"
  set lonDC =  "-77.06189"
  set elvDC =   "16."
  set sampling_mn_DC = "0"
else
  echo  "Unknown location $3, must be Pentagon."
  exit (1)
endif

## Testing only

if ($debug > 0) then
setenv  SODAR_DATA_DIR /data/input/DC/sodar
setenv  RUNDIR  /data/cycles/DARPAP/$MM5HOST
setenv  RD_SODAR_EXE /data/fddahome/cycle_code/{EXECUTABLE_ARCHIVE}/rd_sodar.exe
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_SODAR) mkdir $RUNDIR/${start_date}/RD_SODAR
endif

##	Does the directory exist

if ($debug == 0) then
if(-d $GEAPSTMP) then
 if (! -d $GEAPSTMP/RD_SODAR) then
  $MustHaveDir $GEAPSTMP/RD_SODAR
  ln -s $GEAPSTMP/RD_SODAR $RUNDIR/${start_date}/RD_SODAR
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_SODAR
endif
endif

echo  "Decoding SODAR data for $location"
echo "Working directory is $RUNDIR/${start_date}/RD_SODAR"

##	Have we already done this
#if ( -e $RUNDIR/${start_date}/${obs_date}_SODAR_data.${MM5HOST} ) then
#	echo "already decoded the SODAR data set"
#endif

##	Go to the directory

cd $RUNDIR/${start_date}/RD_SODAR
echo "Now working in  $cwd"

## Bring the most recent SODAR file


set sodar_dir = ${SODAR_DATA_DIR}

ls -1 ${sodar_dir}/*.DAT >&! list_files_DAT_${location}

set FILES = `sort -r -n list_files_DAT_${location}`

if ($#FILES > 0) then
   set file = $FILES[1]
   set fileo = $file:t
   set fileo = $fileo:r
   set fileo = ${fileo}_${location}.DAT
else
    echo  "No *.DAT file was found at ${SODAR_DATA_DIR}"
    exit (1)
endif

cp $file $fileo

## date and time dateb= t-60mn, datea=t+59mn

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

cat >! namelist_${location}.sodar << EOF

&record1
latitude  = ${latDC},
longitude = ${lonDC},
elevation = ${elvDC},
/

&record2
time_window_min = '${dateb}.${timeb}',
time_window_max = '${datea}.${timea}',
sampling_mn     =  ${sampling_mn_DC},
/

&record3
print_obs_found = .FALSE.,
debug           = .FALSE.
/
EOF

## Run the decoder

$RD_SODAR_EXE -i ${fileo} -n namelist_${location}.sodar -o rd_sodar_${location}_${hh} >&! rd_sodar_${location}_${hh}_print.out

#	Put output where it is visible

#if ( -e $RUNDIR/${start_date}/RD_SODAR/${obs_date}_SODAR_data.${MM5HOST} ) rm $RUNDIR/${start_date}/RD_SODAR/${obs_date}_SODAR_data.${MM5HOST}

if ( -e rd_sodar_${location}_${hh}) cat rd_sodar_${location}_${hh} >>!  $RUNDIR/${start_date}/RD_SODAR/SODAR.${obs_date}_data.${MM5HOST}

if ( -e rd_sodar_${location}_${hh}_print.out) cat rd_sodar_${location}_${hh}_print.out >>! $RUNDIR/${start_date}/${start_date}_sodar.out.all

#	Clean up house
if ($debug > 0) exit (0)

if (-f input_${location})          rm input_${location}
if (-f output_${location})         rm output_${location}
if (-f namelist_${location}.sodar) rm namelist_${location}.sodar
if (-f list_files_DAT_${location}) rm list_files_DAT_${location}
if (-f rd_sodar_${location}_${hh}) rm rd_sodar_${location}_${hh}
if (-f rd_sodar_${location}_${hh}_print.out) rm rd_sodar_${location}_${hh}_print_out

exit ( 0 )
