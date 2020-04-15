#!/bin/csh -f
##------------------------------------------------------------------------------
## This shell decodes GPS hourly data file from UCAR/COSMIC.
##
## Francois VANDENBERGHE, September 2003
## Copyright UCAR [RAP] 1996 - 2003. All Rights Reserved.
##------------------------------------------------------------------------------


###############################################################################
echo
echo  " ----------------------------------------------------------------------"
echo  " ---------------- UCAR GPS decoder starts ------------------------------"
echo "$0 $argv[*]"
echo  " ----------------------------------------------------------------------"
###############################################################################

set debug = 0

if ( ${#argv} != 2 ) then
	echo "usage: $0 start_date obs_date"
	echo "where start_date is CCYYMMDDHH"
	echo  "d obs_date is CCYYMMDDHH"
	exit ( 4 )
endif

#set echo
set timestamp
setenv SUBSYSTEM UCARGPS_FDDA


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

##	Does the directory exist

if ($debug == 0) then
if(-d $GEAPSTMP) then
 if (! -d $GEAPSTMP/RD_UCARGPS) then
  $MustHaveDir $GEAPSTMP/RD_UCARGPS
  ln -s $GEAPSTMP/RD_UCARGPS $RUNDIR/${start_date}/RD_UCARGPS
 endif
else
 $MustHaveDir $RUNDIR/${start_date}/RD_UCARGPS
endif
endif

##	Have we already done this

if ( -e $RUNDIR/${start_date}/${obs_date}_UCARGPS_data.${MM5HOST} ) then
	echo "already decoded the UCAR GPS data set"
endif

## Testing only
if ($debug > 0) then
setenv  UCARGPS_DATA_DIR /data/input/ucar_gps
setenv  RUNDIR  /data/cycles/GEAPSO/OQC
setenv  RD_UCARGPS_EXE /data/fddahome/cycle_code/{EXECUTABLE_ARCHIVE}/rd_ucargps.exe
if (! -d $RUNDIR/${start_date}) mkdir $RUNDIR/${start_date}
if (! -d $RUNDIR/${start_date}/RD_UCARGPS) mkdir $RUNDIR/${start_date}/RD_UCARGPS
endif

##	Go to the working directory

cd $RUNDIR/${start_date}/RD_UCARGPS
echo "Now working in  $cwd"

## date and time dateb= t-60mn, datea=t+59mn

echo "${obs_date} , -1" >! input
${EXECUTABLE_ARCHIVE}/advance_cymdh < input >! output
set dateb = `cat output | cut -c 1-8`
set timeb = `cat output | cut -c 9-10`
#set timeb = ${timeb}0000

set datea = `echo ${obs_date} | cut -c 1-8`
set timea = `echo ${obs_date} | cut -c 9-10`
#set timea = ${timea}5959

set hh = `echo ${obs_date} | cut -c 9-10`

## Bring the file list of stations

cp ${CONSTANT_FILES}/ALL_HOURLY.LLH .

## Bring the most recents UCAR GPS data files

if (-f ${datea}${timea}_gpspwv.txt) rm ${datea}${timea}_gpspwv.txt
if (-f ${UCARGPS_DATA_DIR}/${dateb}${timeb}_gpspwv.txt) then
 cp    ${UCARGPS_DATA_DIR}/${dateb}${timeb}_gpspwv.txt ${datea}${timea}_gpspwv.txt
else
 echo "File  ${UCARGPS_DATA_DIR}/${dateb}${timeb}_gpspwv.txt  is missing"
endif

if (-f ${UCARGPS_DATA_DIR}/${datea}${timea}_gpspwv.txt) then
 cat   ${UCARGPS_DATA_DIR}/${datea}${timea}_gpspwv.txt >>! ${datea}${timea}_gpspwv.txt
else
 echo "File  ${UCARGPS_DATA_DIR}/${datea}${timea}_gpspwv.txt  is missing"
endif

if (! -f ${datea}${timea}_gpspwv.txt) then
 echo "Files ${datea}${timea}_gpspwv.txt and ${dateb}${timeb}_gpspwv.txt are missing, no GPS observations decoding"
 exit (1)
endif

## Run the decoder

$RD_UCARGPS_EXE -i ${datea}${timea}_gpspwv.txt -s ALL_HOURLY.LLH >&! rd_ucargps_${hh}_print.out

## Rename output file

if (! -f ${datea}${timea}_gpspwv.decoded) then
   echo "Error file ${datea}${timea}_gpspwv.decoded was not generated,"
  echo  "Check listing in $RUNDIR/${start_date}/${start_date}_ucargps_print.out.all"

else
    mv  ${datea}${timea}_gpspwv.decoded rd_ucargps_${hh}

## Put output where it is visible

if ( -e $RUNDIR/${start_date}/RD_UCARGPS/${obs_date}_UCARGPS_data.${MM5HOST} ) \
     rm $RUNDIR/${start_date}/RD_UCARGPS/${obs_date}_UCARGPS_data.${MM5HOST}
     mv rd_ucargps_${hh} $RUNDIR/${start_date}/RD_UCARGPS/UCARGPS.${obs_date}_data.${MM5HOST}
endif

endif

## Save listing

cat rd_ucargps_${hh}_print.out >>! $RUNDIR/${start_date}/${start_date}_ucargps_print.out.all

## Clean up house

if ($debug > 0) then
exit (0)
endif

rm -f ${datea}${timea}_gpspwv.txt  ${datea}${timea}_gpspwv.rip
rm -f rd_ucargps*print.out

##end

exit ( 0 )
